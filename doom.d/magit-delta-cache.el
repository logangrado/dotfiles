;;; magit-delta-cache.el --- Reuse unchanged Delta output -*- lexical-binding: t; -*-

(require 'cl-lib)

(defvar lg/magit-delta-cache-max-entries 512
  "Maximum number of rendered snapshots and HEAD sources retained.
A file can have multiple snapshots.  This separately bounds per-entry
overhead and FIFO bookkeeping even when individual payloads are tiny.")
(defvar lg/magit-delta-cache-max-bytes (* 128 1024 1024)
  "Maximum retained payload bytes, excluding Lisp object overhead.
Large reviews can exceed the former 16 MiB budget and thrash on refresh.")
(defvar lg/magit-delta--cache (make-hash-table :test #'equal))
(defvar lg/magit-delta--cache-order nil)
(defvar lg/magit-delta--cache-bytes 0)

(defun lg/magit-delta-clear-cache ()
  "Discard cached Delta output and HEAD source contents.
Run after replacing the Delta executable in place."
  (interactive)
  (clrhash lg/magit-delta--cache)
  (setq lg/magit-delta--cache-order nil
        lg/magit-delta--cache-bytes 0))

(defun lg/magit-delta--cache-get (key)
  "Return the (BYTES . VALUE) entry for KEY, or nil."
  (gethash key lg/magit-delta--cache))

(defun lg/magit-delta--cache-put (key value bytes)
  "Retain VALUE under KEY with payload size BYTES, using bounded FIFO eviction."
  (when (and (> lg/magit-delta-cache-max-entries 0)
             (<= bytes lg/magit-delta-cache-max-bytes))
    (when-let ((old (gethash key lg/magit-delta--cache)))
      (cl-decf lg/magit-delta--cache-bytes (car old))
      (setq lg/magit-delta--cache-order
            (delete key lg/magit-delta--cache-order))
      (remhash key lg/magit-delta--cache))
    (while (and lg/magit-delta--cache-order
                (or (>= (hash-table-count lg/magit-delta--cache)
                        lg/magit-delta-cache-max-entries)
                    (> (+ lg/magit-delta--cache-bytes bytes)
                       lg/magit-delta-cache-max-bytes)))
      (let* ((old-key (car (last lg/magit-delta--cache-order)))
             (old (gethash old-key lg/magit-delta--cache)))
        (cl-decf lg/magit-delta--cache-bytes (car old))
        (remhash old-key lg/magit-delta--cache)
        (setq lg/magit-delta--cache-order
              (butlast lg/magit-delta--cache-order))))
    (puthash key (cons bytes value) lg/magit-delta--cache)
    (push key lg/magit-delta--cache-order)
    (cl-incf lg/magit-delta--cache-bytes bytes))
  value)

(defun lg/magit-delta--file-parts (text)
  "Split TEXT into complete git file diffs, retaining ANSI codes.
Return nil for an unfamiliar preamble or format.  Delta's color-only
output must preserve the original file headers for caching to be safe."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (let (starts)
      (while (not (eobp))
        (let ((line (replace-regexp-in-string
                     "\e\\[[0-9;]*m" ""
                     (buffer-substring-no-properties
                      (line-beginning-position) (line-end-position)))))
          (when (string-prefix-p "diff --git " line)
            (push (point) starts)))
        (forward-line 1))
      (setq starts (nreverse starts))
      (when (eql (car starts) (point-min))
        (cl-loop for tail on starts
                 collect (buffer-substring-no-properties
                          (car tail) (or (cadr tail) (point-max))))))))

(defun lg/magit-delta--file-header (part)
  "Return PART's first line without ANSI SGR codes."
  (replace-regexp-in-string
   "\e\\[[0-9;]*m" ""
   (substring part 0 (string-match "\n" part))))

(defun lg/magit-delta--run (input executable args)
  "Return ANSI output from one Delta invocation on INPUT.
Never retain failed or partial output."
  (with-temp-buffer
    (insert input)
    (let ((status (apply #'call-process-region
                         (point-min) (point-max) executable t t nil args)))
      (unless (eq status 0)
        (error "Delta failed (%s); output was not cached" status)))
    (buffer-string)))

(cl-defstruct (lg/magit-delta--snapshot
               (:constructor lg/magit-delta--snapshot-create))
  "Propertized file TEXT.  Legacy SPANS are kept for old cache compatibility.
New snapshots leave SPANS empty and rendering never restores them."
  text spans)

(defun lg/magit-delta--text-payload-bytes (text)
  "Count TEXT bytes and serialized text-property runs, excluding Lisp overhead."
  (+ (string-bytes text)
     (let ((pos 0) (bytes 0))
       (while (< pos (length text))
         (when-let ((props (text-properties-at pos text)))
           (cl-incf bytes (+ 16 (string-bytes (prin1-to-string props)))))
         (setq pos (next-property-change pos text (length text))))
       bytes)))

(defun lg/magit-delta--payload-bytes (value)
  "Count retained text, properties and any legacy span payload still cached.
Lisp container overhead is additional to the configured payload limit."
  (if (stringp value)
      (lg/magit-delta--text-payload-bytes value)
    (+ (lg/magit-delta--text-payload-bytes (lg/magit-delta--snapshot-text value))
       (cl-loop for span in (lg/magit-delta--snapshot-spans value)
                sum (+ 16 (string-bytes (prin1-to-string (aref span 2))))))))

(defun lg/magit-delta--render-files (input executable args decode signature
                                         &optional parts identities)
  "Return decoded file outputs, reusing unchanged files and batching misses.
DECODE receives ANSI output, its augmented input, and the original file
index (nil for a full-render fallback).  SIGNATURE identifies the decoder
and its settings.  Optional PARTS avoids re-splitting INPUT; IDENTITIES
adds per-file stripping metadata to cache identity.

Only cache with --no-gitconfig: external Git configuration otherwise makes
ARGS insufficient to identify the rendering.  No live position-dependent
objects may be returned by DECODE."
  (let ((parts (and (member "--no-gitconfig" args)
                    (or parts (lg/magit-delta--file-parts input)))))
    (if (not parts)
        (list (funcall decode (lg/magit-delta--run input executable args) input nil))
      (let* ((settings
              (secure-hash
               'sha256
               (prin1-to-string
                (list signature default-directory
                      (or (executable-find executable) executable)
                      args process-environment))))
             (keys (cl-loop for part in parts for index from 0
                            collect (list 'render settings
                                          (secure-hash 'sha256 part)
                                          (and identities
                                               (secure-hash
                                                'sha256
                                                (prin1-to-string
                                                 (nth index identities)))))))
             (entries (mapcar #'lg/magit-delta--cache-get keys))
             (missing (cl-loop for part in parts for entry in entries
                               unless entry collect part)))
        (if (not missing)
            (mapcar #'cdr entries)
          (let* ((output (lg/magit-delta--run
                          (mapconcat #'identity missing "") executable args))
                 (colored (lg/magit-delta--file-parts output)))
            (if (not (equal (mapcar #'lg/magit-delta--file-header missing)
                            (mapcar #'lg/magit-delta--file-header colored)))
                ;; Never join uncertain file boundaries with cached output.
                (list (funcall decode
                               (if (cl-every #'null entries)
                                   output
                                 (lg/magit-delta--run input executable args))
                               input nil))
              (cl-loop for key in keys for entry in entries
                       for part in parts for index from 0
                       collect
                       (if entry
                           (cdr entry)
                         (let ((value (funcall decode (pop colored) part index)))
                           (lg/magit-delta--cache-put
                            key value (lg/magit-delta--payload-bytes value))))))))))))

(defun lg/magit-delta--render-cached (input executable args)
  "Return cached ANSI output for INPUT.
The normal Magit wrapper uses parsed snapshots instead; this entry point
also permits direct ANSI-output equivalence tests."
  (mapconcat #'identity
             (lg/magit-delta--render-files
              input executable args (lambda (ansi _input _index) ansi) 'ansi-v1)
             ""))

(provide 'magit-delta-cache)
;;; magit-delta-cache.el ends here
