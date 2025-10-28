;;; ../.dotfiles/doom.d/custom_funcs.el -*- lexical-binding: t; -*-

(defun uuidgen-4 ()
  "Return a lowercase RFC4122 UUIDv4 string by calling `uuidgen` on macOS.
Retries a few times until the version nibble is '4' (defensive)."
  (let* ((bin (or (executable-find "uuidgen") "/usr/bin/uuidgen"))
         (tries 0)
         uuid hex)
    (unless (and bin (file-executable-p bin))
      (user-error "uuidgen not found"))
    (while (and (< tries 8)
                (progn
                  (setq uuid (downcase (car (process-lines bin))))
                  (setq hex (replace-regexp-in-string "-" "" uuid))
                  (not (and (= (length hex) 32) (eq (aref hex 12) ?4))))) ; v4 nibble
      (setq tries (1+ tries)))
    uuid))

(defun uuid4-insert (&optional n)
  "Insert a UUIDv4 at point using macOS `uuidgen`.
With prefix arg N, insert N UUIDs (one per line). The last UUID is pushed to the kill ring."
  (interactive "P")
  (unless (eq system-type 'darwin)
    (user-error "This command expects macOS (Darwin)"))
  (let* ((count (prefix-numeric-value (or n 1)))
         (col (current-column))
         last)
    (dotimes (i count)
      (setq last (uuidgen-4))
      (insert last)
      (when (< i (1- count))
        (newline)
        (move-to-column col)))
    (kill-new last)
    (message "Inserted UUIDv4: %s" last)))

;; Create a commented underline under the current line.
;; Prompts for the underline character and matches the line's indent & length.
(defun comment-underline (char)
  "Insert a commented underline under the current line using CHAR.
The underline starts at the same indent as the current line and the total
visual width (indent + comment delimiters + underline + comment-end)
matches the current line."
  (interactive
   (let* ((s (read-string "Underline character (default -): ")))
     (list (string-to-char (if (string-empty-p s) "-" s)))))
  (save-excursion
    (let* ((bol (line-beginning-position))
           (eol (line-end-position))
           (indent-str (buffer-substring-no-properties
                        bol (save-excursion (back-to-indentation) (point))))
           (indent-cols (save-excursion (back-to-indentation) (current-column)))
           (line-cols   (save-excursion (goto-char eol) (current-column))))
      (goto-char eol)
      (open-line 1)
      (forward-line 1)
      ;; Avoid doubled # by ignoring comment-add and padding manually
      (let* ((comment-add 0)
             (_ (comment-normalize-vars))
             (cs-raw (if (and (stringp comment-start)
                              (> (length comment-start) 0))
                         comment-start
                       "#"))
             (ce-raw (if (and (stringp comment-end)
                              (> (length comment-end) 0))
                         comment-end
                       ""))
             ;; force exactly one space after start if none is present
             (cs (if (string-suffix-p " " cs-raw) cs-raw (concat cs-raw " ")))
             (ce ce-raw)
             (csw (string-width cs))
             (cew (string-width ce))
             (underline-cols (max 0 (- line-cols indent-cols csw cew)))
             (underline (make-string underline-cols char)))
        (message cs)
        (message underline)
        (message ce)
        (insert indent-str cs underline ce)))))


;; WINDOW RESIZING
(defun resize-window-to-1/3 ()
  "Resize the current window to 1/3 of the frame width."
  (interactive)
  (resize-current-window-to-fraction 0.35))
(defun resize-window-to-1/2 ()
  "Resize the current window to 1/2 of the frame width."
  (interactive)
  (resize-current-window-to-fraction 0.5))
(defun resize-window-to-2/3 ()
  "Resize the current window to 2/3 of the frame width."
  (interactive)
  (resize-current-window-to-fraction 0.65))
(defun resize-window-to-9/10 ()
  "Resize the current window to 9/10 of the frame width."
  (interactive)
  (resize-current-window-to-fraction 0.9))

(defun file-notify-rm-all-watches ()
  "Remove all existing file notification watches from Emacs. For recovering from too many open pipes"
  (interactive)
  (maphash
   (lambda (key _value)
     (file-notify-rm-watch key))
   file-notify-descriptors))

;; Resize the whole frame, and not only a window
;; Adapted from https://stackoverflow.com/a/24714383/5103881
(defun acg/zoom-frame (&optional amt frame)
  "Increaze FRAME font size by amount AMT. Defaults to selected
frame if FRAME is nil, and to 1 if AMT is nil."
  (interactive "p")
  (let* ((frame (or frame (selected-frame)))
         (font (face-attribute 'default :font frame))
         (size (font-get font :size))
         (amt (or amt 1))
         (new-size (+ size amt)))
    (set-frame-font (font-spec :size new-size) t `(,frame))
    ;; Also set fixed-pitch height to match default
    (set-face-attribute 'fixed-pitch frame :height (face-attribute 'default :height frame))
    (message "Frame's font new size: %d" new-size)))

(defun acg/zoom-frame-out (&optional amt frame)
  "Call `acg/zoom-frame' with negative argument."
  (interactive "p")
  (acg/zoom-frame (- (or amt 1)) frame))

(defun resize-current-window-to-fraction (fraction)
  "Resize the current window to a specific FRACTION of the frame's total height."
  (interactive "Resize to fraction of frame height: ")
  (let* ((total-lines (frame-total-lines))
         (target-height (ceiling (* fraction total-lines)))
         (current-height (window-height))
         (delta (- target-height current-height)))
    (window-resize nil delta)))
