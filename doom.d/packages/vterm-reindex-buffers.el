;;; vterm-reindex-buffers.el --- Reindex vterm tabs by current visual order -*- lexical-binding: t; -*-

(require 'vterm)
(require 'persp-mode nil t)
(require 'centaur-tabs nil t)   ;; loads centaur-tabs if available
(require 'tab-line nil t)       ;; built-in since Emacs 27; safe if not present

(defun lg/vterm--workspace-name ()
  (if (bound-and-true-p persp-mode)
      (safe-persp-name (get-current-persp))
    "main"))

(defun lg/vterm--base-name (&optional ws-name)
  (format "*v:%s" (or ws-name (lg/vterm--workspace-name))))

(defun lg/vterm--is-vterm-in-workspace-p (buf ws-name)
  (and (buffer-live-p buf)
       (with-current-buffer buf (eq major-mode 'vterm-mode))
       (let* ((name (buffer-name buf))
              (re (format "\\`\\*v:%s\\(?:<\\([0-9]+\\)>\\)?\\'" (regexp-quote ws-name))))
         (string-match re name))))

(defun lg/vterm--current-is-workspace-vterm-p ()
  "Non-nil if current buffer is a vterm in the current workspace."
  (lg/vterm--is-vterm-in-workspace-p (current-buffer) (lg/vterm--workspace-name)))

(defun lg/centaur-or-tabline--buffers-in-visual-order ()
  "Return buffers in the current tab group in visible left-to-right order.
Tries centaur-tabs, then tab-line, then falls back to buffer list."
  ;; 1) centaur-tabs path (some versions expose helpers)
  (cond
   ;; (A) centaur-tabs current tabset → list of buffers
   ((and (fboundp 'centaur-tabs-current-tabset)
         (let ((ts (centaur-tabs-current-tabset)))
           (and ts (fboundp 'centaur-tabs-tabs) (fboundp 'centaur-tabs-tab-value)
                (cl-loop for tab in (funcall 'centaur-tabs-tabs ts)
                         collect (funcall 'centaur-tabs-tab-value tab)))))
    ;; re-evaluate to actually return the list
    (let* ((ts (centaur-tabs-current-tabset))
           (tabs (funcall 'centaur-tabs-tabs ts)))
      (mapcar (lambda (tab) (funcall 'centaur-tabs-tab-value tab)) tabs)))

   ;; (B) some builds export centaur-tabs’ public buffer list for the active group
   ((fboundp 'centaur-tabs-get-buffer-list)
    (centaur-tabs-get-buffer-list))

   ;; 2) tab-line path (built-in) — use its tab function if present
   ((and (boundp 'tab-line-tabs-function)
         (functionp tab-line-tabs-function))
    (let ((tabs (funcall tab-line-tabs-function)))
      ;; `tab-line-tabs-function` usually returns a list of buffers already.
      ;; If an implementation returns tab objects, coerce to buffers.
      (mapcar (lambda (x) (if (bufferp x) x (car-safe x))) tabs)))

   ;; 3) fallback – your centaur tabs buffer list function (may be sorted)
   ((and (boundp 'centaur-tabs-buffer-list-function)
         (functionp centaur-tabs-buffer-list-function))
    (prog1
        (funcall centaur-tabs-buffer-list-function)
      (message "[vterm-reindex] Warning: falling back to buffer-list function; order may be sorted by your config.")))

   ;; ultimate fallback – all buffers in window’s buffer list
   (t
    (message "[vterm-reindex] Warning: could not read visual order from centaur-tabs/tab-line; using window buffer list.")
    (buffer-list))))

(defun lg/vterm--refresh-centaur-tabs ()
  "Force centaur-tabs to rebuild tabsets and redraw the tab bar.
After buffer renames the tab objects still point at the same buffers, so
the cached rendered template would show stale names until something else
triggers a rebuild.  We have to (1) rebuild tabsets, (2) invalidate the
template cache on tabsets that contain vterm tabs, and (3) force a
display update.  See `centaur-tabs-buffer-update-groups` /
`centaur-tabs-set-template` / `centaur-tabs-display-update`."
  (when (fboundp 'centaur-tabs-buffer-update-groups)
    (centaur-tabs-buffer-update-groups))
  (when (and (boundp 'centaur-tabs-tabsets)
             (fboundp 'centaur-tabs-set-template)
             (fboundp 'lg/vterm-index))
    (mapatoms
     (lambda (tabset)
       (when (boundp tabset)
         (let ((tabs (symbol-value tabset)))
           (when (and tabs
                      (cl-some (lambda (tab) (lg/vterm-index (car tab))) tabs))
             (centaur-tabs-set-template tabset nil)))))
     centaur-tabs-tabsets))
  (when (fboundp 'centaur-tabs-display-update)
    (centaur-tabs-display-update)))

(defun lg/vterm--rename-to-canonical (ordered-bufs ws)
  "Two-phase rename ORDERED-BUFS to *v:WS<1>, *v:WS<2>, ... in order.
Returns the list of (buffer . new-name) pairs that were actually renamed
(i.e. excluding buffers already at their target name). Refreshes the tab UI
when any rename occurred."
  (let* ((base (lg/vterm--base-name ws))
         (targets (cl-loop for buf in ordered-bufs
                           for i from 1
                           collect (cons buf (format "%s<%d>" base i))))
         (changes (cl-remove-if (lambda (bp)
                                  (string= (buffer-name (car bp)) (cdr bp)))
                                targets)))
    (when changes
      ;; two-phase rename to avoid collisions
      (dolist (bp changes)
        (with-current-buffer (car bp)
          (rename-buffer
           (generate-new-buffer-name (concat (buffer-name) " @pending")) t)))
      (dolist (bp targets)
        (with-current-buffer (car bp)
          (rename-buffer (cdr bp) t)))
      (lg/vterm--refresh-centaur-tabs)
      (when (boundp 'tab-line-mode)
        (force-mode-line-update t)))
    changes))

;;;###autoload
(defun lg/vterm-reindex-buffers (&optional dryrun)
  "Rename vterm buffers in this workspace to match the current tab *visual* order.
C-u for DRYRUN preview."
  (interactive "P")
  (let* ((ws (lg/vterm--workspace-name))
         (base (lg/vterm--base-name ws))
         (visual (lg/centaur-or-tabline--buffers-in-visual-order))
         (ordered (seq-filter (lambda (b) (lg/vterm--is-vterm-in-workspace-p b ws)) visual)))
    (unless ordered
      (user-error "No vterm buffers for workspace '%s' are visible to reindex" ws))
    (let* ((targets (cl-loop for buf in ordered
                             for i from 1
                             collect (cons buf (format "%s<%d>" base i))))
           (changes (cl-remove-if (lambda (bp) (string= (buffer-name (car bp)) (cdr bp))) targets)))
      (if dryrun
          (if (null changes)
              (message "[vterm-reindex] Already in order for workspace '%s'." ws)
            (message "[vterm-reindex] Would rename:\n%s"
                     (mapconcat (lambda (bp)
                                  (format "  %s -> %s" (buffer-name (car bp)) (cdr bp)))
                                changes "\n")))
        ;; two-phase rename to avoid collisions
        (dolist (bp changes)
          (with-current-buffer (car bp)
            (rename-buffer (generate-new-buffer-name (concat (buffer-name) " @pending")) t)))
        (dolist (bp targets)
          (with-current-buffer (car bp)
            (rename-buffer (cdr bp) t)))
        (when (fboundp 'centaur-tabs-headline-match)
          (centaur-tabs-headline-match))
        (message "[vterm-reindex] Renamed %d buffer%s for workspace '%s'."
                 (length changes) (if (= (length changes) 1) "" "s") ws)))))

;;;###autoload
(defun lg/vterm-resort-buffers-by-name (&optional dryrun)
  "Rename vterm buffers in this workspace so their names are in canonical order:
  *v:WS<1>*, *v:WS<2>*, *v:WS<3>*, ...

This is a one-shot command (no auto sorting). With C-u, do a DRYRUN preview."
  (interactive "P")
  (let* ((ws (lg/vterm--workspace-name))
         (base (lg/vterm--base-name ws))
         ;; Collect *all* vterm buffers in this workspace (not visual order).
         (bufs (seq-filter (lambda (b) (lg/vterm--is-vterm-in-workspace-p b ws))
                           (buffer-list))))
    (unless bufs
      (user-error "No vterm buffers for workspace '%s' to sort" ws))

    ;; Sort by the numeric suffix <N> if present; base name (no suffix) is treated as 0.
    (cl-labels
        ((idx-of (b)
           (let* ((name (buffer-name b))
                  (re (format "\\`\\*v:%s\\(?:<\\([0-9]+\\)>\\)?\\'"
                              (regexp-quote ws))))
             (when (string-match re name)
               (if (match-string 1 name)
                   (string-to-number (match-string 1 name))
                 0))))
         (name< (a b)
           (let ((ia (idx-of a))
                 (ib (idx-of b)))
             (cond
              ((and ia ib (/= ia ib)) (< ia ib))
              ;; fallback: stable alphabetical if something unexpected happens
              (t (string-lessp (buffer-name a) (buffer-name b)))))))

      (setq bufs (sort (copy-sequence bufs) #'name<)))

    (if dryrun
        (let* ((targets (cl-loop for buf in bufs
                                 for i from 1
                                 collect (cons buf (format "%s<%d>" base i))))
               (changes (cl-remove-if
                         (lambda (bp) (string= (buffer-name (car bp)) (cdr bp)))
                         targets)))
          (if (null changes)
              (message "[vterm-sort] Already canonical for workspace '%s'." ws)
            (message "[vterm-sort] Would rename:\n%s"
                     (mapconcat (lambda (bp)
                                  (format "  %s -> %s" (buffer-name (car bp)) (cdr bp)))
                                changes "\n"))))
      (let ((changes (lg/vterm--rename-to-canonical bufs ws)))
        (message "[vterm-sort] Canonicalized %d buffer%s for workspace '%s'."
                 (length changes) (if (= (length changes) 1) "" "s") ws)))))

(defun lg/vterm--workspace-vterms-by-index (ws)
  "Return all vterm buffers in workspace WS, sorted by current numeric index."
  (sort (seq-filter (lambda (b) (lg/vterm--is-vterm-in-workspace-p b ws))
                    (buffer-list))
        (lambda (a b) (< (or (lg/vterm-index a) 0)
                         (or (lg/vterm-index b) 0)))))

(defun lg/vterm--swap-and-renumber (direction)
  "Swap current vterm with its neighbor in DIRECTION (`left' or `right'),
then renumber all workspace vterms to canonical *v:WS<1..N>* order.
No-op at the corresponding edge."
  (let* ((cur (current-buffer))
         (ws (lg/vterm--workspace-name))
         (bufs (lg/vterm--workspace-vterms-by-index ws))
         (pos (cl-position cur bufs))
         (new-pos (and pos (if (eq direction 'right) (1+ pos) (1- pos)))))
    (when (and pos new-pos (>= new-pos 0) (< new-pos (length bufs)))
      (let* ((vec (vconcat bufs))
             (tmp (aref vec pos)))
        (aset vec pos (aref vec new-pos))
        (aset vec new-pos tmp)
        (lg/vterm--rename-to-canonical (append vec nil) ws)))))

;;;###autoload
(defun lg/vterm-move-tab-right ()
  "Move current tab right. For workspace vterm buffers, swap with the
right-hand neighbor and rename so the new order is canonical. Falls
through to `centaur-tabs-move-current-tab-to-right' for non-vterm."
  (interactive)
  (if (lg/vterm--current-is-workspace-vterm-p)
      (lg/vterm--swap-and-renumber 'right)
    (centaur-tabs-move-current-tab-to-right)))

;;;###autoload
(defun lg/vterm-move-tab-left ()
  "Move current tab left. For workspace vterm buffers, swap with the
left-hand neighbor and rename so the new order is canonical. Falls
through to `centaur-tabs-move-current-tab-to-left' for non-vterm."
  (interactive)
  (if (lg/vterm--current-is-workspace-vterm-p)
      (lg/vterm--swap-and-renumber 'left)
    (centaur-tabs-move-current-tab-to-left)))

(defun lg/vterm--reindex-on-kill ()
  "When a workspace vterm is killed, renumber remaining workspace vterms
to canonical 1..N order so deletes don't leave low-index gaps."
  (when (lg/vterm--current-is-workspace-vterm-p)
    (let ((ws (lg/vterm--workspace-name))
          (dying (current-buffer)))
      (run-at-time
       0 nil
       (lambda ()
         (let ((bufs (sort (seq-filter
                            (lambda (b)
                              (and (not (eq b dying))
                                   (lg/vterm--is-vterm-in-workspace-p b ws)))
                            (buffer-list))
                           (lambda (a b)
                             (< (or (lg/vterm-index a) 0)
                                (or (lg/vterm-index b) 0))))))
           (when bufs
             (lg/vterm--rename-to-canonical bufs ws))))))))

(add-hook 'kill-buffer-hook #'lg/vterm--reindex-on-kill)
