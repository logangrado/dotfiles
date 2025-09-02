;;; vterm-reindex-buffers.el --- Reindex vterm tabs by current visual order -*- lexical-binding: t; -*-

(require 'vterm)
(require 'persp-mode nil t)
(require 'centaur-tabs nil t)   ;; loads centaur-tabs if available
(require 'tab-line nil t)       ;; built-in since Emacs 27; safe if not present

(defun my/vterm--workspace-name ()
  (if (bound-and-true-p persp-mode)
      (safe-persp-name (get-current-persp))
    "main"))

(defun my/vterm--base-name (&optional ws-name)
  (format "*v:%s" (or ws-name (my/vterm--workspace-name))))

(defun my/vterm--is-vterm-in-workspace-p (buf ws-name)
  (and (buffer-live-p buf)
       (with-current-buffer buf (eq major-mode 'vterm-mode))
       (let* ((name (buffer-name buf))
              (re (format "\\`\\*v:%s\\(?:<\\([0-9]+\\)>\\)?\\'" (regexp-quote ws-name))))
         (string-match re name))))

(defun my/centaur-or-tabline--buffers-in-visual-order ()
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

;;;###autoload
(defun my/vterm-reindex-buffers (&optional dryrun)
  "Rename vterm buffers in this workspace to match the current tab *visual* order.
C-u for DRYRUN preview."
  (interactive "P")
  (let* ((ws (my/vterm--workspace-name))
         (base (my/vterm--base-name ws))
         (visual (my/centaur-or-tabline--buffers-in-visual-order))
         (ordered (seq-filter (lambda (b) (my/vterm--is-vterm-in-workspace-p b ws)) visual)))
    (unless ordered
      (user-error "No vterm buffers for workspace '%s' are visible to reindex" ws))
    (let* ((targets (cl-loop for buf in ordered
                             for i from 0
                             collect (cons buf (if (= i 0) base (format "%s<%d>" base i)))))
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
