;;; ../.dotfiles/doom.d/packages/evil-modeline.el -*- lexical-binding: t; -*-

;; Tint the active modeline background based on evil state, per-buffer.
;; Only `mode-line' is remapped; `mode-line-inactive' is left alone so
;; unfocused windows stay neutral and the tint always marks the active
;; buffer's state. Colors are recomputed on every state change, so theme
;; switching just works.

(defvar-local lg/evil-modeline--cookies nil
  "List of face-remap cookies for the current buffer's tint.")

(defun lg/evil-modeline--color-for-state (state)
  "Return a muted background color for evil STATE, or nil for no tint."
  (pcase state
    ('normal  (doom-blend (doom-color 'green)   (doom-color 'bg) 0.28))
    ('insert  (doom-blend (doom-color 'cyan)    (doom-color 'bg) 0.28))
    ('visual  (doom-blend (doom-color 'magenta) (doom-color 'bg) 0.30))
    ('replace (doom-blend (doom-color 'red)     (doom-color 'bg) 0.32))
    ('emacs   (doom-blend (doom-color 'orange)  (doom-color 'bg) 0.28))
    (_ nil)))

(defun lg/evil-modeline-apply ()
  "Apply (or refresh) the modeline tint for the current buffer."
  (unless (minibufferp)
    (dolist (cookie lg/evil-modeline--cookies)
      (face-remap-remove-relative cookie))
    (setq lg/evil-modeline--cookies nil)
    (when-let* ((bg (lg/evil-modeline--color-for-state evil-state)))
      ;; `header-line' inherits from `mode-line' by default, so our
      ;; remap leaks into lsp-headerline-breadcrumb, magit-log's header,
      ;; etc. Pin header-line's background to the theme's original
      ;; mode-line bg to keep its look stable while we tint mode-line.
      (let ((header-bg (face-attribute 'mode-line :background nil t)))
        (setq lg/evil-modeline--cookies
              (list
               (face-remap-add-relative 'mode-line :background bg)
               (face-remap-add-relative 'header-line :background header-bg)))))))

(defun lg/evil-modeline-init-all-buffers ()
  "Apply tint to every existing buffer (covers buffers alive at startup)."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (bound-and-true-p evil-local-mode)
        (lg/evil-modeline-apply)))))

(after! doom-modeline
  (add-hook 'evil-normal-state-entry-hook  #'lg/evil-modeline-apply)
  (add-hook 'evil-insert-state-entry-hook  #'lg/evil-modeline-apply)
  (add-hook 'evil-visual-state-entry-hook  #'lg/evil-modeline-apply)
  (add-hook 'evil-replace-state-entry-hook #'lg/evil-modeline-apply)
  (add-hook 'evil-emacs-state-entry-hook   #'lg/evil-modeline-apply)
  (add-hook 'evil-mode-hook                #'lg/evil-modeline-init-all-buffers)
  (lg/evil-modeline-init-all-buffers))
