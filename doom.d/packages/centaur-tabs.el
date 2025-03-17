;;; ../.dotfiles/doom.d/packages/centaur-tabs.el -*- lexical-binding: t; -*-

;; Auto-dim other buffers
(use-package! auto-dim-other-buffers
  :init
  (auto-dim-other-buffers-mode))

(use-package! centaur-tabs
  ;;:defer 5

  :bind (:map evil-normal-state-map
              ("M-[" . centaur-tabs-backward)
              ("M-]" . centaur-tabs-forward)
              ("M-}" . centaur-tabs-move-current-tab-to-right)
              ("M-{" . centaur-tabs-move-current-tab-to-left)
              )

  ;; :hook
  ;; ;; Disalbe tabs in vterm
  ;; ((vterm-mode vterm-toggle--mode) . centaur-tabs-local-mode)

  :config
  (centaur-tabs-mode t)
  (setq centaur-tabs-adjust-buffer-order nil) ;;Dont Auto-reorder tabs when switching
  (setq centaur-tabs-height 32
        centaur-tabs-style "bar"
        centaur-tabs-set-icons t
        centaur-tabs-set-modified-marker t
        centaur-tabs-set-bar 'left  ;;'under doesn't work
        centaur-tabs-set-close-button nil
        centaur-tabs-show-navigation-buttons nil
        )

  ;; The default seems to do a good job
  (centaur-tabs-group-buffer-groups) ;; Default

  ;; CUSTOM TABS GROUPING
  ;; Based off default centaur-tabs-buffer-group
  (defun my/centaur-tabs-buffer-groups ()
    "`centaur-tabs-buffer-groups' control buffers' group rules.

        Group centaur-tabs with mode if buffer is derived from `eshell-mode'
        `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
        All buffer name start with * will group to \"Emacs\".
        Other buffer group by `centaur-tabs-get-group-name' with project name."
    (list
     (cond
      ;; GROUP VTERM BY PROJECT
      ((eq major-mode 'vterm-mode)
       (let ((project-name (projectile-project-name)))
         (if (not (string= "-" project-name))
             ;; Group by project name for vterm buffers
             (concat "vterm: " project-name)
           ;; Fallback group name for vterm buffers not in a project
           "vterm")))
      ((or (string-equal "*" (substring (buffer-name) 0 1))
	   (memq major-mode '(magit-process-mode
			      magit-status-mode
			      magit-diff-mode
			      magit-log-mode
			      magit-file-mode
			      magit-blob-mode
			      magit-blame-mode
			      )))
       "Emacs")
      ((derived-mode-p 'eshell-mode)
       "EShell")
      ((derived-mode-p 'emacs-lisp-mode)
       "Elisp")
      ((derived-mode-p 'dired-mode)
       "Dired")
      ((memq major-mode '(org-mode org-agenda-mode diary-mode))
       "OrgMode")
      (t
       (centaur-tabs-get-group-name (current-buffer))))))

  ;; Set the custom grouping function
  (setq centaur-tabs-buffer-groups-function 'my/centaur-tabs-buffer-groups)
  )
