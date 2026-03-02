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
  :init
  ;; Parse *v:NAME and *v:NAME<N>; base (no <N>) is index 0
  (defun lg/vterm-index (buf)
    "Return numeric index for vterm buffers:
   *v:workspace  -> 0
   *v:workspace<1> -> 1
   Non-vterm buffers -> nil (don’t sort)."
    (with-current-buffer buf
      (when (eq major-mode 'vterm-mode)
        (let ((name (buffer-name buf)))
          (when (string-match "\\`\\*v:[^<]+\\(?:<\\([0-9]+\\)>\\)?\\'" name)
            (let ((m (match-string 1 name)))
              (if m (string-to-number m) 0)))))))


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
  (defun lg/centaur-tabs-buffer-groups ()
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
  (setq centaur-tabs-buffer-groups-function 'lg/centaur-tabs-buffer-groups)

  ;; ;; Make centaur-tabs use the same buffer list as persp mode
  ;; (defun lg/centaur-tabs-buffer-list ()
  ;;   "Return buffers visible in the current workspace."
  ;;   (let ((buffers (persp-buffer-list)))
  ;;     (cl-remove-if
  ;;      (lambda (buffer)
  ;;        (or (string-prefix-p " " (buffer-name buffer)) ; internal buffers
  ;;            ;; (string-prefix-p "*" (buffer-name buffer)) ; hidden buffers
  ;;            (not (buffer-live-p buffer))))
  ;;      buffers)))
  ;; (setq centaur-tabs-buffer-list-function #'lg/centaur-tabs-buffer-list)
  (defun lg/centaur-tabs-buffer-list ()
    "Buffers visible in current workspace, with *v:workspace<N> sorted by N."
    (let* ((raw (persp-buffer-list))
           (buffers (cl-remove-if
                     (lambda (b)
                       (or (string-prefix-p " " (buffer-name b))
                           (not (buffer-live-p b))))
                     raw)))
      ;; stable-sort: vterm by numeric index; others keep existing order
      (cl-stable-sort
       buffers
       (lambda (a b)
         (let ((ia (lg/vterm-index a))
               (ib (lg/vterm-index b)))
           (and ia ib (< ia ib)))))))

  (setq centaur-tabs-buffer-list-function #'lg/centaur-tabs-buffer-list)

  (add-hook 'persp-switch-hook #'centaur-tabs-headline-match)
  ;; Update tabs on switching perspectives (workspaces)
  )
