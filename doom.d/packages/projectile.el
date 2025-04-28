;;; ../.dotfiles/doom.d/packages/projectile.el -*- lexical-binding: t; -*-

(use-package! projectile
  :init
  (defun my/projectile-add-project-with-custom-name (dir)
    "Prompt for a custom name when adding a project to Projectile.
If no name is provided, default to the directory name."
    (interactive (list (read-directory-name "Add project: ")))
    (let* ((default-name (file-name-nondirectory (directory-file-name dir)))
           (custom-name (read-string (format "Project name (%s): " default-name) nil nil default-name)))
      (setq projectile-project-name-function
            (lambda (project-root)
              (if (string= project-root dir)
                  custom-name
                (projectile-default-project-name project-root))))
      (projectile-add-known-project dir)))



  :config
  ;; Only use projectile for directories containing .git
  ;; https://emacs.stackexchange.com/questions/20534/s3fs-is-too-slow-for-emacs
  (add-hook 'find-file-hook
            (lambda ()
              (if (locate-dominating-file default-directory ".git")
                  (projectile-mode))))

  (add-to-list 'projectile-globally-ignored-directories "^venv$")
  (add-to-list 'projectile-globally-ignored-directories "^\\.venv$")

  (setq projectile-enable-caching t)
  ;; PROJECTILE INDEXING
  ;; `alien` is fastest locally, but fails over TRAMP, so use native
  (setq projectile-indexing-method 'native)
  ;; ATTEMPT to use native for remote, alien for local - not working
  ;; (defun my/projectile-set-indexing-method-based-on-location ()
  ;;   (setq-local projectile-indexing-method
  ;;               (if (file-remote-p default-directory)
  ;;                   'native
  ;;                 'alien)))
  ;; (add-hook 'projectile-after-switch-project-hook #'my/projectile-set-indexing-method-based-on-location)


  ;; PROJECTILE OVER TRAMP
  ;; (setq projectile-mode-line "Projectile") ; avoid expensive project name resolution

  ;; ;; Code from here:https://sideshowcoder.com/2017/10/24/projectile-and-tramp/
  ;; However, this looks to be out of date (projectile-on no longer exists)
  ;;
  ;; (defun my/projectile-on-exclude-tramp (orig-fn &rest args)
  ;;   "Prevent `projectile-on` from enabling in TRAMP or Dired TRAMP buffers."
  ;;   (unless (--any? (and it (file-remote-p it))
  ;;                   (list
  ;;                    (buffer-file-name)
  ;;                    list-buffers-directory
  ;;                    default-directory
  ;;                    dired-directory))
  ;;     (apply orig-fn args)))

  ;; (advice-add 'projectile-on :around #'my/projectile-on-exclude-tramp)

  ;; ;; Skip root lookup in remote buffers
  ;; (defun my/ad-projectile-project-root (orig-fn &optional dir)
  ;;   "Disable projectile root lookup in TRAMP buffers."
  ;;   (let ((dir (file-truename (or dir default-directory))))
  ;;     (unless (file-remote-p dir)
  ;;       (funcall orig-fn dir))))

  ;; (advice-add 'projectile-project-root :around #'my/ad-projectile-project-root)
  ;; END PROJECTILE OVER TRAMP

  ;; ATTEMPT TO OPTIMIZE PROJECTILE OVER TRAMP
  ;; Auto-register remote projects
  (defun my/projectile-auto-register-tramp-project ()
    "Automatically register remote TRAMP project if not already known."
    (when (and (file-remote-p default-directory)
               (not (projectile-project-p)))
      (let ((root (vc-root-dir)))
        (when root
          (projectile-add-known-project root)
          (projectile-cache-project root)
          (message "Projectile: Registered remote project %s" root)))))

  ;; Run on file open
  (add-hook 'find-file-hook #'my/projectile-auto-register-tramp-project)

  ;; Run after switching projects
  (add-hook 'projectile-after-switch-project-hook #'my/projectile-auto-register-tramp-project)

  ;; Update the projectile name to include the hostname
  ;; ==================================================
  (defun my-projectile-project-name-advice (orig-fun &rest args)
    "Advice to append hostname to project name for remote projects."
    (let ((project-name (apply orig-fun args)))
      (if (file-remote-p default-directory)
          (let* ((remote-host (tramp-file-name-host (tramp-dissect-file-name default-directory)))
                 (remote-prefix (concat remote-host ":")))
            (concat remote-prefix project-name))
        project-name)))
  (advice-add 'projectile-project-name :around #'my-projectile-project-name-advice)

  ;; Automatically update persp-name with projectile name
  ;; ====================================================
  (defun my-projectile-after-switch-hook ()
    "Hook to rename perspective after switching projects in Projectile."
    (let ((new-persp-name (projectile-project-name)))
      (unless (persp-with-name-exists-p new-persp-name)
        (persp-rename new-persp-name))))

  (add-hook 'projectile-after-switch-project-hook 'my-projectile-after-switch-hook)

  ;; Command to create new projects
  ;; ==============================
  (defun my/create-new-project ()
    "Interactively prompt the user for a directory, creating it if it doesn't exist.
The prompt starts at the user's home directory with completion enabled."
    (interactive)
    (let* ((dir (read-directory-name "New project path: " nil nil nil)))
      (message "New directory: %s" dir)
      (unless (file-directory-p dir)
        (message "Created new directory: %s" dir)
        (make-directory dir t)
        )
      (message "Initializing git repo")
      (let ((default-directory dir))
        (shell-command "git init")
        )
      (message "Adding to projectile")
      (projectile-add-known-project dir)
      (message "Switching to project")
      (projectile-switch-project-by-name dir)
      )
    )
  (map! :leader
        :desc "Create new project"
        "p n" #'my/create-new-project)
  )
