;;; ../.dotfiles/doom.d/packages/projectile.el -*- lexical-binding: t; -*-

(use-package! projectile
  :config

  (add-to-list 'projectile-globally-ignored-directories "^venv$")
  (add-to-list 'projectile-globally-ignored-directories "^\\.venv$")

  (setq projectile-indexing-method 'alien)
  (setq projectile-enable-caching t) ;; Enable caching. Options: `t`: cache for a session `persistent` cache across sessions

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
