;;; ../.dotfiles/doom.d/packages/projectile.el -*- lexical-binding: t; -*-

(use-package! projectile
  :config

  (add-to-list 'projectile-globally-ignored-directories "^venv$")
  (add-to-list 'projectile-globally-ignored-directories "^\\.venv$")

  (setq projectile-indexing-method 'native)

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
  )
