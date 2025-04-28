;;; ../.dotfiles/doom.d/packages/persp.el -*- lexical-binding: t; -*-

(after! persp-mode
  ;; Always display workspace tab bar on bottom
  ;; (defun display-workspaces-in-minibuffer ()
  ;;   (with-current-buffer " *Minibuf-0*"
  ;;     (erase-buffer)
  ;;     (insert (+workspace--tabline))))
  ;; (run-with-idle-timer 1 t #'display-workspaces-in-minibuffer)
  ;; (+workspace/display)

  ;; (custom-set-faces!
  ;; '(+workspace-tab-face :inherit default :family "Jost" :height 135)
  ;; '(+workspace-tab-selected-face :inherit (highlight +workspace-tab-face)))

  ;; (defun my/set-persp-default-directory-to-home (_workspace &rest _)
  ;;   "Set `default-directory` to `~` for newly created perspectives."
  ;;   (message "setting default directory to home")
  ;;   (cd (expand-file-name "~/"))
  ;;   (message "done")
  ;;   )

  ;; ;; ATTEMPT 2: FROM CLAUDE
  ;; ;; Define parameter key for storing the directory
  ;; (defvar persp-directory-param-key 'default-directory-param
  ;;   "Parameter key for storing the default directory in a perspective.")

  ;; ;; Function to save the current directory when leaving a perspective
  ;; (defun save-persp-default-directory (persp-name)
  ;;   "Save the current default directory to the perspective's parameters."
  ;;   (let ((persp (get-current-persp))
  ;;         (dir default-directory))
  ;;     (when (and persp dir)
  ;;       (set-persp-parameter persp-directory-param-key dir persp))))

  ;; ;; Function to restore the default directory when entering a perspective
  ;; (defun restore-persp-default-directory (persp-name &rest _args)
  ;;   "Restore the default directory from the perspective's parameters."
  ;;   (let* ((persp (persp-get-by-name persp-name))
  ;;          (dir (and persp (persp-parameter persp-directory-param-key persp))))
  ;;     (when dir
  ;;       (setq default-directory dir))))

  ;; ;; Add hooks to save/restore the directory when switching perspectives
  ;; (remove-hook 'persp-before-switch-functions #'save-persp-default-directory)
  ;; (remove-hook 'persp-activated-functions #'restore-persp-default-directory)

  ;; (add-hook 'persp-before-switch-functions #'save-persp-default-directory)
  ;; (add-hook 'persp-activated-functions #'restore-persp-default-directory)

  ;; ;; ATTEMPT 1: FROM CHATGPT
  ;; ;; Doesn't work correctly when switching back and forth
  ;; (defun my/save-default-directory (&rest _)
  ;;   "Save `default-directory` in the current perspective and log it."
  ;;   (when (and (bound-and-true-p persp-mode) persp-names-cache)
  ;;     (ignore-errors
  ;;       ;; Ensure we're saving the correct directory
  ;;       (set-persp-parameter 'default-directory default-directory)
  ;;       (message "[persp] Saving default-directory: %s (Perspective: %s)"
  ;;                default-directory
  ;;                (safe-persp-name (get-current-persp))))))

  ;; (defun my/restore-default-directory (&rest _)
  ;;   "Restore `default-directory` from the current perspective and log it."
  ;;   (when (and (bound-and-true-p persp-mode) persp-names-cache)
  ;;     (ignore-errors
  ;;       (let ((dir (persp-parameter 'default-directory)))
  ;;         (if (and (stringp dir) (file-directory-p dir))
  ;;             (progn
  ;;               ;; Ensure the correct directory is restored
  ;;               (setq default-directory dir)
  ;;               (message "[persp] Restored default-directory: %s (Perspective: %s)"
  ;;                        dir
  ;;                        (safe-persp-name (get-current-persp))))
  ;;           (message "[persp] No valid default-directory found, keeping: %s (Perspective: %s)"
  ;;                    default-directory
  ;;                    (safe-persp-name (get-current-persp))))))))
  ;; ;; (remove-hook 'persp-before-switch-functions #'my/save-default-directory)
  ;; ;; (remove-hook 'persp-activated-functions #'my/restore-default-directory)
  ;; ;; Save the directory when switching away from a workspace
  ;; ;; Restore the directory when switching to a workspace
  ;; (add-hook 'persp-before-deactivate-functions #'my/save-default-directory)
  ;; (add-hook 'persp-activated-functions #'my/restore-default-directory)


  ;; ATTEMPT 3: Claude-refined chatGPT
  ;; Function to save the default directory when leaving a perspective
  (defun my/save-default-directory (persp)
    "Save `default-directory` for the perspective that's being deactivated."
    (when (and persp (persp-p persp))
      (let ((persp-name (safe-persp-name persp)))
        (set-persp-parameter 'default-directory default-directory persp)
        (message "[persp] Saved default-directory: %s for perspective: %s"
                 default-directory persp-name))))

  ;; Function to restore the default directory when entering a perspective
  (defun my/restore-default-directory (persp &rest _)
    "Restore `default-directory` for the perspective that's being activated."
    (when (and persp (persp-p persp))
      (let* ((persp-name (safe-persp-name persp))
             (dir (persp-parameter 'default-directory persp)))
        (if (and (stringp dir) (file-directory-p dir))
            (progn
              (setq default-directory dir)
              (message "[persp] Restored default-directory: %s for perspective: %s"
                       dir persp-name))
          (message "[persp] No valid default-directory found for perspective: %s"
                   persp-name)))))

  ;; ATTEMPT 4: Personal debugging/development
  (defun my/save-default-directory (&rest _)
    (message "In save-default-directory")
    (let (
          (persp-name (safe-persp-name (get-current-persp)))
          )
      (message "[persp: %s] Default dir: %s" persp-name default-directory)
      (set-persp-parameter 'default-dir default-directory)
      (message "Set persp-default-dir to: %s" (persp-parameter 'default-dir))
      )
    )
  (defun my/restore-default-directory (&rest _)
    (message "In restore-default-directory")
    (let (
          (persp-name (safe-persp-name (get-current-persp)))
          )
      (message "[persp: %s] Default dir: %s" persp-name default-directory)
      (message "Saved persp-default-dir is: %s" (persp-parameter 'default-dir))
      )
    )

  ;; Hook into perspective switching functions
  (remove-hook 'persp-before-deactivate-functions #'my/save-default-directory)
  (remove-hook 'persp-activated-functions #'my/restore-default-directory)
  (add-hook 'persp-before-deactivate-functions #'my/save-default-directory)
  (add-hook 'persp-activated-functions #'my/restore-default-directory)


  (defun workspaces-formatted ()
    ;; fancy version as in screenshot
    (+doom-dashboard--center (frame-width)
                             (let ((names (or persp-names-cache nil))
                                   (current-name (safe-persp-name (get-current-persp))))
                               (mapconcat
                                #'identity
                                (cl-loop for name in names
                                         for i to (length names)
                                         collect
                                         (concat (propertize (format " %d" (1+ i)) 'face
                                                             `(:inherit ,(if (equal current-name name)
                                                                             '+workspace-tab-selected-face
                                                                           '+workspace-tab-face)
                                                               :weight bold))
                                                 (propertize (format " %s " name) 'face
                                                             (if (equal current-name name)
                                                                 '+workspace-tab-selected-face
                                                               '+workspace-tab-face))))
                                " "))))

  )
