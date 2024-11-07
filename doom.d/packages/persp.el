;;; ../.dotfiles/doom.d/packages/persp.el -*- lexical-binding: t; -*-

;; Always display workspace tab bar on bottom
(after! persp-mode
  ;; (defun display-workspaces-in-minibuffer ()
  ;;   (with-current-buffer " *Minibuf-0*"
  ;;     (erase-buffer)
  ;;     (insert (+workspace--tabline))))
  ;; (run-with-idle-timer 1 t #'display-workspaces-in-minibuffer)
  ;; (+workspace/display)

  ;; (custom-set-faces!
  ;; '(+workspace-tab-face :inherit default :family "Jost" :height 135)
  ;; '(+workspace-tab-selected-face :inherit (highlight +workspace-tab-face)))

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
