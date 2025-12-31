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

  ;; ALWAYS SHOW WORKSPACES - but not in the minibuffer
  ;; --------------------------------------------------
  ;; https://discourse.doomemacs.org/t/permanently-display-workspaces-in-the-tab-bar/4088
  (defun lg/invisible-current-workspace ()
    "The tab bar doesn't update when only faces change (i.e. the
current workspace), so we invisibly print the current workspace
name as well to trigger updates"
    (propertize (safe-persp-name (get-current-persp)) 'invisible t))
  (defun lg/workspaces-formatted ()
    "Render Doom workspaces in the tab bar, centered.
     Also ensure `none` workspace is filtered out"
    (+doom-dashboard--center
     (frame-width)
     (let* ((names (or persp-names-cache
                       (persp-names-current-frame-fast-ordered)))
            ;; Drop the nil workspace (usually named `persp-nil-name`, e.g. \"none\")
            (names (cl-remove persp-nil-name names :test #'string=))
            (current-name (safe-persp-name (get-current-persp)))
            (i 0))
       (mapconcat
        #'identity
        (cl-loop
         for name in names
         do (cl-incf i)
         collect
         (concat
          (propertize (format " %d" i)
                      'face `(:inherit ,(if (equal current-name name)
                                            '+workspace-tab-selected-face
                                          '+workspace-tab-face)
                              :weight bold))
          (propertize (format " %s " name)
                      'face (if (equal current-name name)
                                '+workspace-tab-selected-face
                              '+workspace-tab-face))))
        " "))))

  (customize-set-variable 'tab-bar-format '(lg/workspaces-formatted tab-bar-format-align-right lg/invisible-current-workspace))

  ;; don't show current workspaces when we switch, since we always see them
  (advice-add #'+workspace/display :override #'ignore)
  ;; same for renaming and deleting (and saving, but oh well)
  (advice-add #'+workspace-message :override #'ignore)

  ;; Ensure swap-left/right updates tabbar
  (defun lg/refresh-workspace-tab-bar (&rest _)
    "Force tab-bar to refresh after workspace reordering."
    ;; Update Doom's cache if you're using it in rendering
    (when (boundp 'persp-names-cache)
      (setq persp-names-cache (persp-names-current-frame-fast-ordered)))
    ;; Force tab-bar recompute + redraw
    (when (fboundp 'tab-bar--invalidate-cache)
      (tab-bar--invalidate-cache))
    (force-mode-line-update t)
    (redraw-display))
  (advice-add #'+workspace/swap-left  :after #'lg/refresh-workspace-tab-bar)
  (advice-add #'+workspace/swap-right :after #'lg/refresh-workspace-tab-bar)
  ;; --------------------------------------------------

  )

(after! tab-bar
  ;; Used to show workspaces
  (tab-bar-mode 1)
  (setq tab-bar-show 1) 
  )
