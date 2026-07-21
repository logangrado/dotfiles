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
    "Render Doom workspaces in the tab bar, left-aligned, one entry per repo
\(worktrees of the same repo share one entry — see `lg/worktree-repo-list')."
    (let* ((names (if (fboundp 'lg/worktree-repo-list)
                      (lg/worktree-repo-list)
                    ;; Fallback if persp-worktree.el hasn't loaded yet.
                    (cl-remove persp-nil-name
                               (or persp-names-cache
                                   (persp-names-current-frame-fast-ordered))
                               :test #'string=)))
           (current-name (safe-persp-name (get-current-persp)))
           (current-repo (if (fboundp 'lg/worktree-persp-repo)
                             (lg/worktree-persp-repo current-name)
                           current-name))
           (i 0))
      (mapconcat
       #'identity
       (cl-loop
        for name in names
        do (cl-incf i)
        collect
        (concat
         (propertize (format " %d" i)
                     'face `(:inherit ,(if (equal current-repo name)
                                           '+workspace-tab-selected-face
                                         '+workspace-tab-face)
                             :weight bold))
         (propertize (format " %s " name)
                     'face (if (equal current-repo name)
                               '+workspace-tab-selected-face
                             '+workspace-tab-face))))
       " ")))

  (customize-set-variable 'tab-bar-format '(lg/workspaces-formatted tab-bar-format-align-right lg/worktree-bar-formatted lg/invisible-current-workspace))

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

  ;; Refresh on every buffer/window change, not just persp switches — the
  ;; worktree segment's highlighted entry (and whether it shows at all)
  ;; depends on the current buffer, which changes far more often than the
  ;; persp does.
  (add-hook 'window-buffer-change-functions #'lg/refresh-workspace-tab-bar)
  (add-hook 'window-selection-change-functions #'lg/refresh-workspace-tab-bar)
  )

(after! tab-bar
  ;; Used to show workspaces
  (tab-bar-mode 1)
  (setq tab-bar-show 1)
  )

;; --------------------------------------------------------------------------
;; Worktree segment, folded into the same frame-global `tab-bar-format' row
;; as the persp tabs (rather than a separate per-window slot like
;; tab-line-format/header-line-format — those are scoped per-window, so a
;; single persp with two windows open would render two independent worktree
;; rows instead of one shared one). Rendering itself lives in
;; `lg/worktree-bar-formatted' (persp-worktree.el) — this file only wires it
;; into `tab-bar-format' above.
;; --------------------------------------------------------------------------
