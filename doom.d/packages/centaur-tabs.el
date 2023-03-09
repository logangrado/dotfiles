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
         ;; ("H" . centaur-tabs-backward)
         ;; ("L" . centaur-tabs-forward)
         )

  :hook
  ;; Disalbe tabs in vterm
  ((vterm-mode vterm-toggle--mode) . centaur-tabs-local-mode)

  :config
  (centaur-tabs-mode t)
  (setq centaur-tabs-height 32
        centaur-tabs-style "bar"
        centaur-tabs-set-icons t
        centaur-tabs-set-modified-marker t
        centaur-tabs-set-bar 'left  ;;'under doesn't work
        centaur-tabs-set-close-button nil
        )

  ;; The default seems to do a good job
  (centaur-tabs-group-buffer-groups) ;; Default
  ;; (centaur-tabs-group-by-projectile-project) ;; This shows magit buffers, which we dont want!

  ;; ;; These seem to help keep vterm tabs out of tabbar group?
  ;; (defun centaur-tabs-buffer-groups ()
  ;;   (list
  ;;    (cond
  ;;     ((string-match "vterm" (format "%s" (buffer-name))) "Emacs")
  ;;     (t (centaur-tabs-get-group-name (current-buffer))))))
  ;;

  ;; (defun my-show-only-vterm ()
  ;;   (when (bound-and-true-p centaur-tabs-mode)
  ;;         (if (string-match "vterm" (format "%s" (buffer-name)))
  ;;             (centaur-tabs-local-mode)
  ;;           (centaur-tabs-local-mode 0))))

  ;; ;; (add-hook! 'window-configuration-change-hook 'my-show-only-vterm)
  ;; (add-hook! 'buffer-list-update-hook 'my-show-only-vterm)
  )
