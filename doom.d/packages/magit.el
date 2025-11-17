;;; ../.dotfiles/doom.d/packages/magit.el -*- lexical-binding: t; -*-

(use-package magit
  :init
  (when (eq system-type 'darwin)
    (setq magit-git-executable "/opt/homebrew/bin/git"))

  :config
  (setq magit-diff-refine-hunk t)

  (custom-set-faces!
    `(magit-branch-local :foreground ,(nth 2 (doom-themes--colors-p 'blue)) :bold t)
    `(magit-branch-current :inherit magit-branch-local :underline t)
    `(magit-branch-remote :foreground ,(nth 2 (doom-themes--colors-p 'green)) :bold t)
    `(magit-branch-remote-head :inherit magit-branch-remote :box nil :underline t)
    )

  (map! :map magit-mode-map
        "K" #'(lambda () (interactive) (previous-line 10) (evil-scroll-line-up 10))
        "J" #'(lambda () (interactive) (next-line 10) (evil-scroll-line-down 10))
        )

  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules
                          'magit-insert-unpulled-from-upstream)
  (setq magit-module-sections-nested nil)

  ;; Make magit transient buffers on bottom of frame
  ;; This isn't as "nice", but it prevents magit from resizing windows!
  (setq transient-display-buffer-action '(display-buffer-at-bottom)
        magit-display-buffer-function #'+magit-display-buffer-fn
        magit-bury-buffer-function #'magit-mode-quit-window)

  (transient-append-suffix 'magit-log "-A"
    '("=p" "First parent" "--first-parent" :level 1))

  ;; Wrap lines in diff view
  (defun my-wrap-lines ()
    "Disable `truncate-lines' in the current buffer."
    (setq truncate-lines nil))

  (add-hook 'magit-diff-mode-hook #'my-wrap-lines)


  (defun lg/magit-log-branches ()
    "Show logs for local branches and their remotes, plus main branches."
    (interactive)
    (let* ((local-branches (magit-list-local-branch-names))
           (remote-branches (magit-list-remote-branch-names "origin"))
           (remote-pairs (seq-filter
                          (lambda (remote-ref)
                            (member remote-ref remote-branches))
                          (mapcar (lambda (branch)
                                    (concat "origin/" branch))
                                  local-branches)))
           (main-branches (seq-filter
                           (lambda (ref)
                             (member ref remote-branches))
                           '("origin/dev" "origin/main" "origin/master")))
           (all-refs (append local-branches
                             remote-pairs
                             main-branches
                             (magit-list-stashes)
                             )))
      (magit-log-setup-buffer
       (delete-dups all-refs)
       (list "--graph" "--decorate" "--ignore-missing")
       nil
       "test message"
       'magit-log-mode)))

  (transient-append-suffix 'magit-log "b"
    '("l" "Locals and refs" lg/magit-log-branches))
  (transient-append-suffix 'magit-log "b"
    '("c" "current" magit-log-current))

  )

;; (use-package magit-todos
;;   :after magit
;;   :config (magit-todos-mode 1)
;;   (add-to-list 'magit-todos-exclude-globs "*.html")
;;   )
