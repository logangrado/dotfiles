;;; ../.dotfiles/doom.d/packages/magit.el -*- lexical-binding: t; -*-

(use-package magit
  :init
  (when (eq system-type 'darwin)
    (setq magit-git-executable "/opt/homebrew/bin/git"))

  :config
  (setq magit-diff-refine-hunk t)

  (custom-set-faces!
    `(magit-branch-local       :foreground ,(nth 2 (doom-themes--colors-p 'blue))   :bold t)
    `(magit-branch-current     :inherit magit-branch-local :underline t)
    `(magit-branch-remote      :foreground ,(nth 2 (doom-themes--colors-p 'green))  :bold t)
    `(magit-branch-remote-head :inherit magit-branch-remote :box nil :underline t)
    `(magit-branch-worktree    :foreground ,(nth 2 (doom-themes--colors-p 'yellow)) :bold t :underline t))

  ;; -----------------------------------------------------------
  ;; Log decoration: highlight branches checked out in other worktrees
  ;; -----------------------------------------------------------

  ;; Cache worktree data per-repo to avoid shelling out on every log line.
  ;; Keyed by repo toplevel path; invalidated on each magit buffer refresh.
  (defvar lg/worktree--branch-cache (make-hash-table :test #'equal)
    "Cache: repo-toplevel -> list of short branch names in non-current worktrees.")

  (defun lg/worktree--refresh-cache ()
    "Rebuild the worktree branch cache for the current repo."
    (when-let ((top (magit-toplevel)))
      (let* ((current-top (expand-file-name top))
             (all (magit-list-worktrees))
             (other-branches
              (delq nil
                    (mapcar (lambda (wt)
                              (when (and (not (string= (expand-file-name (car wt))
                                                       current-top))
                                         (nth 2 wt))   ; BRANCH element
                                (nth 2 wt)))
                            all))))
        (puthash current-top other-branches lg/worktree--branch-cache))))

  (add-hook 'magit-refresh-buffer-hook #'lg/worktree--refresh-cache)

  (defun lg/magit--worktree-ref-labels (result)
    "Post-process `magit-format-ref-labels' output.
Scan for face runs of `magit-branch-local'; if the run text matches a
branch checked out in another worktree, reface it as `magit-branch-worktree'."
    (or (when-let* ((top (magit-toplevel))
                    (wt-branches (gethash (expand-file-name top)
                                          lg/worktree--branch-cache)))
          (let ((result (copy-sequence result))
                (pos 0)
                (len (length result)))
            (while (< pos len)
              (let ((end (next-single-property-change pos 'font-lock-face result len)))
                (when (and (eq (get-text-property pos 'font-lock-face result)
                               'magit-branch-local)
                           (member (substring-no-properties result pos end)
                                   wt-branches))
                  (put-text-property pos end 'font-lock-face 'magit-branch-worktree result))
                (setq pos end)))
            result))
        result))

  (advice-add 'magit-format-ref-labels :filter-return
              #'lg/magit--worktree-ref-labels)

  (map! :map magit-mode-map
        "K" #'(lambda () (interactive) (previous-line 10) (evil-scroll-line-up 10))
        "J" #'(lambda () (interactive) (next-line 10) (evil-scroll-line-down 10))
        )

  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules
                          'magit-insert-unpulled-from-upstream)
  (setq magit-module-sections-nested nil)

  ;; Show worktrees section in status (built-in, only appears with 2+ worktrees)
  (magit-add-section-hook 'magit-status-sections-hook
                          #'magit-insert-worktrees
                          'magit-insert-status-headers
                          t)   ; t = insert AFTER magit-insert-status-headers

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
  (defun lg/magit-log-current-and-main ()
    "Show logs for current branch (and its remote) plus main branches."
    (interactive)
    (let* ((current (magit-get-current-branch))
           (remote-branches (magit-list-remote-branch-names "origin"))
           (local-branches (magit-list-local-branch-names))
           ;; Current branch + its remote if it exists
           (current-refs (when current
                           (if (member (concat "origin/" current) remote-branches)
                               (list current (concat "origin/" current))
                             (list current))))
           ;; Main branches - local and remote
           (main-names '("dev" "main" "master"))
           (main-local (seq-filter (lambda (b) (member b local-branches)) main-names))
           (main-remote (seq-filter (lambda (r) (member r remote-branches))
                                    (mapcar (lambda (b) (concat "origin/" b)) main-names)))
           (all-refs (delete-dups (append current-refs main-local main-remote))))
      (magit-log-setup-buffer
       all-refs
       (list "--graph" "--decorate" "--ignore-missing")
       nil
       nil
       'magit-log-mode)))

  (transient-append-suffix 'magit-log "b"
    '("l" "locals and refs" lg/magit-log-branches))
  (transient-append-suffix 'magit-log "b"
    '("c" "current and main" lg/magit-log-current-and-main))
  (transient-append-suffix 'magit-log "b"
    '("C" "current" magit-log-current))

  (transient-define-prefix my/magit-x-transient ()
    "Extra actions (discard / resolve conflicts)."
    ["Discard"
     ("x" "Discard (same as old `x`)" magit-discard)]
    ["Resolve conflicts (smerge)"
     ("u" "Keep upper/ours (hunk)" magit-smerge-keep-upper)
     ("l" "Keep lower/theirs (hunk)" magit-smerge-keep-lower)
     ("a" "Keep all (hunk)" magit-smerge-keep-all)])

  (transient-replace-suffix 'magit-dispatch "x"
    '("x" "discard…" my/magit-x-transient :transient transient--do-replace))

  ;; Also rebind `x` in the actual magit-status keymap
  (define-key magit-hunk-section-map (kbd "x") #'my/magit-x-transient)
  )

;; (use-package magit-todos
;;   :after magit
;;   :config (magit-todos-mode 1)
;;   (add-to-list 'magit-todos-exclude-globs "*.html")
;;   )
