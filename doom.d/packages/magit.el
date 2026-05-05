;;; ../.dotfiles/doom.d/packages/magit.el -*- lexical-binding: t; -*-

(use-package magit
  :init
  (when (eq system-type 'darwin)
    (setq magit-git-executable "/opt/homebrew/bin/git"))

  :config

  ;; -----------------------------------------------------------
  ;; Settings
  ;; -----------------------------------------------------------

  (setq magit-diff-refine-hunk t
        magit-module-sections-nested nil
        ;; Show transient popups at the bottom of the frame instead of
        ;; splitting the magit window (prevents unwanted window resizing).
        transient-display-buffer-action '(display-buffer-at-bottom)
        magit-display-buffer-function #'+magit-display-buffer-fn
        magit-bury-buffer-function #'magit-mode-quit-window)

  ;; -----------------------------------------------------------
  ;; Faces
  ;; -----------------------------------------------------------

  ;; magit-branch-worktree is not a built-in Magit face; define it so
  ;; custom-set-faces! has something to customize and describe-face can find it.
  (defface magit-branch-worktree
    '((t :inherit magit-branch-local))
    "Face for a branch checked out in another worktree."
    :group 'magit-faces)

  (custom-set-faces!
    `(magit-branch-local       :foreground ,(nth 2 (doom-themes--colors-p 'blue))   :bold t)
    `(magit-branch-remote      :foreground ,(nth 2 (doom-themes--colors-p 'green))  :bold t)
    `(magit-branch-remote-head :inherit magit-branch-remote :box nil :underline t)
    `(magit-branch-worktree    :inherit magit-branch-local :box t)
    `(magit-branch-current     :foreground ,(nth 2 (doom-themes--colors-p 'magenta)) :bold t :underline nil :box t))

  ;; -----------------------------------------------------------
  ;; Worktree decoration: highlight branches in other worktrees
  ;; -----------------------------------------------------------
  ;; In the log view, branches checked out in other worktrees are refaced
  ;; from magit-branch-local → magit-branch-worktree (box style) so you
  ;; can visually distinguish them. Uses a per-repo cache to avoid shelling
  ;; out on every log line; the cache is rebuilt on each magit buffer refresh.

  ;; Cache: keyed by repo toplevel path; invalidated via magit-refresh-buffer-hook.
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

  (defun lg/magit--worktree-ref-labels (result)
    "Post-process `magit-format-ref-labels' output.
Scan for face runs of `magit-branch-local'; if the run text matches a
branch checked out in another worktree, reface it as `magit-branch-worktree'."
    (let ((top (magit-toplevel)))
      (if (not top)
          result
        (let ((top-abs (expand-file-name top)))
          ;; Lazy init: magit-refresh-buffer-hook fires AFTER the first render,
          ;; so populate the cache here on first miss rather than waiting for the hook.
          (when (eq (gethash top-abs lg/worktree--branch-cache 'lg/miss) 'lg/miss)
            (lg/worktree--refresh-cache))
          (let ((wt-branches (gethash top-abs lg/worktree--branch-cache)))
            (if (not wt-branches)
                result
              (let ((result (copy-sequence result))
                    (pos 0)
                    (len (length result)))
                (while (< pos len)
                  (let ((end (next-single-property-change pos 'font-lock-face result len)))
                    (when (and (eq (get-text-property pos 'font-lock-face result)
                                   'magit-branch-local)
                               (member (substring-no-properties result pos end)
                                       wt-branches))
                      ;; Magit sets both face and font-lock-face via
                      ;; magit--propertize-face; update both so rendering works
                      ;; regardless of which property the display engine reads.
                      (put-text-property pos end 'font-lock-face
                                         'magit-branch-worktree result)
                      (put-text-property pos end 'face
                                         'magit-branch-worktree result))
                    (setq pos end)))
                result)))))))

  ;; -----------------------------------------------------------
  ;; Buffer management: one magit session per workspace
  ;; -----------------------------------------------------------
  ;; Invariant: at most one magit-status buffer per persp-mode perspective.
  ;; Sub-buffers (log, diff) are transient children — q returns to status.
  ;;
  ;; Enforcement:
  ;; - lg/magit-status-for-workspace (SPC g g): if ANY magit-status buffer
  ;;   exists in the perspective, go to it. Never opens a second one.
  ;; - lg/magit-cleanup-workspace-buffers (:after advice on
  ;;   magit-status-setup-buffer): when magit creates a new status buffer
  ;;   (e.g. worktree switch via RET), kill all other magit buffers so
  ;;   the old status buffer doesn't linger in the window history.
  ;; - lg/magit-back-to-status (q): from sub-buffers, return to the status
  ;;   buffer. From status, quit magit entirely.
  ;; - lg/magit-kill-stale-log-buffers (:before advice on
  ;;   magit-log-setup-buffer): kill old log buffers when switching log views.

  (defun lg/magit--workspace-buffers ()
    "Return the buffer list for the current perspective, or all buffers."
    (if (bound-and-true-p persp-mode)
        (persp-buffer-list)
      (buffer-list)))

  (defun lg/magit-kill-workspace-magit-buffers (&optional keep)
    "Kill all magit-mode buffers in the current perspective except KEEP."
    (dolist (buf (lg/magit--workspace-buffers))
      (when (and (buffer-live-p buf)
                 (not (eq buf keep))
                 (with-current-buffer buf
                   (derived-mode-p 'magit-mode)))
        (kill-buffer buf))))

  (defun lg/magit-status-for-workspace ()
    "Open magit-status, reusing the existing buffer for this perspective.
If any magit-status buffer exists in the workspace, go to it — never
opens a second one, even if the user has cd'd to a different directory."
    (interactive)
    (let ((existing (cl-find-if
                     (lambda (buf)
                       (and (buffer-live-p buf)
                            (with-current-buffer buf
                              (eq major-mode 'magit-status-mode))))
                     (lg/magit--workspace-buffers))))
      (if existing
          (progn
            (pop-to-buffer existing '((display-buffer-same-window)))
            (magit-refresh))
        (magit-status))))

  (defun lg/magit-cleanup-workspace-buffers (&rest _)
    "After a new status buffer is created, kill all other magit buffers.
Ensures the workspace never accumulates stale magit buffers (e.g. after
switching to a different worktree of the same project)."
    (lg/magit-kill-workspace-magit-buffers (current-buffer)))

  (defun lg/magit-back-to-status ()
    "From non-status magit buffer: quit and show refreshed magit-status in same window.
From magit-status: quit magit entirely."
    (interactive)
    (if (eq major-mode 'magit-status-mode)
        (magit-mode-bury-buffer)
      (let ((win (selected-window))
            (topdir (or (magit-toplevel) default-directory)))
        ;; quit-window buries the log/diff buffer but keeps the window alive
        (quit-window nil win)
        (let ((status-buf
               (cl-find-if
                (lambda (buf)
                  (and (buffer-live-p buf)
                       (with-current-buffer buf
                         (and (eq major-mode 'magit-status-mode)
                              (equal (expand-file-name default-directory)
                                     (expand-file-name topdir))))))
                (lg/magit--workspace-buffers))))
          (if status-buf
              (progn
                ;; set-window-buffer guarantees same-window display,
                ;; bypassing Doom's display-buffer popup rules.
                (set-window-buffer win status-buf)
                (select-window win)
                (with-current-buffer status-buf (magit-refresh)))
            (select-window win)
            (magit-status-setup-buffer topdir))))))

  (defun lg/magit-kill-stale-log-buffers (&rest _)
    "Kill existing magit-log buffers for the current repo/perspective.
Intended as :before advice on `magit-log-setup-buffer' so switching
between log views (e.g. log-all → log-local) doesn't leave stale
buffers that q would cycle back through."
    (let* ((topdir (or (magit-toplevel) default-directory))
           (cur (current-buffer)))
      (dolist (buf (lg/magit--workspace-buffers))
        (when (and (buffer-live-p buf)
                   (not (eq buf cur))
                   (with-current-buffer buf
                     (and (eq major-mode 'magit-log-mode)
                          (equal (expand-file-name default-directory)
                                 (expand-file-name topdir)))))
          (kill-buffer buf)))))

  ;; -----------------------------------------------------------
  ;; Log commands
  ;; -----------------------------------------------------------

  (defun lg/magit-log-branches (&optional args files)
    "Show logs for local branches and their remotes, plus main branches."
    (interactive (list (transient-args 'magit-log) nil))
    (let* ((local-branches (magit-list-local-branch-names))
           (remote-branches (magit-list-remote-branch-names "origin"))
           ;; For each local branch, include origin/<branch> if it exists
           (remote-pairs (seq-filter
                          (lambda (remote-ref)
                            (member remote-ref remote-branches))
                          (mapcar (lambda (branch)
                                    (concat "origin/" branch))
                                  local-branches)))
           ;; Always include main-like remote branches if they exist
           (main-branches (seq-filter
                           (lambda (ref)
                             (member ref remote-branches))
                           '("origin/dev" "origin/main" "origin/master")))
           (all-refs (append local-branches
                             remote-pairs
                             main-branches
                             (magit-list-stashes)))
           (merged-args (seq-uniq
                         (append (list "--graph" "--decorate" "--ignore-missing")
                                 args))))
      (magit-log-setup-buffer
       (delete-dups all-refs)
       merged-args
       files)))

  (defun lg/magit-log-current-and-main (&optional args files)
    "Show logs for current branch (and its remote) plus main branches."
    (interactive (list (transient-args 'magit-log) nil))
    (let* ((current (magit-get-current-branch))
           (remote-branches (magit-list-remote-branch-names "origin"))
           (local-branches (magit-list-local-branch-names))
           ;; Current branch + its remote tracking branch if it exists
           (current-refs (when current
                           (if (member (concat "origin/" current) remote-branches)
                               (list current (concat "origin/" current))
                             (list current))))
           ;; Main branches - include both local and remote if they exist
           (main-names '("dev" "main" "master"))
           (main-local (seq-filter (lambda (b) (member b local-branches)) main-names))
           (main-remote (seq-filter (lambda (r) (member r remote-branches))
                                    (mapcar (lambda (b) (concat "origin/" b)) main-names)))
           (all-refs (delete-dups (append current-refs main-local main-remote)))
           (merged-args (seq-uniq
                         (append (list "--graph" "--decorate" "--ignore-missing")
                                 args))))
      (magit-log-setup-buffer
       all-refs
       merged-args
       files
       nil
       'magit-log-mode)))

  ;; -----------------------------------------------------------
  ;; Discard / smerge transient
  ;; -----------------------------------------------------------
  ;; Replaces the default `x` (magit-discard) with a transient that also
  ;; offers smerge conflict resolution commands in one menu.

  (transient-define-prefix lg/magit-x-transient ()
    "Extra actions (discard / resolve conflicts)."
    ["Discard"
     ("x" "Discard (same as old `x`)" magit-discard)]
    ["Resolve conflicts (smerge)"
     ("u" "Keep upper/ours (hunk)" magit-smerge-keep-upper)
     ("l" "Keep lower/theirs (hunk)" magit-smerge-keep-lower)
     ("a" "Keep all (hunk)" magit-smerge-keep-all)])

  ;; -----------------------------------------------------------
  ;; Detached checkout
  ;; -----------------------------------------------------------
  ;; Git won't let you checkout a branch that's checked out in another
  ;; worktree. This command uses `git checkout --detach` to safely visit
  ;; the commit without attaching HEAD to the branch. In magit-log, it
  ;; picks up the commit at point via magit-read-other-branch-or-commit.

  (defun lg/magit-checkout-detached (rev)
    "Checkout REV in detached HEAD state.
Useful for visiting commits/branches checked out in other worktrees."
    (interactive (list (magit-read-other-branch-or-commit "Detached checkout")))
    (magit-run-git "checkout" "--detach" rev))

  ;; -----------------------------------------------------------
  ;; Diff
  ;; -----------------------------------------------------------

  (defun lg/magit-wrap-lines ()
    "Disable `truncate-lines' in the current buffer."
    (setq truncate-lines nil))

  ;; -----------------------------------------------------------
  ;; Keybindings, hooks, advice & transient suffixes
  ;; -----------------------------------------------------------

  ;; Keybindings
  (map! :map magit-mode-map
        "K" #'(lambda () (interactive) (previous-line 10) (evil-scroll-line-up 10))
        "J" #'(lambda () (interactive) (next-line 10) (evil-scroll-line-down 10)))

  (map! :leader
        (:prefix "g"
         :desc "Magit status" "g" #'lg/magit-status-for-workspace))

  (map! :map magit-mode-map
        :n "q" #'lg/magit-back-to-status)

  ;; Hooks
  (add-hook 'magit-refresh-buffer-hook #'lg/worktree--refresh-cache)
  (add-hook 'magit-diff-mode-hook #'lg/magit-wrap-lines)

  ;; Advice
  (advice-add 'magit-format-ref-labels :filter-return #'lg/magit--worktree-ref-labels)
  (advice-add 'magit-status-setup-buffer :after #'lg/magit-cleanup-workspace-buffers)
  (advice-add 'magit-log-setup-buffer :before #'lg/magit-kill-stale-log-buffers)

  ;; Status sections
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules
                          'magit-insert-unpulled-from-upstream)
  (magit-add-section-hook 'magit-status-sections-hook
                          #'magit-insert-worktrees
                          'magit-insert-status-headers
                          t)   ; t = insert AFTER magit-insert-status-headers

  ;; Transient suffixes
  (transient-append-suffix 'magit-log "-A"
    '("=p" "First parent" "--first-parent" :level 1))
  (transient-append-suffix 'magit-log "b"
    '("l" "locals and refs" lg/magit-log-branches))
  (transient-append-suffix 'magit-log "b"
    '("c" "current and main" lg/magit-log-current-and-main))
  (transient-append-suffix 'magit-log "b"
    '("C" "current" magit-log-current))
  (transient-append-suffix 'magit-branch "b"
    '("B" "detached checkout" lg/magit-checkout-detached))
  (transient-replace-suffix 'magit-dispatch "x"
    '("x" "discard…" lg/magit-x-transient :transient transient--do-replace))
  (define-key magit-hunk-section-map (kbd "x") #'lg/magit-x-transient))

;; magit-delta rendering notes (for future reference):
;;
;; Delta completely replaces magit's diff text with its own ANSI-colored
;; output. xterm-color then converts those ANSI codes into font-lock-face
;; text properties on overlays (priority 0), setting BOTH :foreground and
;; :background. This means:
;;
;;   - custom-set-faces! for magit-diff-added, magit-diff-added-highlight,
;;     etc. has NO effect — those faces are never consulted for diff content.
;;   - All color tuning must go through gitconfig [delta] section.
;;
;; To distinguish focused vs unfocused hunks, we attempted adding priority-5
;; overlays via magit-section-highlight-hook that override only :background.
;; This did not work in practice — unclear whether the hooks fired correctly
;; or whether something else interfered. A future attempt should verify:
;;   1. That magit-section-highlight-hook fires at all with magit-delta active
;;      (add a (message ...) to confirm).
;;   2. That (magit-section-match 'hunk section) matches correctly.
;;   3. Whether delta itself has a --focused-hunk style option.
(use-package! magit-delta
  :after magit
  :hook (magit-mode . magit-delta-mode)
  :config
  ;; Fully control the delta invocation so gitconfig changes never affect magit.
  ;; --no-gitconfig ignores ~/.gitconfig [delta] entirely.
  ;; --color-only is required: tells delta not to restructure the diff text,
  ;;   only add ANSI color — magit parses the diff structure itself.
  (setq magit-delta-delta-args
        `("--no-gitconfig"
          "--color-only"
          "--true-color" ,(if xterm-color--support-truecolor "always" "never")
          "--max-line-distance" "0.6"
          ;; "--syntax-theme" "Dracula"
          "--plus-style"      "syntax #1f4a28"       ; line bg: readable green
          "--minus-style"     "syntax #4a1f1f"        ; line bg: readable red
          "--plus-emph-style" "syntax #2e6e3e bold"  ; word bg: brighter green
          "--minus-emph-style" "syntax #6e2e2e bold")) ; word bg: brighter red

  )
