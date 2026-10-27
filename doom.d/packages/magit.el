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

  (defvar lg/worktree--porcelain-full-list-cache nil
    "Per-refresh cache of `lg/worktree--porcelain-full-list''s result.

Populated by whichever of the Worktrees section or
`lg/worktree--refresh-cache' runs first in a `magit-status' refresh cycle,
reused by the other, and cleared by `lg/worktree--refresh-cache' (the
cycle's last consumer, via `magit-refresh-buffer-hook') so the next
refresh recomputes fresh. Without this, both call sites shelled out to
`git worktree list --porcelain' separately every refresh (confirmed via a
per-call git log during profiling: ~0.04s of pure duplication).")

  (defun lg/magit--worktree-path-branches ()
    "Return cached (PATH . BRANCH) pairs for Magit's current refresh.
PATH has no trailing slash; BRANCH is nil for detached or bare worktrees.
Keep this separate from the tab-bar's `lg/worktree--porcelain-list',
which reads Git independently of Magit's refresh-scoped cache."
    (mapcar (pcase-lambda (`(,path ,branch ,_commit ,_bare)) (cons path branch))
            (lg/worktree--porcelain-full-list)))

  (defun lg/worktree--refresh-cache ()
    "Rebuild the worktree branch cache for the current repo."
    (unwind-protect
        (when-let ((top (magit-toplevel)))
          (let* ((current-top (expand-file-name top))
                 (all (lg/magit--worktree-path-branches))
                 (other-branches
                  (delq nil
                        (mapcar (lambda (wt)
                                  (when (and (not (string= (expand-file-name (car wt))
                                                           current-top))
                                             (cdr wt))   ; BRANCH element
                                    (cdr wt)))
                                all))))
            (puthash current-top other-branches lg/worktree--branch-cache)))
      ;; This hook fires after section insertion (see
      ;; `lg/magit--worktree-ref-labels'), so it's always the last consumer
      ;; of `lg/worktree--porcelain-full-list-cache' in a refresh cycle --
      ;; clear it here so the next refresh recomputes fresh rather than
      ;; serving stale data.
      (setq lg/worktree--porcelain-full-list-cache nil)))

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
  ;; Ref-label tag-verify batching: avoid N `rev-parse --verify
  ;; refs/tags/*' subprocess calls per log/status render
  ;; -----------------------------------------------------------
  ;; For every distinct ref name magit needs to decorate (branch/tag/HEAD
  ;; labels), `magit-format-ref-labels' calls `magit-rev-verify' on
  ;; "refs/tags/<name>" to check whether that name could also resolve as a
  ;; tag, purely for correct label/face rendering. Profiled via `elp' on
  ;; `magit-log' with a multi-branch/multi-commit view: 130 calls, ~1.78s —
  ;; ~75% of total render time, the single largest cost by far. Batch it:
  ;; one `git for-each-ref refs/tags' call per refresh, checked in Elisp
  ;; instead of N subprocess calls. Non-tag `magit-rev-verify' calls are
  ;; untouched.

  (defvar lg/ref-verify-tags-cache nil
    "Per-refresh cache: tag refname -> object hash. Populated lazily by
`lg/ref-verify-tags-table', cleared on `magit-refresh-buffer-hook' so
new/renamed tags are always picked up fresh next refresh.")

  (defun lg/ref-verify-tags-table ()
    "Hash table of all refs/tags/* -> object hash, for one refresh cycle."
    (or lg/ref-verify-tags-cache
        (setq lg/ref-verify-tags-cache
              (let ((table (make-hash-table :test #'equal)))
                (dolist (line (magit-git-lines "for-each-ref"
                                                "--format=%(refname) %(objectname)"
                                                "refs/tags"))
                  (let ((sp (string-search " " line)))
                    (when sp
                      (puthash (substring line 0 sp) (substring line (1+ sp)) table))))
                table))))

  (define-advice magit-rev-verify (:around (orig rev) lg/batch-tag-verify)
    "Batch refs/tags/* lookups via `lg/ref-verify-tags-table' instead of
one `rev-parse --verify' subprocess per ref."
    (if (string-prefix-p "refs/tags/" rev)
        (gethash rev (lg/ref-verify-tags-table))
      (funcall orig rev)))

  ;; -----------------------------------------------------------
  ;; Worktrees section: avoid magit-list-worktrees' per-worktree subprocess
  ;; -----------------------------------------------------------
  ;; Stock `magit-insert-worktrees' goes through `magit-list-worktrees',
  ;; which calls `magit-toplevel' (a `git rev-parse --show-toplevel'
  ;; subprocess) once per worktree just to canonicalize each path -- on a
  ;; repo with N worktrees that's N extra subprocess spawns for
  ;; information already present verbatim in `git worktree list
  ;; --porcelain' output. Profiled via `elp' on a real multi-worktree repo:
  ;; this section alone was ~28% of total `magit-status' time, almost all
  ;; of it those redundant `magit-toplevel' calls. Replace it with a
  ;; version built on a single porcelain call instead.

  (defun lg/worktree--porcelain-full-list ()
    "Return a list of (PATH BRANCH COMMIT BARE) for the current repo's
worktrees, via a single `git worktree list --porcelain' subprocess call
per `magit-status' refresh (see `lg/worktree--porcelain-full-list-cache').

Unlike `lg/magit--worktree-path-branches' (path+branch only, used by
log ref-label decoration), this also captures the HEAD commit and bare
flag needed to render `lg/magit-insert-worktrees' faithfully,
without adding a second subprocess call."
    (or lg/worktree--porcelain-full-list-cache
        (setq lg/worktree--porcelain-full-list-cache
              (let (worktrees path branch commit bare)
                (dolist (line (magit-git-lines "worktree" "list" "--porcelain"))
                  (cond
                   ((string-prefix-p "worktree " line)
                    (when path (push (list path branch commit bare) worktrees))
                    (setq path (directory-file-name (substring line 9))
                          branch nil commit nil bare nil))
                   ((string-prefix-p "HEAD " line)
                    (setq commit (substring line 5)))
                   ((string-prefix-p "branch refs/heads/" line)
                    (setq branch (substring line 18)))
                   ((string= line "bare")
                    (setq bare t))))
                (when path (push (list path branch commit bare) worktrees))
                (nreverse worktrees)))))

  (defun lg/magit-insert-worktrees ()
    "Replacement for stock `magit-insert-worktrees': same \"Worktrees\"
section (one line per worktree, showing its branch/bare-status/detached
commit and path), but built from a single subprocess call
\(`lg/worktree--porcelain-full-list') instead of N+1.

Known gap vs. the stock section: RET on a row here doesn't carry stock
magit's worktree-visit keybinding, since that's wired to the section type
stock `magit-insert-worktrees' uses internally and isn't part of magit's
public API. Use `lg/worktree-switch'/the worktree tab bar to jump to a
worktree instead."
    (let ((worktrees (lg/worktree--porcelain-full-list)))
      (when (length> worktrees 1)
        (magit-insert-section (worktrees)
          (magit-insert-heading t "Worktrees")
          (let* ((heads
                  (mapcar (pcase-lambda (`(,_path ,branch ,commit ,bare))
                            (cond
                             (branch (propertize branch 'font-lock-face 'magit-branch-local))
                             (bare   (propertize "(bare)" 'font-lock-face 'font-lock-comment-face))
                             (commit (propertize (substring commit 0 (min 7 (length commit)))
                                                  'font-lock-face 'magit-hash))
                             (t "")))
                          worktrees))
                 (align (1+ (apply #'max (mapcar #'length heads)))))
            (cl-mapc
             (lambda (head wt)
               (let ((path (car wt)))
                 (magit-insert-section (worktree path)
                   (insert (format (format "%%-%ds " align) head))
                   (insert (propertize (abbreviate-file-name path) 'font-lock-face 'magit-filename))
                   (insert "\n"))))
             heads worktrees))
          (insert "\n")))))

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
    (interactive (magit-log-arguments))
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
    (interactive (magit-log-arguments))
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
  ;; Push & create PR (one workflow). Browse-half in forge-config.el.
  ;; -----------------------------------------------------------
  (declare-function forge-create-pullreq "forge-commands")
  (defvar lg/forge--pending-pr-browse) ; defined in forge-config.el

  (defun lg/magit--read-push-remote (branch)
    "Return BRANCH's configured push/upstream remote, or prompt for one."
    (let* ((remotes (magit-list-remotes))
           (configured (or (magit-get-push-remote branch)
                           (magit-get-remote branch))))
      (cond ((null remotes) (user-error "No remotes configured"))
            ((member configured remotes) configured)
            (t (magit-read-remote "Push to remote")))))

  (defun lg/magit--read-pr-target-branch (remote source-branch)
    "Prompt for the PR base for SOURCE-BRANCH on REMOTE, defaulting to HEAD."
    (let* ((branches (magit-list-remote-branch-names remote))
           (head (magit-git-string
                  "symbolic-ref" "--short"
                  (format "refs/remotes/%s/HEAD" remote)))
           (default (or (and (member head branches) head)
                        (seq-find (lambda (branch) (member branch branches))
                                  (mapcar (lambda (name)
                                            (format "%s/%s" remote name))
                                          '("main" "master" "dev"))))))
      (magit-completing-read
       (format "PR base branch (merge %s/%s into)" remote source-branch)
       branches nil t nil 'magit-revision-history default)))

  (defun lg/magit-push-and-create-pr (branch)
    "Push local BRANCH, then create and visit a PR.
After `C-c C-c' in the post buffer, opens the new PR in your browser
via the hook in forge-config.el."
    (interactive (list (magit-read-local-branch "PR source branch to push")))
    (require 'forge)
    (let* ((remote (lg/magit--read-push-remote branch))
           (target (lg/magit--read-pr-target-branch remote branch))
           ;; forge-create-pullreq expects source qualified as
           ;; <remote>/<branch> — unqualified would split to (\".\" . branch)
           ;; in magit-split-branch-name and break repo resolution at submit.
           (source (format "%s/%s" remote branch))
           (refspec (concat branch ":" branch)))
      (message "Pushing %s to %s..." branch remote)
      ;; magit-run-git only displays the error from a failed git invocation;
      ;; it doesn't signal. Raise so we abort before opening the PR buffer.
      (let ((magit-process-raise-error t))
        (magit-run-git "push" "-u" remote refspec))
      (setq lg/forge--pending-pr-browse t)
      (forge-create-pullreq source target)))

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
  (add-hook 'magit-refresh-buffer-hook
            (lambda () (setq lg/ref-verify-tags-cache nil)))
  (add-hook 'magit-diff-mode-hook #'lg/magit-wrap-lines)

  ;; Advice
  (advice-add 'magit-format-ref-labels :filter-return #'lg/magit--worktree-ref-labels)
  (advice-add 'magit-status-setup-buffer :after #'lg/magit-cleanup-workspace-buffers)
  (advice-add 'magit-log-setup-buffer :before #'lg/magit-kill-stale-log-buffers)

  ;; Status sections
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules
                          'magit-insert-unpulled-from-upstream)
  ;; Replace stock `magit-insert-worktrees' (magit-list-worktrees' per-worktree
  ;; `magit-toplevel' subprocess fan-out) with `lg/magit-insert-worktrees' —
  ;; `magit-add-section-hook' only adds, so the stock section must be removed
  ;; explicitly or both run.
  (remove-hook 'magit-status-sections-hook 'magit-insert-worktrees)
  (magit-add-section-hook 'magit-status-sections-hook
                          #'lg/magit-insert-worktrees
                          'magit-insert-status-headers
                          t)   ; t = insert AFTER magit-insert-status-headers

  ;; Drop optional status-buffer sections/headers that were measured (via a
  ;; per-call git subprocess log during profiling) to cost real time for
  ;; info not otherwise used day-to-day. To bring one back, delete its
  ;; remove-hook line below.
  (remove-hook 'magit-status-sections-hook 'magit-insert-stashes)     ; ~0.036s (2 git calls)
  ;; `magit-insert-tags-header' lives in `magit-status-headers-hook', the
  ;; nested hook `magit-insert-status-headers' runs to build the compound
  ;; header block — not in `magit-status-sections-hook' itself.
  (remove-hook 'magit-status-headers-hook 'magit-insert-tags-header)  ; ~0.096s (6 git calls)

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
  ;; Anchor in Magit's unconditional "Push" group; the current-branch group
  ;; (which contains `magit-push-current-to-pushremote') is hidden at detached HEAD.
  (transient-append-suffix 'magit-push 'magit-push-other
    '("R" "Push & open PR" lg/magit-push-and-create-pr))
  (transient-replace-suffix 'magit-dispatch "x"
    '("x" "discard…" lg/magit-x-transient :transient transient--do-replace))
  (define-key magit-hunk-section-map (kbd "x") #'lg/magit-x-transient))

;; magit-delta rendering:
;;
;; Keep Delta's syntax foreground/background as face TEXT properties.
;; Upstream xterm-color syntax overlays made navigation and redisplay slow
;; on large reviews, even after caching their computation.  Magit paints
;; font-lock-face, so syntax faces deliberately use the separate face property.
;; Tune syntax colors through magit-delta-delta-args below, not Magit faces
;; or gitconfig (our invocation uses --no-gitconfig).
;;
;; Only line-ending background-extension overlays remain.  Never put those
;; backgrounds over the whole line: overlays override syntax text properties
;; and would mask Delta's brighter word-emphasis backgrounds.
;; -----------------------------------------------------------
;; magit-delta context wrapper
;; -----------------------------------------------------------
;; Delta highlights each hunk in isolation via syntect, which is stateless
;; across hunks.  When a hunk starts mid-multi-line construct (e.g. inside a
;; Python docstring) syntect lands in the wrong state and the whole hunk is
;; rendered as a string.  We work around this by prepending N synthetic
;; context lines (read from HEAD or the working tree) to every hunk before
;; delta sees the diff, then stripping those lines after delta colorizes —
;; all in the same buffer call that magit-delta already uses.
(defvar lg/magit-delta-extra-context-lines 30
  "Synthetic preceding lines fed to delta per hunk for syntect state.
Read from HEAD (fallback: working tree).  Stripped before xterm-color runs.")

(defvar lg/magit-delta-plus-bg  "#1f4a28"
  "Background colour for added (`+') lines.  Shared by the delta
`--plus-style' arg and the `:extend t' overlay applied post-render so
the colour fills past end-of-line.")

(defvar lg/magit-delta-minus-bg "#4a1f1f"
  "Background colour for removed (`-') lines.  See `lg/magit-delta-plus-bg'.")

(load! "../magit-delta-cache")

(defvar lg/magit-delta--prefix-cache nil
  "Refresh-scoped cache of file lines and repository HEAD identity.")

(defun lg/magit-delta--with-refresh-cache (orig-fn &rest args)
  "Share context reads and discard old background overlays before refreshing.
Explicit cleanup also repairs legacy non-evaporating overlays left in
already-open buffers.  Do not touch Magit sections or review comments."
  (save-restriction
    (widen)
    (remove-overlays (point-min) (point-max) 'lg-magit-delta-line-bg t))
  (let ((lg/magit-delta--prefix-cache (make-hash-table :test #'equal)))
    (apply orig-fn args)))

(defun lg/magit-delta--ansi-strip (s)
  "Strip ANSI CSI sequences from S."
  (replace-regexp-in-string "\e\\[[^m]*m" "" s))

(defun lg/magit-delta--file-lines (path)
  "Return all lines of PATH from HEAD, falling back to the working tree.
Caches the result in `lg/magit-delta--prefix-cache' when that var is bound
to a hash table; subsequent calls for the same PATH avoid the git subprocess."
  (let ((cached (if lg/magit-delta--prefix-cache
                    (gethash path lg/magit-delta--prefix-cache 'absent)
                  'absent)))
    (if (not (eq cached 'absent))
        cached
      (let* ((identity
              (or (and lg/magit-delta--prefix-cache
                       (gethash :identity lg/magit-delta--prefix-cache))
                  (let ((id (list (file-truename default-directory)
                                  (magit-rev-parse "HEAD"))))
                    (when lg/magit-delta--prefix-cache
                      (puthash :identity id lg/magit-delta--prefix-cache))
                    id)))
             (revision (cadr identity))
             (key (list 'source identity path))
             (entry (lg/magit-delta--cache-get key))
             (head-lines
              (if entry
                  (cdr entry)
                ;; Use the resolved immutable revision, not a moving HEAD.
                (let ((lines
                       (and revision
                            (with-temp-buffer
                              (when (eq 0 (magit-git-insert
                                           "show" (concat revision ":" path)))
                                ;; magit-git-lines discards blank lines,
                                ;; which corrupts source line coordinates.
                                (split-string (buffer-string) "\n"))))))
                  (when lines
                    (lg/magit-delta--cache-put
                     key lines (cl-loop for line in lines
                                        sum (1+ (string-bytes line)))))
                  lines)))
             ;; Never persist working-tree fallback: it can change without
             ;; HEAD moving.  Augmented diff contents key the render cache.
             (fresh (or head-lines
                        (let ((abs (expand-file-name path (magit-toplevel))))
                          (and (file-readable-p abs)
                               (with-temp-buffer
                                 (insert-file-contents abs)
                                 (split-string (buffer-string) "\n")))))))
        (when lg/magit-delta--prefix-cache
          (puthash path fresh lg/magit-delta--prefix-cache))
        fresh))))

(defun lg/magit-delta--read-prefix-lines (path end-line max-lines)
  "Up to MAX-LINES lines of PATH, immediately preceding line END-LINE (1-indexed).
PATH is relative to the magit toplevel.  Returns a list of strings (no
newlines) or nil if nothing readable."
  (when (and path (> end-line 1) (> max-lines 0))
    (let ((lines (lg/magit-delta--file-lines path)))
      (when lines
        (let* ((want (min max-lines (1- end-line)))
               (start (- end-line want))
               (len (length lines))
               (s (max 0 (1- start)))
               (e (min len (1- end-line))))
          (and (< s e) (cl-subseq lines s e)))))))

(defun lg/magit-delta--augment-buffer ()
  "Prepend syntect-stabilizing context to each hunk in the current buffer.
Returns an alist of (HUNK-INDEX EXTRA-COUNT ORIG-HEADER) for use by
`lg/magit-delta--strip-augmentations'."
  (let ((current-path nil)
        (saw-minus nil)            ; previous line was a `--- ...` header
        (idx -1)
        (augs '()))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((advance 1)
              (next-saw-minus nil))
          (cond
           ((looking-at "^---\\(?: a/\\| \\)")
            ;; New file boundary; clear the previous path so a malformed
            ;; or skipped `+++` line cannot leak the wrong path into later hunks.
            (setq current-path nil
                  next-saw-minus t))
           ;; Real file header always follows a `---` line.  Without that
           ;; guard, source lines like "+++ foo" inside a hunk would match.
           ((and saw-minus
                 (looking-at "^\\+\\+\\+ \\(?:b/\\)?\\(.+?\\)\\s-*$"))
            (let ((p (match-string-no-properties 1)))
              (setq current-path (and p (not (string= p "/dev/null")) p))))
           ((looking-at "^@@ -\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? \\+\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? @@\\(.*\\)$")
            (setq idx (1+ idx))
            (let* ((orig-header (match-string-no-properties 0))
                   (old-start (string-to-number (match-string 1)))
                   (old-len   (if (match-string 2) (string-to-number (match-string 2)) 1))
                   (new-start (string-to-number (match-string 3)))
                   (new-len   (if (match-string 4) (string-to-number (match-string 4)) 1))
                   (rest      (match-string-no-properties 5))
                   ;; Skip pure-insertion hunks (old-len = 0): their old-start
                   ;; is "the line AFTER which we insert", which doesn't compose
                   ;; cleanly with our context-prepending formula.  Rare.
                   (extra (and current-path
                               (> old-len 0)
                               (min lg/magit-delta-extra-context-lines (1- new-start))))
                   (prefix (and extra (> extra 0)
                                (lg/magit-delta--read-prefix-lines
                                 current-path new-start extra))))
              (when (and prefix (= (length prefix) extra))
                (let ((new-header
                       (format "@@ -%d,%d +%d,%d @@%s"
                               (- old-start extra) (+ old-len extra)
                               (- new-start extra) (+ new-len extra)
                               rest)))
                  (delete-region (line-beginning-position) (line-end-position))
                  (insert new-header)
                  (forward-line 1)
                  (dolist (ln prefix)
                    (insert " " ln "\n"))
                  (push (list idx extra orig-header) augs)
                  ;; point already past the inserted block
                  (setq advance 0))))))
          (setq saw-minus next-saw-minus)
          (forward-line advance))))
    (nreverse augs)))

(defun lg/magit-delta--strip-augmentations (augs)
  "Undo the augmentations recorded by `lg/magit-delta--augment-buffer'.
AUGS is a list of (HUNK-INDEX EXTRA-COUNT ORIG-HEADER).  Tolerates ANSI
escape sequences inserted by delta around the @@ header."
  (when augs
    (let ((lookup (make-hash-table)))
      (dolist (a augs) (puthash (nth 0 a) (cdr a) lookup))
      (save-excursion
        (goto-char (point-min))
        (let ((idx -1))
          (while (not (eobp))
            (let* ((line (buffer-substring-no-properties
                          (line-beginning-position) (line-end-position)))
                   (stripped (lg/magit-delta--ansi-strip line))
                   (is-header (string-prefix-p "@@ " stripped))
                   (advance 1))
              (when is-header
                (setq idx (1+ idx))
                (when-let* ((entry (gethash idx lookup))
                            (extra (nth 0 entry))
                            (orig-header (nth 1 entry)))
                  (delete-region (line-beginning-position) (line-end-position))
                  (insert orig-header)
                  (forward-line 1)
                  (let ((start (point)))
                    (forward-line extra)
                    (delete-region start (point)))
                  (setq advance 0)))
              (forward-line advance))))))))

;; Fill gaps in Delta's +/- line backgrounds without masking its brighter
;; word-emphasis backgrounds.  Text faces provide the base background at
;; lower precedence than syntax faces; one overlay at the newline extends
;; that base color to the window edge.  A full-line overlay would override
;; syntax text properties regardless of its priority.
(defun lg/magit-delta--extend-line-backgrounds ()
  "Fill +/- line backgrounds and extend them with one line-ending overlay."
  (save-excursion
    (goto-char (point-min))
    (let ((in-hunk nil))
      (while (not (eobp))
        (cond
         ((looking-at "^diff ") (setq in-hunk nil))
         ((looking-at "^@@ ")   (setq in-hunk t))
         ((and in-hunk
               (looking-at "^[+-]")
               (not (looking-at "^\\(\\+\\+\\+\\|---\\) ")))
          (let* ((plus (eq (char-after) ?+))
                 (bg   (if plus lg/magit-delta-plus-bg lg/magit-delta-minus-bg))
                 (beg  (line-beginning-position))
                 (eol  (line-end-position))
                 (end  (min (point-max) (1+ eol))))
            (add-face-text-property beg end `(:background ,bg :extend t) t)
            (when (< eol end)
              (let ((ov (make-overlay eol end)))
                (overlay-put ov 'face `(:background ,bg :extend t))
                (overlay-put ov 'priority -1)
                (overlay-put ov 'lg-magit-delta-line-bg t)
                (overlay-put ov 'evaporate t))))))
        (forward-line 1)))))

(defun lg/magit-delta--file-augmentations (parts augs)
  "Translate global AUGS hunk indices into per-file indices for PARTS."
  (let ((index 0))
    (mapcar
     (lambda (part)
       (let ((first index) (pos 0) local)
         (while (string-match "^@@ " part pos)
           (setq index (1+ index) pos (match-end 0)))
         (while (and augs (< (caar augs) index))
           (let ((entry (pop augs)))
             (push (cons (- (car entry) first) (cdr entry)) local)))
         (nreverse local)))
     parts)))

(defun lg/magit-delta--conversion-settings ()
  "Capture settings affecting ANSI conversion in the current wash buffer."
  (cl-loop for var in '(font-lock-mode xterm-color-render
                        xterm-color-use-bold-for-bright
                        xterm-color-names xterm-color-names-bright)
           when (boundp var) collect (cons var (symbol-value var))))

(defun lg/magit-delta--syntax-text-faces ()
  "Normalize xterm colors to face properties that survive Magit painting.
Magit rewrites font-lock-face during washing and section highlighting;
keeping ANSI colors there would silently discard syntax highlighting."
  (let ((pos (point-min)))
    (while (< pos (point-max))
      (let ((next (next-single-property-change pos 'font-lock-face nil (point-max)))
            (face (get-text-property pos 'font-lock-face)))
        (when face (put-text-property pos next 'face face))
        (setq pos next))))
  (remove-text-properties (point-min) (point-max)
                          '(font-lock-face nil xterm-color nil)))

(defun lg/magit-delta--decode-file (ansi augs settings)
  "Decode ANSI into syntax text faces and relative background-overlay spans.
SETTINGS comes from the original wash buffer.  No syntax overlays are
created.  Temporary background overlays are destroyed after capture."
  (with-temp-buffer
    (dolist (setting settings)
      (set (make-local-variable (car setting)) (cdr setting)))
    (insert ansi)
    (lg/magit-delta--strip-augmentations augs)
    (xterm-color-colorize-buffer)
    (lg/magit-delta--syntax-text-faces)
    (when magit-delta-hide-plus-minus-markers
      (magit-delta-hide-plus-minus-markers))
    (lg/magit-delta--extend-line-backgrounds)
    (lg/magit-delta--snapshot-create
     :text (buffer-string)
     :spans (mapcar
             (lambda (ov)
               (vector (1- (overlay-start ov)) (1- (overlay-end ov))
                       ;; Only our coloring properties belong in the cache.
                       ;; Never retain arbitrary third-party buffer objects.
                       (cl-loop for prop in '(face font-lock-face xterm-color
                                             evaporate priority lg-magit-delta-line-bg)
                                for value = (overlay-get ov prop)
                                when value append (list prop (copy-tree value)))))
             (let ((lists (overlay-lists)))
               (append (car lists) (cdr lists)))))))

(defun lg/magit-delta--insert-snapshot (snapshot)
  "Insert SNAPSHOT's syntax text faces and recreate only background overlays."
  (let ((start (point)))
    (insert (lg/magit-delta--snapshot-text snapshot))
    (dolist (span (lg/magit-delta--snapshot-spans snapshot))
      (when (< (aref span 0) (aref span 1))
        (let ((ov (make-overlay (+ start (aref span 0))
                                (+ start (aref span 1)))))
          (cl-loop for (prop value) on (aref span 2) by #'cddr
                   do (overlay-put ov prop value))
          ;; Enforce lifetime on insertion, not just in the decoder: an
          ;; existing cache entry may predate this flag or explicitly lack it.
          (when (overlay-get ov 'lg-magit-delta-line-bg)
            (overlay-put ov 'evaporate t)))))))

(defun lg/magit-delta-call-delta-with-context (orig-fn &rest args)
  "Color diffs using cached stripped text and relative color spans per file.
Only changed files are sent through Delta, ANSI conversion, context
stripping and line-background scanning.  Syntax colors use text faces;
only background-extension overlays are recreated before section parsing."
  (if args
      (apply orig-fn args)
    (let ((lg/magit-delta--prefix-cache
           (or lg/magit-delta--prefix-cache (make-hash-table :test #'equal)))
          (original (buffer-string))
          (settings (lg/magit-delta--conversion-settings)))
      (condition-case err
          (let* ((augs (lg/magit-delta--augment-buffer))
                 (input (buffer-substring-no-properties (point-min) (point-max)))
                 (parts (lg/magit-delta--file-parts input))
                 (local-augs (lg/magit-delta--file-augmentations parts augs))
                 (snapshots
                  (lg/magit-delta--render-files
                   input magit-delta-delta-executable
                   (magit-delta--make-delta-args)
                   (lambda (ansi _input index)
                     (lg/magit-delta--decode-file
                      ansi (if index (nth index local-augs) augs) settings))
                   (list 'syntax-text-v1 settings magit-delta-hide-plus-minus-markers
                         lg/magit-delta-plus-bg lg/magit-delta-minus-bg)
                   parts local-augs)))
            (delete-region (point-min) (point-max))
            (goto-char (point-min))
            (mapc #'lg/magit-delta--insert-snapshot snapshots)
            (goto-char (point-min)))
        ((error quit)
         ;; A failed process/decoder must not leave synthetic context behind.
         (delete-region (point-min) (point-max))
         (insert original)
         (signal (car err) (cdr err)))))))

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
          "--plus-style"      ,(concat "syntax " lg/magit-delta-plus-bg)   ; line bg: readable green
          "--minus-style"     ,(concat "syntax " lg/magit-delta-minus-bg)  ; line bg: readable red
          "--plus-emph-style" "syntax #2e6e3e bold"   ; word bg: brighter green
          "--minus-emph-style" "syntax #6e2e2e bold")) ; word bg: brighter red

  (advice-add 'magit-refresh-buffer
              :around #'lg/magit-delta--with-refresh-cache)
  (advice-add 'magit-delta-call-delta-and-convert-ansi-escape-sequences
              :around #'lg/magit-delta-call-delta-with-context))
