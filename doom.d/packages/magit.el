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
  (declare-function forge-get-repository "forge-core")
  (defvar lg/forge--pending-pr-browse) ; defined in forge-config.el

  (defun lg/magit--local-branch-at-point ()
    "Return branch at point only if it's a local branch, else nil."
    (let ((b (magit-branch-at-point)))
      (and b (magit-local-branch-p b) b)))

  (defun lg/magit--push-target-branch ()
    "Branch to push: local branch at point (in log/refs), else current."
    (or (and (derived-mode-p 'magit-log-mode 'magit-refs-mode)
             (lg/magit--local-branch-at-point))
        (magit-get-current-branch)
        (user-error "No branch to push (detached HEAD?)")))

  (defun lg/magit--read-push-remote ()
    "Return sole remote, or prompt when there are multiple."
    (let ((remotes (magit-list-remotes)))
      (cond ((null remotes) (user-error "No remotes configured"))
            ((null (cdr remotes)) (car remotes))
            (t (magit-read-remote "Push to remote")))))

  (defun lg/magit--default-pr-target-branch (remote)
    "Determine PR target, qualified as REMOTE/<branch>."
    (let ((repo (ignore-errors (forge-get-repository :tracked))))
      (or (and repo
               (ignore-errors (oref repo default-branch))
               (format "%s/%s" remote (oref repo default-branch)))
          (let ((remote-branches (magit-list-remote-branch-names remote)))
            (cl-find-if (lambda (b) (member b remote-branches))
                        (list (concat remote "/main")
                              (concat remote "/master")
                              (concat remote "/dev"))))
          (magit-read-remote-branch "Target branch" remote))))

  (defun lg/magit-push-and-create-pr ()
    "Push branch (at point or current), then create & visit a PR.
After `C-c C-c' in the post buffer, opens the new PR in your browser
via the hook in forge-config.el."
    (interactive)
    (require 'forge)
    (let* ((branch (lg/magit--push-target-branch))
           (remote (lg/magit--read-push-remote))
           (target (lg/magit--default-pr-target-branch remote))
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
  (transient-append-suffix 'magit-push "p"
    '("R" "Push & open PR" lg/magit-push-and-create-pr))
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

(defvar lg/magit-delta--prefix-cache nil
  "Per-invocation cache: path -> list of file lines (or nil sentinel).
Bound to a fresh hash table by `lg/magit-delta-call-delta-with-context'
so multiple hunks of the same file share a single git subprocess call.")

(defun lg/magit-delta--ansi-strip (s)
  "Strip ANSI CSI sequences from S."
  (replace-regexp-in-string "\e\\[[^m]*m" "" s))

(defun lg/magit-delta--file-lines (path)
  "Return all lines of PATH from HEAD, falling back to the working tree.
Caches the result in `lg/magit-delta--prefix-cache' when that var is bound
to a hash table; subsequent calls for the same PATH avoid the git subprocess."
  (let ((cached (and lg/magit-delta--prefix-cache
                     (gethash path lg/magit-delta--prefix-cache 'absent))))
    (if (not (eq cached 'absent))
        cached
      (let ((fresh (or (ignore-errors
                         (magit-git-lines "show" (concat "HEAD:" path)))
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

;; Delta colours each `+'/`-' line by attaching ANSI background to the text
;; only; nothing covers BOL..text-start or text-end..window-edge, so the
;; coloured stripe ends mid-line.  Magit's own diff faces (which carry
;; `:extend t') aren't consulted once delta replaces the diff text.  We
;; emulate `:extend t' by adding one low-priority full-line overlay per
;; `+'/`-' content line.  Priority -1 keeps delta's per-char overlays
;; (including the brighter emph-style) visible on top within the text.
(defun lg/magit-delta--extend-line-backgrounds ()
  "Add :extend t overlays so +/- diff lines fill to window edge."
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
                 (end  (min (point-max) (1+ (line-end-position))))
                 (ov   (make-overlay beg end)))
            (overlay-put ov 'face `(:background ,bg :extend t))
            (overlay-put ov 'priority -1)
            (overlay-put ov 'lg-magit-delta-line-bg t))))
        (forward-line 1)))))

(defun lg/magit-delta-call-delta-with-context (orig-fn &rest args)
  "Around-advice for `magit-delta-call-delta-and-convert-ansi-escape-sequences'.
Augments hunks with extra preceding context so delta's syntect sees the
right syntactic state, then strips that context from delta's output.
A per-invocation cache shares HEAD reads across hunks of the same file."
  (let ((lg/magit-delta--prefix-cache (make-hash-table :test 'equal)))
    (let ((augs (lg/magit-delta--augment-buffer)))
      (unwind-protect
          (apply orig-fn args)
        (lg/magit-delta--strip-augmentations augs)
        (lg/magit-delta--extend-line-backgrounds)))))

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

  (advice-add 'magit-delta-call-delta-and-convert-ansi-escape-sequences
              :around #'lg/magit-delta-call-delta-with-context))
