;;; doom.d/packages/review.el -*- lexical-binding: t; -*-

;; review: Magit-native branch review using isolated git worktrees.
;;
;; Instead of a bespoke diff renderer, this system opens a real magit-status
;; buffer in a dedicated worktree.  The git staging area acts as the "reviewed"
;; tracker — native s/u/x bindings work at full magit speed with no redraw.
;;
;; Usage:
;;   SPC g r r  Start a review — pick branch + base.
;;              Creates a worktree, soft-resets to base, unstages everything,
;;              opens magit-status.  Main working tree is never touched.
;;   s / u / x  Stage / unstage / discard hunks (native magit).
;;   C          Add an inline comment at point (popup buffer).
;;              C-c C-c saves; C-c C-k cancels.
;;   D          Delete the comment on the current line.
;;   SPC g r p  Publish — restore branch, write REVIEW.md to repo root (untracked),
;;              remove worktree, refresh main magit-status.
;;   SPC g r k  Cancel — restore branch, remove worktree.
;;   SPC g r c  Resume — jump to (or re-open) the review magit-status buffer.
;;
;; Architecture:
;;   Sessions keyed by git common dir (stable across worktrees).
;;   Worktrees created at .hatchery/review-worktrees/<branch-slug>/.
;;   Comments stored in session plist; overlays re-applied via
;;   magit-post-refresh-hook using file+hunk-header as a stable key.

(after! magit

  ;; ==========================================================================
  ;; State
  ;; ==========================================================================

  (defvar lg/review--sessions (make-hash-table :test #'equal)
    "Hash table: git-common-dir → session plist.
Keys: :branch :base :old-sha :worktree-path :repo-top :comments :status-buf")

  (defvar lg/review--comment-context nil
    "Plist for the review comment currently being edited.
Keys: :repo-key :branch :file :line :hunk-key :hunk-offset
      :diff-buf :diff-pos :diff-context")

  (defvar lg/review-instructions
    "You are responding to a code review. Treat this like a PR review response.

**Before writing any code:**
1. Read all comments.
2. For each comment, decide: implement directly, or raise for discussion.
3. Present your plan and any questions or pushbacks to the user.
4. Wait for agreement, then implement.

**Rules:**
- You MUST address every comment — none can be skipped.
- For clear, small instructions: implement directly if you agree.
- For questions or ambiguous suggestions (e.g. \"Should we do X?\"): surface them in step 3, do not assume intent.
- Push back on suggestions you think are wrong — explain your reasoning before declining.
- Do not make changes outside the scope of the review. Necessary side-effects are fine (e.g. updating imports after a rename).
- Preserve existing tests unless they are no longer relevant due to a change you are making. Removing a test MUST be discussed first."
    "Instructions for the review agent, prepended to every review file.")

  (defface lg/review-comment-face
    '((t :foreground "#f8f8f2" :background "#44475a" :extend t))
    "Face for inline review comment overlays in diff buffers.")

  ;; ==========================================================================
  ;; Helpers
  ;; ==========================================================================

  (defun lg/review--repo-key ()
    "Stable session key: absolute path to git common dir.
Same value from the main working tree and any of its linked worktrees."
    (expand-file-name
     (or (magit-git-string "rev-parse" "--git-common-dir") ".git")
     (magit-toplevel)))

  (defun lg/review--find-session ()
    "Return (KEY . SESSION) for the current repo, or nil.
Tries the computed repo-key first; falls back to matching any active
session whose repo-top is a parent of the current default-directory.
This handles path-normalisation differences between worktrees."
    (let* ((key     (ignore-errors (lg/review--repo-key)))
           (session (and key (gethash key lg/review--sessions))))
      (if session
          (cons key session)
        ;; Fallback: scan sessions, match by repo-top containment
        (let ((cur (expand-file-name default-directory)))
          (catch 'found
            (maphash (lambda (k s)
                       (let ((top (plist-get s :repo-top)))
                         (when (and top (string-prefix-p top cur))
                           (throw 'found (cons k s)))))
                     lg/review--sessions)
            nil)))))

  (defun lg/review--slug (ref)
    "Convert REF to a filesystem-safe slug (replace / and : with -)."
    (replace-regexp-in-string "[/:]" "-" ref))

  (defun lg/review--repo-dir (repo-top)
    "Return the worktree storage dir for REPO-TOP under ~/.review-worktrees/.
Mirrors lg/review--repo-dir's <name>-<hash> convention."
    (let* ((top  (expand-file-name repo-top))
           (name (file-name-nondirectory (directory-file-name top)))
           (hash (substring (md5 top) 0 6)))
      (expand-file-name (format "%s-%s" name hash) "~/.review-worktrees/")))

  (defun lg/review--worktree-path (repo-top branch)
    "Return the review worktree path for BRANCH (~/.review-worktrees/...)."
    (expand-file-name (lg/review--slug branch)
                      (lg/review--repo-dir repo-top)))

  (defun lg/review--run-git (dir &rest args)
    "Run git in DIR with ARGS.  Return exit code."
    (apply #'call-process "git" nil nil nil "-C" dir args))

  (defun lg/review--run-git-output (dir &rest args)
    "Run git in DIR with ARGS.  Return stdout as a string."
    (with-output-to-string
      (apply #'call-process "git" nil standard-output nil "-C" dir args)))

  (defun lg/review--branch-worktree-path (repo-top branch)
    "Return the worktree path where BRANCH is checked out, or nil."
    (let* ((raw (lg/review--run-git-output repo-top "worktree" "list" "--porcelain"))
           (blocks (split-string raw "\n\n" t))
           result)
      (dolist (block blocks result)
        (when (string-match (format "^branch refs/heads/%s$"
                                    (regexp-quote branch))
                            block)
          (when (string-match "^worktree \\(.+\\)$" block)
            (setq result (match-string 1 block)))))))

  (defvar lg/review--last-branch nil
    "The most recently selected branch for review, used as default next time.")

  (defun lg/review--base-ref ()
    "Detect the remote HEAD ref (e.g. origin/main)."
    (or (when-let ((ref (magit-git-string "symbolic-ref" "refs/remotes/origin/HEAD")))
          (string-remove-prefix "refs/remotes/" ref))
        (seq-find #'magit-rev-verify '("origin/main" "origin/master" "origin/dev"))
        (user-error "Cannot detect base branch — run: git remote set-head origin --auto")))

  (defun lg/review--read-branch ()
    "Prompt for a branch to review, defaulting to the last selection."
    (let* ((current (magit-get-current-branch))
           (candidates (magit-list-local-branch-names))
           (default (or lg/review--last-branch current))
           (prompt (if default
                       (format "Branch to review (default %s): " default)
                     "Branch to review: "))
           (choice (completing-read prompt candidates nil t nil nil default)))
      (setq lg/review--last-branch choice)
      choice))

  (defun lg/review--read-base-ref ()
    "Prompt for the base ref, defaulting to origin HEAD."
    (let* ((default (lg/review--base-ref))
           (prompt (format "Base branch (default %s): " default))
           (candidates (magit-list-branch-names))
           (choice (completing-read prompt candidates nil t nil nil default)))
      (if (string-empty-p choice) default choice)))

  ;; ==========================================================================
  ;; Diff-context helpers (used by lg/review-comment)
  ;; ==========================================================================

  (defun lg/review--hunk-line-at-point (section)
    "Return new-file line number at point within hunk SECTION, or nil."
    (let ((target (line-beginning-position)))
      (save-excursion
        (goto-char (oref section start))
        (when (looking-at "@@ .* \\+\\([0-9]+\\)\\(?:,[0-9]+\\)? @@")
          (let ((start-line (string-to-number (match-string 1)))
                (content (oref section content))
                (offset 0))
            (goto-char content)
            (while (and (< (point) target) (< (point) (oref section end)))
              (unless (eq (char-after) ?-) (cl-incf offset))
              (forward-line))
            (+ start-line offset))))))

  (defun lg/review--grab-diff-context ()
    "Grab diff context around point with the selected lines marked.
If evil visual state is active, uses the selection.  Otherwise treats
the current line as a single-line selection.  ~5 lines of padding are
included before/after, capped at hunk boundaries.
Reads directly from the magit buffer."
    (let* ((section-start (if (magit-section-match 'hunk)
                               (oref (magit-current-section) content)
                             (point-min)))
           (section-end (if (magit-section-match 'hunk)
                             (oref (magit-current-section) end)
                           (point-max)))
           (sel-beg (save-excursion
                      (goto-char (if (evil-visual-state-p)
                                     (region-beginning)
                                   (point)))
                      (line-beginning-position)))
           (sel-end (save-excursion
                      (goto-char (if (evil-visual-state-p)
                                     (region-end)
                                   (point)))
                      (line-end-position)))
           (ctx-beg (save-excursion
                      (goto-char sel-beg)
                      (forward-line -5)
                      (max (point) section-start)))
           (ctx-end (save-excursion
                      (goto-char sel-end)
                      (forward-line 6)
                      (min (point) section-end)))
           (lines (split-string
                   (buffer-substring-no-properties ctx-beg ctx-end)
                   "\n"))
           (pos ctx-beg)
           result)
      (dolist (line lines)
        (let ((line-end (+ pos (length line) 1)))
          (push (if (> (length line) 0)
                    (if (and (>= pos sel-beg) (< pos (1+ sel-end)))
                        (concat (substring line 0 1) ">" (substring line 1))
                      (concat (substring line 0 1) " " (substring line 1)))
                  line)
                result)
          (setq pos line-end)))
      (string-trim-right (mapconcat #'identity (nreverse result) "\n"))))

  ;; ==========================================================================
  ;; Start
  ;; ==========================================================================

  (defun lg/review-start ()
    "Start a review in an isolated git worktree.
Creates a worktree at branch HEAD, soft-resets to base, unstages all
changes, then opens magit-status in the worktree.  The main working
tree is completely undisturbed throughout."
    (interactive)
    (let* ((repo-top (expand-file-name (magit-toplevel)))
           (repo-key (lg/review--repo-key)))
      (when (gethash repo-key lg/review--sessions)
        (user-error "Review already in progress — resume with SPC g r c"))
      (let* ((branch        (lg/review--read-branch))
             (base-ref      (lg/review--read-base-ref))
             ;; Use the merge-base so the diff is purely branch-vs-fork-point,
             ;; not branch-vs-current-tip-of-base (which would include commits
             ;; on the base branch made after B forked off).
             (base          (string-trim
                             (shell-command-to-string
                              (format "git -C %s merge-base %s %s"
                                      (shell-quote-argument repo-top)
                                      (shell-quote-argument branch)
                                      (shell-quote-argument base-ref)))))
             (old-sha       (magit-rev-parse branch))
             (worktree-path (lg/review--worktree-path repo-top branch)))
        (when (string-empty-p base)
          (error "Could not compute merge-base of %s and %s" branch base-ref))
        (when (file-exists-p worktree-path)
          (user-error "Review worktree already exists at %s — cancel first"
                      worktree-path))
        ;; Create the worktree in detached HEAD mode at branch SHA.
        ;; --detach avoids "already checked out" errors when the branch is live
        ;; in another worktree, and ensures the branch ref is never modified by
        ;; git-reset during the review — only we update it explicitly on publish.
        (unless (= 0 (call-process "git" nil nil nil
                                   "-C" repo-top "worktree" "add"
                                   "--detach" worktree-path old-sha))
          (error "Failed to create review worktree for %s" branch))
        ;; Soft-reset to merge-base: detached HEAD moves to fork point, all
        ;; branch-only changes staged (base-branch advances are excluded).
        (unless (= 0 (lg/review--run-git worktree-path "reset" "--soft" base))
          (call-process "git" nil nil nil "-C" repo-top
                        "worktree" "remove" "--force" worktree-path)
          (error "Failed to soft-reset to %s" base))
        ;; Unstage everything into the working tree
        (unless (= 0 (lg/review--run-git worktree-path "restore" "--staged" "."))
          (call-process "git" nil nil nil "-C" repo-top
                        "worktree" "remove" "--force" worktree-path)
          (error "Failed to unstage changes in worktree"))
        ;; For new files (added by branch), register them as intent-to-add so
        ;; they appear in the diff as additions rather than as untracked files.
        (let* ((new-files-raw (lg/review--run-git-output
                               repo-top "diff" "--name-only" "--diff-filter=A"
                               base old-sha))
               (new-files (split-string (string-trim new-files-raw) "\n" t)))
          (dolist (f new-files)
            (lg/review--run-git worktree-path "add" "--intent-to-add" f)))
        ;; Open magit-status in the worktree
        (let* ((default-directory (file-name-as-directory worktree-path))
               (buf               (magit-status-setup-buffer)))
          (puthash repo-key
                   (list :branch branch :base base-ref :old-sha old-sha
                         :worktree-path worktree-path :repo-top repo-top
                         :comments nil :status-buf buf)
                   lg/review--sessions))
        (message "Review: %s vs %s (merge-base)  [s/x: stage/discard, C: comment, SPC g r p: publish]"
                 branch base-ref))))

  ;; ==========================================================================
  ;; Review header section
  ;; ==========================================================================

  (defun lg/review--insert-header ()
    "Insert a REVIEW MODE banner at the top of magit-status when active."
    (when-let* ((pair    (lg/review--find-session))
                (key     (car pair))
                (session (cdr pair)))
      (magit-insert-section (review-header)
        (insert (propertize
                 (format "  REVIEW  %s  vs  %s   [C: comment  D: del-comment  SPC g r p: publish  SPC g r k: cancel]\n"
                         (plist-get session :branch)
                         (plist-get session :base))
                 'face '(:foreground "#ff79c6" :weight bold :extend t))))))

  (add-hook 'magit-status-sections-hook #'lg/review--insert-header -90)

  ;; ==========================================================================
  ;; Comment popup mode
  ;; ==========================================================================

  (defvar lg/review-comment-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "C-c C-c") #'lg/review--finish-comment)
      (define-key map (kbd "C-c C-k") #'lg/review--cancel-comment)
      map)
    "Keymap for `lg/review-comment-mode'.")

  (define-minor-mode lg/review-comment-mode
    "Minor mode active in the review comment popup buffer."
    :lighter " V2Comment"
    :keymap lg/review-comment-mode-map)

  ;; ==========================================================================
  ;; Comment add / finish / cancel / delete
  ;; ==========================================================================

  (defun lg/review-comment ()
    "Add a review comment at the current diff location.
With a visual selection, uses the selection as diff context.
Opens a popup buffer below; C-c C-c saves, C-c C-k cancels."
    (interactive)
    (let* ((pair    (lg/review--find-session))
           (key     (car pair))
           (session (cdr pair)))
      (unless session
        (user-error "Not in an active review session — start one with SPC g r r"))
      (let* ((branch   (plist-get session :branch))
             (file     (magit-file-at-point))
             (hunk-sec (when (magit-section-match 'hunk)
                         (magit-current-section)))
             (line     (when hunk-sec
                         (lg/review--hunk-line-at-point hunk-sec)))
             (hunk-key (when hunk-sec
                         (format "%s\t%s" (or file "")
                                 (save-excursion
                                   (goto-char (oref hunk-sec start))
                                   (buffer-substring-no-properties
                                    (point) (line-end-position))))))
             (hunk-off (when hunk-sec
                         (count-lines (oref hunk-sec content) (point))))
             (diff-ctx (lg/review--grab-diff-context))
             (diff-buf (current-buffer))
             (diff-pos (line-end-position))
             (location (format "%s%s" (or file "general")
                               (if line (format ":%d" line) "")))
             (buf      (get-buffer-create "*review-comment*")))
        (when (evil-visual-state-p) (evil-exit-visual-state))
        (setq lg/review--comment-context
              (list :repo-key key :branch branch
                    :file file :line line
                    :hunk-key hunk-key :hunk-offset hunk-off
                    :diff-buf diff-buf :diff-pos diff-pos
                    :diff-context diff-ctx))
        (with-current-buffer buf
          (erase-buffer)
          (markdown-mode)
          (insert (format "<!-- %s -->\n\n" location))
          (goto-char (point-max))
          (lg/review-comment-mode 1)
          (when (bound-and-true-p evil-mode) (evil-insert-state 1)))
        (let ((win (split-window-vertically -12)))
          (select-window win)
          (switch-to-buffer buf)
          (goto-char (point-max)))
        (message "C-c C-c to save, C-c C-k to cancel"))))

  (defun lg/review--finish-comment ()
    "Save the review comment and show it inline."
    (interactive)
    (unless lg/review--comment-context
      (user-error "No comment being edited"))
    (let* ((ctx      lg/review--comment-context)
           (text     (string-trim
                      (buffer-substring-no-properties
                       (save-excursion (goto-char (point-min))
                                       (forward-line 2) (point))
                       (point-max))))
           (repo-key (plist-get ctx :repo-key))
           (file     (plist-get ctx :file))
           (line     (plist-get ctx :line))
           (hunk-key (plist-get ctx :hunk-key))
           (hunk-off (plist-get ctx :hunk-offset))
           (diff-ctx (plist-get ctx :diff-context))
           (diff-buf (plist-get ctx :diff-buf))
           (diff-pos (plist-get ctx :diff-pos))
           (location (format "%s%s" (or file "general")
                             (if line (format ":%d" line) ""))))
      (when (string-empty-p text)
        (lg/review--cancel-comment)
        (user-error "Empty comment — cancelled"))
      (let (ov)
        (when (buffer-live-p diff-buf)
          (with-current-buffer diff-buf
            (setq ov (make-overlay (1+ diff-pos) (1+ diff-pos)))
            (overlay-put ov 'before-string
                         (concat
                          (propertize (format "  >> %s\n" location)
                                      'face '(lg/review-comment-face bold))
                          (mapconcat
                           (lambda (l)
                             (propertize (format "     %s\n" l)
                                         'face 'lg/review-comment-face))
                           (split-string text "\n") "")))
            (overlay-put ov 'lg/review-comment t)))
        (when-let ((session (gethash repo-key lg/review--sessions)))
          (plist-put session :comments
                     (cons (list :file file :line line :text text
                                 :diff-context diff-ctx :ov ov
                                 :hunk-key hunk-key :hunk-offset hunk-off)
                           (plist-get session :comments)))))
      (setq lg/review--comment-context nil)
      (let ((win (selected-window)))
        (kill-buffer "*review-comment*")
        (when (window-live-p win) (delete-window win)))
      (when (buffer-live-p diff-buf) (pop-to-buffer diff-buf))
      (message "Comment saved")))

  (defun lg/review--cancel-comment ()
    "Cancel the review comment being edited."
    (interactive)
    (let ((diff-buf (plist-get lg/review--comment-context :diff-buf)))
      (setq lg/review--comment-context nil)
      (let ((win (selected-window)))
        (kill-buffer "*review-comment*")
        (when (window-live-p win) (delete-window win)))
      (when (buffer-live-p diff-buf) (pop-to-buffer diff-buf))
      (message "Comment cancelled")))

  (defun lg/review-delete-comment ()
    "Delete the review comment on the current line."
    (interactive)
    (let* ((pair    (lg/review--find-session))
           (key     (car pair))
           (session (cdr pair)))
      (unless session
        (user-error "Not in an active review session"))
      (let* ((comments (plist-get session :comments))
             (targets  (list (line-beginning-position 2)
                             (line-beginning-position)))
             found)
        (dolist (c comments)
          (let* ((ov     (plist-get c :ov))
                 (ov-pos (and ov (overlay-start ov))))
            (when (and ov-pos (member ov-pos targets))
              (setq found c))))
        (unless found (user-error "No comment on this line"))
        (when (y-or-n-p (format "Delete comment on %s%s? "
                                (or (plist-get found :file) "general")
                                (if (plist-get found :line)
                                    (format ":%d" (plist-get found :line)) "")))
          (delete-overlay (plist-get found :ov))
          (plist-put session :comments (delq found comments))
          (message "Comment deleted")))))

  ;; ==========================================================================
  ;; Overlay re-application after magit refresh
  ;; ==========================================================================

  (defun lg/review--find-hunk-section (hunk-key)
    "Walk the section tree to find a hunk section matching HUNK-KEY.
HUNK-KEY is \"FILE\\tHUNK-HEADER\" where HUNK-HEADER is the @@ line."
    (when (and (bound-and-true-p magit-root-section) hunk-key)
      (let* ((tab-pos (cl-position ?\t hunk-key))
             (header  (and tab-pos (substring hunk-key (1+ tab-pos))))
             found)
        (when header
          (cl-labels ((walk (s)
                        (when (and (not found) (eq (oref s type) 'hunk))
                          (save-excursion
                            (goto-char (oref s start))
                            (when (equal (buffer-substring-no-properties
                                          (point) (line-end-position))
                                         header)
                              (setq found s))))
                        (dolist (child (oref s children))
                          (unless found (walk child)))))
            (walk magit-root-section)))
        found)))

  (defun lg/review--reapply-overlays ()
    "Re-apply comment overlays after a magit buffer refresh."
    (when-let* ((pair     (lg/review--find-session))
                (session  (cdr pair))
                (comments (plist-get session :comments)))
      ;; Remove stale overlays first
      (dolist (c comments)
        (when-let ((ov (plist-get c :ov)))
          (delete-overlay ov)
          (plist-put c :ov nil)))
      ;; Re-apply
      (dolist (c comments)
        (let* ((hunk-key (plist-get c :hunk-key))
               (hunk-off (or (plist-get c :hunk-offset) 0))
               (file     (plist-get c :file))
               (line     (plist-get c :line))
               (text     (plist-get c :text))
               (location (format "%s%s" (or file "general")
                                 (if line (format ":%d" line) "")))
               (section  (lg/review--find-hunk-section hunk-key)))
          (when section
            (save-excursion
              (goto-char (oref section content))
              (forward-line hunk-off)
              (let* ((pos (line-end-position))
                     (ov  (make-overlay (1+ pos) (1+ pos))))
                (overlay-put ov 'before-string
                             (concat
                              (propertize (format "  >> %s\n" location)
                                          'face '(lg/review-comment-face bold))
                              (mapconcat
                               (lambda (l)
                                 (propertize (format "     %s\n" l)
                                             'face 'lg/review-comment-face))
                               (split-string text "\n") "")))
                (overlay-put ov 'lg/review-comment t)
                (plist-put c :ov ov))))))))

  (add-hook 'magit-post-refresh-hook #'lg/review--reapply-overlays)

  ;; ==========================================================================
  ;; REVIEW.md generation
  ;; ==========================================================================

  (defun lg/review--generate-review-content (session staged-stat)
    "Return REVIEW.md content string for SESSION.
STAGED-STAT is output of `git diff --staged --stat' (may be empty)."
    (let* ((branch   (plist-get session :branch))
           (base     (plist-get session :base))
           (comments (reverse (plist-get session :comments))))
      (with-temp-buffer
        (insert (format "# Review: %s\n\n" branch))
        (insert (format "**Base**: %s\n" base))
        (insert (format "**Date**: %s\n\n" (format-time-string "%Y-%m-%d")))
        (unless (string-blank-p staged-stat)
          (insert "## Staged diff\n\n```\n"
                  (string-trim staged-stat)
                  "\n```\n\n"))
        (insert "---\n\n")
        (unless (string-empty-p (bound-and-true-p lg/review-instructions))
          (insert "## Instructions\n\n" lg/review-instructions "\n\n"))
        (insert "## Comments\n\n")
        (if comments
            (dolist (c comments)
              (let ((file (plist-get c :file))
                    (line (plist-get c :line))
                    (text (plist-get c :text))
                    (ctx  (plist-get c :diff-context)))
                (insert (format "### %s%s\n"
                                (or file "general")
                                (if line (format ":%d" line) "")))
                (when ctx
                  (insert "```diff\n" ctx "\n```\n"))
                (insert text "\n\n")))
          (insert "_No comments._\n\n"))
        (buffer-string))))

  ;; ==========================================================================
  ;; Publish
  ;; ==========================================================================

  (defun lg/review-publish ()
    "Publish the review: restore branch, write REVIEW.md to repo root, remove worktree."
    (interactive)
    (let* ((pair    (or (lg/review--find-session)
                        (user-error "No active review session")))
           (key     (car pair))
           (session (cdr pair)))
      (let* ((branch        (plist-get session :branch))
             (old-sha       (plist-get session :old-sha))
             (worktree-path (plist-get session :worktree-path))
             (repo-top      (plist-get session :repo-top)))
        (unless (y-or-n-p (format "Publish review for %s? " branch))
          (user-error "Cancelled"))
        ;; Collect staged diff stat before we reset (for REVIEW.md header)
        (let* ((staged-stat  (lg/review--run-git-output
                              worktree-path "diff" "--staged" "--stat"))
               (review-content (lg/review--generate-review-content
                                session staged-stat))
               (review-dir   (or (lg/review--branch-worktree-path repo-top branch)
                                  repo-top))
               (review-file  (expand-file-name "REVIEW.md" review-dir)))
          ;; Stage all remaining/unreviewed hunks so we can diff against old-sha
          ;; to capture exactly what was discarded.
          (lg/review--run-git worktree-path "add" "-A")
          (let* ((discard-patch (lg/review--run-git-output
                                 worktree-path "diff" "--cached" old-sha))
                 (has-discards  (not (string-blank-p discard-patch))))
            ;; Restore branch to original HEAD (undoes the soft-reset)
            (unless (= 0 (lg/review--run-git worktree-path "reset" "--hard" old-sha))
              (error "Failed to restore branch %s to %s" branch old-sha))
            ;; If any hunks were discarded, apply the patch and commit them
            (when has-discards
              (let ((patch-file (make-temp-file "review-discard-" nil ".patch")))
                (unwind-protect
                    (progn
                      (with-temp-file patch-file (insert discard-patch))
                      (unless (= 0 (lg/review--run-git worktree-path "apply" patch-file))
                        (error "Failed to apply discard patch for %s" branch))
                      (lg/review--run-git worktree-path "add" "-A")
                      (unless (= 0 (lg/review--run-git worktree-path "commit" "-m"
                                                          (format "review(discard): %s" branch)))
                        (error "Failed to commit discards for %s" branch)))
                  (delete-file patch-file))))
          ;; Write REVIEW.md as an untracked file in the main worktree (not committed).
          (with-temp-file review-file
            (insert review-content))
          (find-file review-file)
          ;; Advance the branch ref to pick up any discard commit.
          ;; We use update-ref (plumbing) rather than branch -f so this works
          ;; even when the branch is currently checked out in another worktree.
          (unless (= 0 (lg/review--run-git worktree-path
                                              "update-ref"
                                              (format "refs/heads/%s" branch)
                                              "HEAD"))
            (error "Failed to advance branch ref for %s" branch))
          ;; Clean up
          (lg/review--cleanup-session key repo-top worktree-path)
          ;; Refresh main repo magit-status if visible
          (when-let ((buf (let ((default-directory repo-top))
                            (magit-get-mode-buffer 'magit-status-mode))))
            (with-current-buffer buf (magit-refresh)))
          (message "Review published: REVIEW.md written to %s (untracked)" review-dir))))))

  ;; ==========================================================================
  ;; Cancel
  ;; ==========================================================================

  (defun lg/review-cancel ()
    "Cancel the review: restore branch to original HEAD, remove worktree."
    (interactive)
    (let* ((pair    (or (lg/review--find-session)
                        (user-error "No active review session")))
           (key     (car pair))
           (session (cdr pair)))
      (let* ((branch        (plist-get session :branch))
             (old-sha       (plist-get session :old-sha))
             (worktree-path (plist-get session :worktree-path))
             (repo-top      (plist-get session :repo-top)))
        (unless (y-or-n-p (format "Cancel review for %s? " branch))
          (user-error "Cancelled"))
        ;; The branch ref was never touched (we used --detach), so nothing to
        ;; restore — just force-remove the worktree and clean up the session.
        ;; Clean up
        (lg/review--cleanup-session key repo-top worktree-path)
        (message "Review for %s cancelled" branch))))

  ;; ==========================================================================
  ;; Resume
  ;; ==========================================================================

  (defun lg/review-resume ()
    "Jump to (or re-open) the review magit-status buffer."
    (interactive)
    (let (sessions)
      (maphash (lambda (k v) (push (cons k v) sessions)) lg/review--sessions)
      (unless sessions
        (user-error "No active review sessions"))
      (let* ((session (if (= 1 (length sessions))
                          (cdar sessions)
                        (let* ((choices (mapcar (lambda (pair)
                                                  (cons (plist-get (cdr pair) :branch)
                                                        (cdr pair)))
                                                sessions))
                               (branch  (completing-read "Resume review: "
                                                         (mapcar #'car choices) nil t)))
                          (cdr (assoc branch choices)))))
             (buf (plist-get session :status-buf)))
        (if (buffer-live-p buf)
            (switch-to-buffer buf)
          ;; Buffer was killed — re-open in worktree
          (let* ((wt  (plist-get session :worktree-path))
                 (default-directory (file-name-as-directory wt))
                 (new-buf (magit-status-setup-buffer)))
            (plist-put session :status-buf new-buf)
            (switch-to-buffer new-buf)))
        (message "Reviewing %s vs %s"
                 (plist-get session :branch)
                 (plist-get session :base)))))

  ;; ==========================================================================
  ;; Cleanup helper
  ;; ==========================================================================

  (defun lg/review--cleanup-session (repo-key repo-top worktree-path)
    "Kill the status buffer, force-remove the worktree, remove session."
    (when-let* ((session (gethash repo-key lg/review--sessions))
                (buf     (plist-get session :status-buf)))
      (when (buffer-live-p buf)
        (kill-buffer buf)))
    (call-process "git" nil nil nil
                  "-C" repo-top "worktree" "remove" "--force" worktree-path)
    (remhash repo-key lg/review--sessions))

  ;; ==========================================================================
  ;; Keybindings
  ;; ==========================================================================

  ;; C / D in magit-status (for the worktree's status buffer)
  (after! magit-status
    (define-key magit-status-mode-map (kbd "C") #'lg/review-comment)
    (define-key magit-status-mode-map (kbd "D") #'lg/review-delete-comment)
    (evil-define-key* 'normal magit-status-mode-map
      "C" #'lg/review-comment
      "D" #'lg/review-delete-comment))

  ;; C / D in magit-diff (for diffs opened from the worktree status)
  (after! magit-diff
    (define-key magit-diff-section-base-map (kbd "C") #'lg/review-comment)
    (define-key magit-diff-section-base-map (kbd "D") #'lg/review-delete-comment)
    (evil-define-key* 'visual magit-diff-mode-map
      "C" #'lg/review-comment))

  ;; SPC g r r/p/k/c — overwrite v1 bindings with v2
  ;; Clear SPC g r (vc-gutter revert-hunk) so it can become a prefix keymap.
  (map! :leader "g r" nil)
  (map! :leader
        (:prefix ("g" . "git")
         (:prefix ("r" . "review")
          :desc "Start review"   "r" #'lg/review-start
          :desc "Publish review" "p" #'lg/review-publish
          :desc "Cancel review"  "k" #'lg/review-cancel
          :desc "Resume review"  "c" #'lg/review-resume))))
