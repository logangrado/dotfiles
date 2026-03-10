;;; ~/.doom.d/packages/review.el -*- lexical-binding: t; -*-

;; Local PR-style branch review system for agent worktrees.
;;
;; Lets you diff any local branch against the remote HEAD, add inline
;; comments, and publish the review as a REVIEW.md that an agent (or
;; collaborator) can read from their working directory.
;;
;; Usage:
;;   SPC g r r  Start a review — pick a branch, open its diff.
;;              If a prior review was published, offers to show only
;;              new changes since the last review.
;;   C          (in diff) Add a comment at the current file/line.
;;              Opens a small popup buffer below the diff for typing.
;;              C-c C-c saves the comment; C-c C-k cancels.
;;              Saved comments appear as inline overlays in the diff
;;              and are written to a review file on disk.
;;   D          (in diff) Delete the comment on the current line.
;;   SPC g r p  Publish — copies the review file to the branch's
;;              worktree as REVIEW.md (or repo root if no worktree).
;;              Records the reviewed-at SHA for incremental reviews.
;;              Closes the review session.
;;   SPC g r c  Resume an in-progress review.
;;   SPC g r k  Cancel a review (kill buffer, optionally delete file).
;;   SPC g r d  Quick diff only (no review session).
;;
;; Storage:
;;   Author's review history lives under ~/.reviews/<repo>-<hash>/<branch>/
;;   Each session produces a <base>..<head>.review file.  A .last-reviewed
;;   marker records the SHA at publish time for "since last review" detection.
;;
;; Multiple concurrent reviews are supported — each branch gets its own
;; diff buffer and comment list.

(after! magit

  ;; --- State ---

  (defvar lg/review--sessions (make-hash-table :test #'equal)
    "Hash table: branch → plist (:review-file PATH :diff-buf BUFFER).")

  (defvar lg/review--last-branch nil
    "Last selected branch, used as default for review commands.")

  (defvar lg/review--comment-context nil
    "Plist (:branch :file :line :diff-buf :diff-pos) for comment being edited.")

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
    "Instructions for the review agent, prepended to every review file.
Set this to a non-empty string to embed guidance for the agent reading REVIEW.md.")

  (defvar-local lg/review--comments nil
    "List of (:file FILE :line LINE :text TEXT :ov OVERLAY) plists.
Buffer-local to the diff buffer.")

  ;; --- Path helpers ---

  (defun lg/review--base-ref ()
    "Detect the remote HEAD ref (e.g. origin/main)."
    (or (when-let ((ref (magit-git-string "symbolic-ref" "refs/remotes/origin/HEAD")))
          (string-remove-prefix "refs/remotes/" ref))
        (seq-find #'magit-rev-verify '("origin/main" "origin/master" "origin/dev"))
        (user-error "Cannot detect base branch — run: git remote set-head origin --auto")))

  (defun lg/review--repo-dir ()
    "Return the review storage directory for the current repo under ~/.reviews/."
    (let* ((top (expand-file-name (magit-toplevel)))
           (name (file-name-nondirectory (directory-file-name top)))
           (hash (substring (md5 top) 0 6)))
      (expand-file-name (format "%s-%s" name hash) "~/.reviews/")))

  (defun lg/review--slug (ref)
    "Convert REF to a filesystem-safe slug (replace / with -)."
    (replace-regexp-in-string "/" "-" ref))

  (defun lg/review--branch-dir (branch)
    "Return the review directory for BRANCH."
    (expand-file-name (lg/review--slug branch) (lg/review--repo-dir)))

  (defun lg/review--review-file (branch base-ref)
    "Return the review file path for BRANCH reviewed against BASE-REF."
    (expand-file-name (format "%s..%s.review"
                              (lg/review--slug base-ref)
                              (lg/review--slug branch))
                      (lg/review--branch-dir branch)))

  (defun lg/review--last-reviewed-file (branch)
    "Return the .last-reviewed marker path for BRANCH."
    (expand-file-name ".last-reviewed" (lg/review--branch-dir branch)))

  (defun lg/review--load-last-reviewed (branch)
    "Read the last-reviewed SHA for BRANCH, or nil."
    (let ((file (lg/review--last-reviewed-file branch)))
      (when (file-exists-p file)
        (string-trim
         (with-temp-buffer
           (insert-file-contents file)
           (buffer-string))))))

  (defun lg/review--save-last-reviewed (branch)
    "Record BRANCH's current HEAD as the last-reviewed SHA."
    (let ((file (lg/review--last-reviewed-file branch))
          (sha (magit-rev-parse branch)))
      (make-directory (file-name-directory file) t)
      (with-temp-file file (insert sha "\n"))))

  (defun lg/review--worktree-for-branch (branch)
    "Return the worktree path for BRANCH, or nil."
    (let ((current-top (expand-file-name (magit-toplevel))))
      (cl-loop for (path _commit wt-branch) in (magit-list-worktrees)
               when (and (equal wt-branch branch)
                         (not (string= (expand-file-name path) current-top)))
               return (expand-file-name path))))

  (defun lg/review--publish-target (branch)
    "Return the REVIEW.md target path for BRANCH."
    (if-let ((wt (lg/review--worktree-for-branch branch)))
        (expand-file-name "REVIEW.md" wt)
      (expand-file-name "REVIEW.md" (magit-toplevel))))

  ;; --- Branch selection ---

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
           (input (read-string prompt nil nil default)))
      (if (string-empty-p input) default input)))

  ;; --- Deriving branch from diff buffer ---

  (defun lg/review--branch-from-range ()
    "Extract the target branch from the current magit-diff buffer range."
    (when (bound-and-true-p magit-buffer-range)
      (when (string-match "\\.\\{2,3\\}\\(.+\\)$" magit-buffer-range)
        (match-string 1 magit-buffer-range))))

  ;; --- Review file generation ---

  (defun lg/review--write-review-file (branch)
    "Regenerate the review file for BRANCH from the in-memory comments list."
    (let* ((session (gethash branch lg/review--sessions))
           (review-file (plist-get session :review-file))
           (diff-buf (plist-get session :diff-buf))
           (comments (when (buffer-live-p diff-buf)
                       (buffer-local-value 'lg/review--comments diff-buf))))
      (when review-file
        (make-directory (file-name-directory review-file) t)
        (with-temp-file review-file
          (insert (format "# Review: %s\n\n" branch))
          (insert (format "**Date**: %s\n\n" (format-time-string "%Y-%m-%d")))
          (insert "---\n\n")
          (unless (string-empty-p lg/review-instructions)
            (insert "## Instructions\n\n" lg/review-instructions "\n\n"))
          (insert "## Comments\n\n")
          (dolist (c (reverse comments))
            (let ((file (plist-get c :file))
                  (line (plist-get c :line))
                  (text (plist-get c :text))
                  (ctx (plist-get c :diff-context)))
              (insert (format "### %s%s\n"
                              (or file "general")
                              (if line (format ":%d" line) "")))
              (when ctx
                (insert "```diff\n" ctx "\n```\n"))
              (insert text "\n\n")))))))

  ;; --- Commands ---

  (defun lg/review-diff ()
    "Show diff between remote HEAD and a branch."
    (interactive)
    (let ((branch (lg/review--read-branch))
          (base (lg/review--read-base-ref)))
      (magit-diff-range (format "%s...%s" base branch))))

  (defun lg/review-start ()
    "Start a review: show diff for a branch.
If a prior review exists, offers to review since last review or all changes."
    (interactive)
    (let* ((branch (lg/review--read-branch))
           (base (lg/review--read-base-ref))
           (last-sha (lg/review--load-last-reviewed branch))
           (current-sha (magit-rev-parse branch))
           ;; Determine diff range
           (effective-base
            (if (and last-sha
                     (not (string= last-sha current-sha))
                     (magit-rev-verify last-sha))
                (if (y-or-n-p (format "Prior review found at %.8s. Review since then? "
                                      last-sha))
                    last-sha
                  base)
              base))
           (range (format "%s...%s" effective-base branch))
           (review-file (lg/review--review-file branch effective-base)))
      ;; Show diff in current window
      (magit-diff-range range)
      ;; Track session (review file path + diff buffer)
      (puthash branch (list :review-file review-file
                            :diff-buf (current-buffer))
               lg/review--sessions)
      (message "Review started for %s — press C to comment, SPC g r p to publish" branch)))

  ;; --- Comment editing (popup buffer in current window) ---

  (defvar lg/review-comment-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "C-c C-c") #'lg/review--finish-comment)
      (define-key map (kbd "C-c C-k") #'lg/review--cancel-comment)
      map)
    "Keymap for the review comment buffer.")

  (define-minor-mode lg/review-comment-mode
    "Minor mode for the review comment editing buffer."
    :lighter " Comment"
    :keymap lg/review-comment-mode-map)

  (evil-set-initial-state 'lg/review-comment-mode 'insert)

  (defun lg/review--grab-diff-context ()
    "Grab diff context around point with the selected lines marked.
If evil visual state is active, uses the selection.  Otherwise treats
the current line as a single-line selection.  In both cases, ~5 lines
of padding are included before/after, capped at hunk boundaries."
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
          (if (and (>= pos sel-beg) (< pos (1+ sel-end)))
              ;; Insert > after the first char (+/-/space) to mark selected lines
              (push (if (> (length line) 0)
                        (concat (substring line 0 1) ">" (substring line 1))
                      line)
                    result)
            (push line result))
          (setq pos line-end)))
      (string-trim-right (mapconcat #'identity (nreverse result) "\n"))))

  (defun lg/review-comment ()
    "Add a comment at the current diff location.
With a visual selection, uses the selection as diff context.
Otherwise grabs ~10 lines around point.
Opens a popup buffer below the current window for typing.
C-c C-c saves, C-c C-k cancels."
    (interactive)
    (let* ((branch (or (lg/review--branch-from-range)
                       (user-error "Not in a review diff buffer")))
           (session (gethash branch lg/review--sessions)))
      (unless session
        (user-error "No active review for %s — start one with SPC g r r" branch))
      (let* ((file (magit-file-at-point))
             (line (when (magit-section-match 'hunk)
                     (magit-diff-hunk-line (magit-current-section) nil)))
             (diff-context (lg/review--grab-diff-context))
             (diff-buf (current-buffer))
             (diff-pos (line-end-position))
             (location (format "%s%s" (or file "general")
                               (if line (format ":%d" line) "")))
             (buf (get-buffer-create "*review-comment*")))
        ;; Exit visual state if active
        (when (evil-visual-state-p) (evil-exit-visual-state))
        ;; Store context for finish/cancel
        (setq lg/review--comment-context
              (list :branch branch :file file :line line
                    :diff-buf diff-buf :diff-pos diff-pos
                    :diff-context diff-context))
        ;; Set up comment buffer
        (with-current-buffer buf
          (erase-buffer)
          (markdown-mode)
          (insert (format "<!-- %s -->\n\n" location))
          (goto-char (point-max))
          (lg/review-comment-mode 1))
        ;; Split current window vertically, show comment buffer below
        (let ((comment-win (split-window-vertically -12)))
          (select-window comment-win)
          (switch-to-buffer buf)
          (goto-char (point-max)))
        (message "C-c C-c to save, C-c C-k to cancel"))))

  (defun lg/review--finish-comment ()
    "Save the comment and show it inline in the diff."
    (interactive)
    (unless lg/review--comment-context
      (user-error "No comment being edited"))
    (let* ((ctx lg/review--comment-context)
           ;; Extract text after the header line
           (text (string-trim
                  (buffer-substring-no-properties
                   (save-excursion (goto-char (point-min))
                                   (forward-line 2)
                                   (point))
                   (point-max))))
           (branch (plist-get ctx :branch))
           (file (plist-get ctx :file))
           (line (plist-get ctx :line))
           (diff-context (plist-get ctx :diff-context))
           (diff-buf (plist-get ctx :diff-buf))
           (diff-pos (plist-get ctx :diff-pos))
           (location (format "%s%s" (or file "general")
                             (if line (format ":%d" line) ""))))
      (when (string-empty-p text)
        (lg/review--cancel-comment)
        (user-error "Empty comment — cancelled"))
      ;; Add visual overlay in diff buffer
      (let (ov)
        (when (buffer-live-p diff-buf)
          (with-current-buffer diff-buf
            ;; before-string on a zero-width overlay at start of next line
            ;; so the comment appears on its own line between diff lines
            (setq ov (make-overlay (1+ diff-pos) (1+ diff-pos)))
            (overlay-put ov 'before-string
                         (propertize (format "  >> %s: %s\n" location text)
                                     'face 'font-lock-comment-face))
            (overlay-put ov 'lg/review-comment t)
            ;; Track in buffer-local list
            (push (list :file file :line line :text text
                        :diff-context diff-context :ov ov)
                  lg/review--comments)))
        ;; Write review file
        (lg/review--write-review-file branch))
      ;; Close comment window, return to diff
      (setq lg/review--comment-context nil)
      (let ((win (selected-window)))
        (kill-buffer "*review-comment*")
        (when (window-live-p win)
          (delete-window win)))
      (when (buffer-live-p diff-buf)
        (pop-to-buffer diff-buf))
      (message "Comment saved")))

  (defun lg/review--cancel-comment ()
    "Cancel the comment being edited."
    (interactive)
    (let ((diff-buf (plist-get lg/review--comment-context :diff-buf)))
      (setq lg/review--comment-context nil)
      (let ((win (selected-window)))
        (kill-buffer "*review-comment*")
        (when (window-live-p win)
          (delete-window win)))
      (when (buffer-live-p diff-buf)
        (pop-to-buffer diff-buf))
      (message "Comment cancelled")))

  (defun lg/review-delete-comment ()
    "Delete the review comment on the current line.
Comments are attached to the line above them, so place cursor on
the diff line where you pressed C to add the comment."
    (interactive)
    (let* ((branch (or (lg/review--branch-from-range)
                       (user-error "Not in a review diff buffer")))
           ;; Comment overlays sit at (1+ line-end-position) of the
           ;; annotated line, i.e. at (line-beginning-position 2).
           ;; Check both the start of the next line and start of this line
           ;; in case the cursor is on the line just below.
           (target-positions (list (line-beginning-position 2)
                                   (line-beginning-position)))
           (found nil))
      (dolist (c lg/review--comments)
        (let* ((ov (plist-get c :ov))
               (ov-pos (and ov (overlay-start ov))))
          (when (and ov-pos (member ov-pos target-positions))
            (setq found c))))
      (unless found
        (user-error "No comment on this line"))
      (when (y-or-n-p (format "Delete comment on %s%s? "
                              (or (plist-get found :file) "general")
                              (if (plist-get found :line)
                                  (format ":%d" (plist-get found :line))
                                "")))
        (delete-overlay (plist-get found :ov))
        (setq lg/review--comments (delq found lg/review--comments))
        (lg/review--write-review-file branch)
        (message "Comment deleted"))))

  (defun lg/review-publish ()
    "Publish the review to the target branch's worktree or repo root.
Closes the review session afterwards."
    (interactive)
    (let* ((branch (or (lg/review--branch-from-range)
                       (user-error "Not in a review diff buffer")))
           (session (gethash branch lg/review--sessions))
           (review-file (and session (plist-get session :review-file)))
           (diff-buf (and session (plist-get session :diff-buf)))
           (target (lg/review--publish-target branch)))
      (unless (and review-file (file-exists-p review-file))
        (user-error "No review file for %s — add comments first" branch))
      (copy-file review-file target t)
      (lg/review--save-last-reviewed branch)
      ;; Close the review session
      (remhash branch lg/review--sessions)
      (when (buffer-live-p diff-buf)
        ;; Switch away first since we're likely inside this buffer
        (when (eq (current-buffer) diff-buf)
          (magit-mode-bury-buffer))
        (kill-buffer diff-buf))
      (message "Review published to %s" target)))

  ;; --- Session management ---

  (defun lg/review--active-sessions ()
    "Return alist of (branch . session-plist) for live sessions."
    (let (result dead)
      (maphash (lambda (branch session)
                 (if (buffer-live-p (plist-get session :diff-buf))
                     (push (cons branch session) result)
                   (push branch dead)))
               lg/review--sessions)
      ;; Clean up dead entries
      (dolist (branch dead)
        (remhash branch lg/review--sessions))
      (nreverse result)))

  (defun lg/review-resume ()
    "Resume an in-progress review. Always shows a picker."
    (interactive)
    (let* ((sessions (lg/review--active-sessions)))
      (unless sessions
        (user-error "No reviews in progress"))
      (let* ((branch (completing-read "Resume review: "
                                      (mapcar #'car sessions) nil t))
             (session (cdr (assoc branch sessions)))
             (diff-buf (plist-get session :diff-buf)))
        (switch-to-buffer diff-buf)
        (message "Resumed review for %s" branch))))

  (defun lg/review-cancel ()
    "Cancel and discard a review in progress."
    (interactive)
    (let* ((sessions (lg/review--active-sessions)))
      (unless sessions
        (user-error "No reviews in progress"))
      (let* ((branch (completing-read "Cancel review: "
                                      (mapcar #'car sessions) nil t))
             (session (cdr (assoc branch sessions)))
             (review-file (plist-get session :review-file))
             (diff-buf (plist-get session :diff-buf)))
        (when (y-or-n-p (format "Cancel review for %s? " branch))
          ;; Kill diff buffer
          (when (buffer-live-p diff-buf)
            (kill-buffer diff-buf))
          ;; Delete review file
          (when (and review-file (file-exists-p review-file))
            (when (y-or-n-p "Delete review file from disk? ")
              (delete-file review-file)))
          ;; Remove from sessions
          (remhash branch lg/review--sessions)
          (message "Review for %s cancelled" branch)))))

  ;; --- Keybindings ---

  ;; SPC g r was bound to +vc-gutter/revert-hunk (revert hunk under point
  ;; in file buffers). Magit's own `k`/`x` cover hunk discard, so we
  ;; reclaim the prefix for review commands.
  (map! :leader "g r" nil)
  (map! :leader
        (:prefix ("g" . "git")
         (:prefix ("r" . "review")
          :desc "Start review"    "r" #'lg/review-start
          :desc "Resume review"   "c" #'lg/review-resume
          :desc "Cancel review"   "k" #'lg/review-cancel
          :desc "Diff only"       "d" #'lg/review-diff
          :desc "Publish"         "p" #'lg/review-publish))))

;; Bind C on magit's diff section keymap (magit-diff-section-base-map),
;; which is where the original C → magit-commit-add-log lives.  This is
;; a section keymap active when point is on any diff hunk/file, so it
;; takes precedence over mode-map evil-state bindings.
(after! magit-diff
  (define-key magit-diff-section-base-map (kbd "C") #'lg/review-comment)
  (define-key magit-diff-section-base-map (kbd "D") #'lg/review-delete-comment)
  ;; In evil visual state, C is evil-change — override it for magit-diff
  (evil-define-key* 'visual magit-diff-mode-map
    "C" #'lg/review-comment))

