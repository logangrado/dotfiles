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
;;   R          (in review diff) Mark hunk or file as reviewed / unreviewed.
;;   C          (in diff) Add a comment at the current file/line.
;;              Opens a small popup buffer below the diff for typing.
;;              C-c C-c saves the comment; C-c C-k cancels.
;;              Saved comments appear as inline overlays in the diff
;;              and are written to a review file on disk.
;;   D          (in diff) Delete the comment on the current line.
;;   g          (in review diff) Refresh the buffer.
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

  (defvar lg/review-delta-args
    '("--no-gitconfig"
      "--line-numbers"
      "--true-color" "always"
      "--max-line-distance" "0.6")
    "Arguments passed to delta when colorizing hunks in the review buffer.
`--no-gitconfig' isolates the review buffer from ~/.gitconfig [delta] changes.
`--line-numbers' is intentional: line numbers are useful in the review view
(unlike in magit, where they break hunk staging).
Customize here rather than in gitconfig to keep review rendering stable.")

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
    "List of (:file FILE :line LINE :text TEXT :ov OVERLAY :hunk-key KEY :hunk-offset N) plists.
Buffer-local to the diff buffer.")

  (defvar-local lg/review--reviewed-hunks nil
    "Hash table: hunk-key → t.  Hunk-key = \"FILE\\tHUNK-HEADER\".")

  (defvar-local lg/review--diff-range nil
    "The diff range used for this buffer, e.g. \"origin/main...mybranch\".")

  (defvar-local lg/review--parsed-files nil
    "Parsed diff data: list of (:path PATH :hunks LIST) plists.
Set by `lg/review--render-review-buffer'; used by `lg/review--grab-diff-context'
to produce raw diff text (not delta-decorated) in REVIEW.md.")

  (defface lg/review-comment-face
    '((t :foreground "#f8f8f2" :background "#44475a" :extend t))
    "Face for inline review comment overlays in diff buffers.")

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
           (candidates (magit-list-branch-names))
           (choice (completing-read prompt candidates nil t nil nil default)))
      (if (string-empty-p choice) default choice)))

  ;; --- Deriving branch from diff buffer ---

  (defun lg/review--branch-from-range ()
    "Extract the target branch from the current review/magit-diff buffer range."
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

  ;; =========================================================================
  ;; --- Review diff mode (two-section Unreviewed / Reviewed) ---
  ;; =========================================================================

  ;; --- Diff parsing ---

  (defun lg/review--parse-diff (diff-text)
    "Parse DIFF-TEXT into a list of file-entry plists.
Each entry: (:path PATH :hunks LIST-OF-HUNK-PLISTS)
Each hunk:  (:key \"PATH\\tHEADER\" :header HEADER :body (LINE...))"
    (let (files
          current-path current-hunks
          current-key current-header current-body)
      (cl-flet ((finish-hunk ()
                  (when current-key
                    (push (list :key current-key
                                :header current-header
                                :body (nreverse current-body))
                          current-hunks)
                    (setq current-key nil current-header nil current-body nil)))
                (finish-file ()
                  (when current-path
                    (push (list :path current-path
                                :hunks (nreverse current-hunks))
                          files)
                    (setq current-path nil current-hunks nil))))
        (dolist (line (split-string diff-text "\n"))
          (cond
           ((string-match "^diff --git a/.+ b/\\(.+\\)$" line)
            (finish-hunk)
            (finish-file)
            (setq current-path (match-string 1 line)))
           ((string-match "^@@ " line)
            (finish-hunk)
            (setq current-key    (format "%s\t%s" current-path line)
                  current-header line
                  current-body   (list line)))
           ((and current-key
                 (not (string-match
                       "^\\(---\\|\\+\\+\\+\\|index \\|new file\\|old mode\\|new mode\\|Binary\\)"
                       line)))
            (push line current-body))))
        (finish-hunk)
        (finish-file))
      (nreverse files)))

  ;; --- Line number helper (replaces magit-diff-hunk-line) ---

  (defun lg/review--hunk-line-at-point (section)
    "Return new-file line number at point within hunk SECTION, or nil."
    ;; Capture user's line position before any point movement.
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

  ;; --- Buffer rendering ---

  (defun lg/review--diff-face-for-line (line)
    "Return the face to use for a diff body LINE (fallback when delta unavailable)."
    (cond
     ((string-prefix-p "+" line) 'magit-diff-added)
     ((string-prefix-p "-" line) 'magit-diff-removed)
     ((string-prefix-p "@" line) 'magit-diff-hunk-heading)
     (t 'magit-diff-context)))

  (defun lg/review--get-diff-text (range)
    "Return plain diff text for RANGE (used for parsing only)."
    (with-temp-buffer
      (magit-git-insert "diff" range)
      (buffer-string)))

  (defun lg/review--delta-colorize-hunk (path hunk)
    "Return delta-colored body lines for HUNK in PATH.
Constructs a minimal synthetic unified diff for just this hunk, pipes it
through delta, and converts ANSI codes to Emacs `face' text properties via
`ansi-color-apply' (built-in; sets `face', not `font-lock-face', so colors
render without font-lock-mode being active).

We take the LAST N lines of delta's output (N = number of body lines in the
hunk) rather than searching for @@ in delta's output.  When the user has
line-numbers enabled in their delta config, delta replaces the @@ hunk header
with a decorative separator, so @@ may not appear literally.  Since we send
exactly one hunk, our body lines are always the final N lines of output;
delta's own decorations (file header, hunk header, horizontal rules) always
appear before them.

Returns a list of propertized strings, or nil if delta is not available."
    (when (executable-find "delta")
      (let* ((header     (plist-get hunk :header))
             (body-lines (cdr (plist-get hunk :body)))
             (n          (length body-lines))
             (synthetic  (concat (format "diff --git a/%s b/%s\n--- a/%s\n+++ b/%s\n"
                                         path path path path)
                                 header "\n"
                                 (mapconcat #'identity body-lines "\n")
                                 "\n")))
        (with-temp-buffer
          (insert synthetic)
          (when (zerop (apply #'call-process-region (point-min) (point-max)
                                                   "delta" t t nil lg/review-delta-args))
            (let* ((raw    (ansi-color-apply (buffer-string)))
                   ;; ansi-color-apply sets font-lock-face, not face.
                   ;; Our buffer has no font-lock-mode, so copy to face.
                   (_      (let ((pos 0) (len (length raw)))
                             (while (< pos len)
                               (let* ((next (next-single-property-change
                                             pos 'font-lock-face raw len))
                                      (flf  (get-text-property pos 'font-lock-face raw)))
                                 (when flf
                                   (put-text-property pos next 'face flf raw))
                                 (setq pos next)))))
                   (lines  (split-string raw "\n"))
                   ;; Drop the trailing empty string produced by the final newline
                   (lines  (if (string-empty-p (car (last lines)))
                               (butlast lines)
                             lines)))
              (when (>= (length lines) n)
                (nthcdr (- (length lines) n) lines))))))))

  (defun lg/review--insert-hunk-section (hunk path)
    "Insert HUNK as a collapsible magit section, colorized via delta if available."
    (let* ((key    (plist-get hunk :key))
           (header (plist-get hunk :header))
           (body   (plist-get hunk :body))
           (colored (lg/review--delta-colorize-hunk path hunk)))
      (magit-insert-section (hunk key)
        (magit-insert-heading header)
        (if colored
            (dolist (line colored)
              (let ((start (point)))
                (insert line "\n")
                ;; Apply delta colors as high-priority overlays so they survive
                ;; magit's section highlighting, which calls put-text-property
                ;; with font-lock-face on the buffer content, overriding text
                ;; properties.  Overlays with priority > 0 always win.
                (let ((pos start)
                      (end (1- (point))))  ; exclude trailing \n
                  (while (< pos end)
                    (let* ((next (or (next-single-property-change
                                     pos 'font-lock-face nil end)
                                    end))
                           (flf  (get-text-property pos 'font-lock-face)))
                      (when flf
                        (let ((ov (make-overlay pos next)))
                          (overlay-put ov 'face flf)
                          (overlay-put ov 'priority 10)
                          (overlay-put ov 'lg/review-delta t)))
                      (setq pos next))))))
          ;; Fallback: apply magit diff faces manually
          (dolist (line (cdr body))
            (insert (propertize line 'face (lg/review--diff-face-for-line line))
                    "\n"))))))

  (defun lg/review--insert-file-section (path hunks)
    "Insert a file section for PATH containing only HUNKS."
    (magit-insert-section (file path)
      (magit-insert-heading
        (propertize path 'face 'magit-diff-file-heading))
      (dolist (hunk hunks)
        (lg/review--insert-hunk-section hunk path))))

  (defun lg/review--render-review-buffer (&optional collapse-files)
    "Render the full Unreviewed/Reviewed buffer contents.
When COLLAPSE-FILES is non-nil, collapse all file sections after rendering."
    (let* ((inhibit-read-only t)
           (diff-text (lg/review--get-diff-text lg/review--diff-range))
           (files (lg/review--parse-diff (or diff-text "")))
           (reviewed lg/review--reviewed-hunks))
      (setq lg/review--parsed-files files)
      (erase-buffer)
      (magit-insert-section (root)
        ;; Unreviewed section
        (magit-insert-section (review-unreviewed)
          (magit-insert-heading "Unreviewed")
          (dolist (file-entry files)
            (let* ((path (plist-get file-entry :path))
                   (unreviewed (cl-remove-if
                                (lambda (h) (gethash (plist-get h :key) reviewed))
                                (plist-get file-entry :hunks))))
              (when unreviewed
                (lg/review--insert-file-section path unreviewed)))))
        ;; Reviewed section
        (magit-insert-section (review-reviewed)
          (magit-insert-heading "Reviewed")
          (dolist (file-entry files)
            (let* ((path (plist-get file-entry :path))
                   (reviewed-hunks (cl-remove-if-not
                                    (lambda (h) (gethash (plist-get h :key) reviewed))
                                    (plist-get file-entry :hunks))))
              (when reviewed-hunks
                (lg/review--insert-file-section path reviewed-hunks)))))
        (insert "\n"))
      ;; On initial open, collapse all file sections so only filenames show.
      (when collapse-files
        (cl-labels ((walk (s)
                      (when (eq (oref s type) 'file)
                        (magit-section-hide s))
                      (dolist (child (oref s children))
                        (walk child))))
          (walk magit-root-section)))
      (goto-char (point-min))))

  ;; --- Reviewed state helpers ---

  (defun lg/review--collect-hunk-keys (section)
    "Return list of all hunk-section values under SECTION."
    (let (keys)
      (cl-labels ((walk (s)
                    (if (eq (oref s type) 'hunk)
                        (push (oref s value) keys)
                      (dolist (child (oref s children))
                        (walk child)))))
        (walk section))
      keys))

  (defun lg/review--all-hunk-keys ()
    "Return all hunk-section values in buffer order (depth-first)."
    (lg/review--collect-hunk-keys magit-root-section))

  (defun lg/review--next-hunk-key (anchor-key)
    "Return the hunk key immediately after ANCHOR-KEY in display order, or nil."
    (cadr (member anchor-key (lg/review--all-hunk-keys))))

  (defun lg/review--apply-reviewed (section reviewed-p)
    "Set REVIEWED-P for all hunks under SECTION.
Returns the key to use as navigation anchor: the section's own hunk key,
or the last hunk key under a file section."
    (let ((type (oref section type)))
      (cond
       ((eq type 'hunk)
        (let ((key (oref section value)))
          (if reviewed-p
              (puthash key t lg/review--reviewed-hunks)
            (remhash key lg/review--reviewed-hunks))
          key))
       ((eq type 'file)
        (let ((keys (lg/review--collect-hunk-keys section)))
          (if reviewed-p
              (dolist (k keys) (puthash k t lg/review--reviewed-hunks))
            (dolist (k keys) (remhash k lg/review--reviewed-hunks)))
          (car (last keys))))
       (t
        (user-error "Point is not on a hunk or file section")))))

  (defun lg/review-stage ()
    "Mark hunk or file at point as reviewed; advance point to next hunk."
    (interactive)
    (let* ((section (magit-current-section))
           (anchor  (lg/review--apply-reviewed section t))
           (next    (lg/review--next-hunk-key anchor)))
      (lg/review--refresh-diff-buffer)
      (if next
          (when-let ((s (lg/review--find-hunk-section next)))
            (goto-char (oref s start)))
        (goto-char (point-min)))))

  (defun lg/review-unstage ()
    "Mark hunk or file at point as unreviewed; move point to it in Unreviewed."
    (interactive)
    (let* ((section (magit-current-section))
           (type    (oref section type))
           (goto-key (cond ((eq type 'hunk) (oref section value))
                           ((eq type 'file)
                            (car (lg/review--collect-hunk-keys section)))
                           (t nil))))
      (lg/review--apply-reviewed section nil)
      (lg/review--refresh-diff-buffer)
      (when goto-key
        (when-let ((s (lg/review--find-hunk-section goto-key)))
          (goto-char (oref s start))))))

  ;; --- Comment overlay re-application ---

  (defun lg/review--find-hunk-section (key)
    "Walk the magit section tree to find a hunk section with value KEY."
    (let (found)
      (cl-labels ((walk (section)
                    (when (and (eq (oref section type) 'hunk)
                               (equal (oref section value) key))
                      (setq found section))
                    (dolist (child (oref section children))
                      (walk child))))
        (walk magit-root-section))
      found))

  (defun lg/review--reapply-comment-overlays ()
    "Re-create comment overlays after a buffer refresh."
    (dolist (c lg/review--comments)
      (let* ((key    (plist-get c :hunk-key))
             (offset (or (plist-get c :hunk-offset) 0))
             (section (when key (lg/review--find-hunk-section key))))
        (if section
            (save-excursion
              (goto-char (oref section content))
              (forward-line offset)
              (let* ((pos (line-end-position))
                     (file (plist-get c :file))
                     (line (plist-get c :line))
                     (text (plist-get c :text))
                     (location (format "%s%s" (or file "general")
                                       (if line (format ":%d" line) "")))
                     (ov (make-overlay (1+ pos) (1+ pos))))
                (overlay-put ov 'before-string
                             (concat
                              (propertize (format "  >> %s\n" location)
                                          'face '(lg/review-comment-face bold))
                              (mapconcat
                               (lambda (l)
                                 (propertize (format "     %s\n" l)
                                             'face 'lg/review-comment-face))
                               (split-string text "\n")
                               "")))
                (overlay-put ov 'lg/review-comment t)
                (plist-put c :ov ov)))
          ;; Hunk not currently visible — no overlay
          (plist-put c :ov nil)))))

  ;; --- Refresh ---

  (defun lg/review--refresh-diff-buffer ()
    "Refresh the review diff buffer, preserving reviewed state and comments."
    (interactive)
    (dolist (c lg/review--comments)
      (when-let ((ov (plist-get c :ov)))
        (delete-overlay ov)
        (plist-put c :ov nil)))
    ;; erase-buffer does not remove overlays; clean up delta color overlays explicitly.
    (remove-overlays (point-min) (point-max) 'lg/review-delta t)
    (lg/review--render-review-buffer)
    (lg/review--reapply-comment-overlays))

  ;; --- Major mode ---

  (defvar lg/review-diff-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "s") #'lg/review-stage)
      (define-key map (kbd "u") #'lg/review-unstage)
      (define-key map (kbd "C") #'lg/review-comment)
      (define-key map (kbd "D") #'lg/review-delete-comment)
      (define-key map (kbd "g") #'lg/review--refresh-diff-buffer)
      map)
    "Keymap for `lg/review-diff-mode'.")

  (define-derived-mode lg/review-diff-mode magit-section-mode "Review"
    "Two-section diff buffer: Unreviewed / Reviewed."
    (setq-local revert-buffer-function
                (lambda (_ignore-auto _noconfirm)
                  (lg/review--refresh-diff-buffer))))

  ;; evil-mode's normal state overrides plain define-key bindings.
  ;; Explicitly bind for normal state so s/u/C/D/g work without switching states.
  (evil-define-key* 'normal lg/review-diff-mode-map
    "s" #'lg/review-stage
    "u" #'lg/review-unstage
    "C" #'lg/review-comment
    "D" #'lg/review-delete-comment
    "g" #'lg/review--refresh-diff-buffer)

  ;; --- Commands ---

  (defun lg/review-diff ()
    "Show diff between remote HEAD and a branch (no review session)."
    (interactive)
    (let ((branch (lg/review--read-branch))
          (base (lg/review--read-base-ref)))
      (magit-diff-range (format "%s...%s" base branch))))

  (defun lg/review-start ()
    "Start a review: show a two-section diff buffer for a branch.
If a prior review exists, offers to review since last review or all changes."
    (interactive)
    (let* ((branch (lg/review--read-branch))
           (base (lg/review--read-base-ref))
           (last-sha (lg/review--load-last-reviewed branch))
           (current-sha (magit-rev-parse branch))
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
           (review-file (lg/review--review-file branch effective-base))
           (buf (get-buffer-create (format "*review: %s*" branch))))
      (with-current-buffer buf
        (lg/review-diff-mode)
        ;; magit-buffer-range lets lg/review--branch-from-range work
        (setq-local magit-buffer-range range)
        (setq lg/review--diff-range range)
        (setq lg/review--reviewed-hunks (make-hash-table :test #'equal))
        (lg/review--render-review-buffer t))
      (switch-to-buffer buf)
      (puthash branch (list :review-file review-file :diff-buf buf)
               lg/review--sessions)
      (message "Review started for %s — R: mark reviewed, C: comment, SPC g r p: publish"
               branch)))

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

  (defun lg/review--find-parsed-hunk (key)
    "Return the raw hunk plist with :key KEY from `lg/review--parsed-files'."
    (cl-some (lambda (file)
               (cl-find key (plist-get file :hunks)
                        :key (lambda (h) (plist-get h :key))
                        :test #'equal))
             lg/review--parsed-files))

  (defun lg/review--grab-diff-context ()
    "Grab diff context around point with the selected lines marked.
If evil visual state is active, uses the selection.  Otherwise treats
the current line as a single-line selection.  In both cases, ~5 lines
of padding are included before/after, capped at hunk boundaries.

When in a hunk section of the review buffer, reads from the raw parsed
diff body (standard unified-diff format) so that REVIEW.md contains
clean diff lines rather than delta's decorated line-number output."
    (if-let* (((magit-section-match 'hunk))
               (section  (magit-current-section))
               (hunk     (lg/review--find-parsed-hunk (oref section value)))
               (raw-body (cdr (plist-get hunk :body)))) ; cdr skips the @@ header
        ;; Raw path: map buffer point → index into raw-body (1 buffer line = 1 raw line)
        (let* ((content     (oref section content))
               (n           (length raw-body))
               (point-idx   (max 0 (min (1- n)
                                        (1- (count-lines content (point))))))
               (sel-beg-idx (if (evil-visual-state-p)
                                (max 0 (1- (count-lines content (region-beginning))))
                              point-idx))
               (sel-end-idx (if (evil-visual-state-p)
                                (max 0 (1- (count-lines content (region-end))))
                              point-idx))
               (ctx-beg-idx (max 0 (- sel-beg-idx 5)))
               (ctx-end-idx (min (1- n) (+ sel-end-idx 5)))
               (ctx-lines   (cl-subseq raw-body ctx-beg-idx (1+ ctx-end-idx)))
               (rel-sel-beg (- sel-beg-idx ctx-beg-idx))
               (rel-sel-end (- sel-end-idx ctx-beg-idx))
               result)
          (cl-loop for line in ctx-lines
                   for i from 0
                   do (push (if (> (length line) 0)
                                (if (and (>= i rel-sel-beg) (<= i rel-sel-end))
                                    (concat (substring line 0 1) ">" (substring line 1))
                                  (concat (substring line 0 1) " " (substring line 1)))
                              line)
                            result))
          (string-trim-right (mapconcat #'identity (nreverse result) "\n")))
      ;; Fallback: read directly from buffer (magit-diff-mode or non-hunk position)
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
        (string-trim-right (mapconcat #'identity (nreverse result) "\n")))))

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
             (hunk-section (when (magit-section-match 'hunk)
                             (magit-current-section)))
             (line (when hunk-section
                     (lg/review--hunk-line-at-point hunk-section)))
             (hunk-key (when hunk-section (oref hunk-section value)))
             (hunk-offset (when hunk-section
                            (count-lines (oref hunk-section content) (point))))
             (diff-context (lg/review--grab-diff-context))
             (diff-buf (current-buffer))
             (diff-pos (line-end-position))
             (location (format "%s%s" (or file "general")
                               (if line (format ":%d" line) "")))
             (buf (get-buffer-create "*review-comment*")))
        (when (evil-visual-state-p) (evil-exit-visual-state))
        (setq lg/review--comment-context
              (list :branch branch :file file :line line
                    :hunk-key hunk-key :hunk-offset hunk-offset
                    :diff-buf diff-buf :diff-pos diff-pos
                    :diff-context diff-context))
        (with-current-buffer buf
          (erase-buffer)
          (markdown-mode)
          (insert (format "<!-- %s -->\n\n" location))
          (goto-char (point-max))
          (lg/review-comment-mode 1))
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
           (text (string-trim
                  (buffer-substring-no-properties
                   (save-excursion (goto-char (point-min))
                                   (forward-line 2)
                                   (point))
                   (point-max))))
           (branch      (plist-get ctx :branch))
           (file        (plist-get ctx :file))
           (line        (plist-get ctx :line))
           (hunk-key    (plist-get ctx :hunk-key))
           (hunk-offset (plist-get ctx :hunk-offset))
           (diff-context (plist-get ctx :diff-context))
           (diff-buf    (plist-get ctx :diff-buf))
           (diff-pos    (plist-get ctx :diff-pos))
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
                           (split-string text "\n")
                           "")))
            (overlay-put ov 'lg/review-comment t)
            (push (list :file file :line line :text text
                        :diff-context diff-context :ov ov
                        :hunk-key hunk-key :hunk-offset hunk-offset)
                  lg/review--comments)))
        (lg/review--write-review-file branch))
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
      (remhash branch lg/review--sessions)
      (when (buffer-live-p diff-buf)
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
          (when (buffer-live-p diff-buf)
            (kill-buffer diff-buf))
          (when (and review-file (file-exists-p review-file))
            (when (y-or-n-p "Delete review file from disk? ")
              (delete-file review-file)))
          (remhash branch lg/review--sessions)
          (message "Review for %s cancelled" branch)))))

  ;; --- Keybindings ---

  ;; SPC g r was bound to +vc-gutter/revert-hunk. Reclaim the prefix for review.
  (map! :leader "g r" nil)
  (map! :leader
        (:prefix ("g" . "git")
         (:prefix ("r" . "review")
          :desc "Start review"    "r" #'lg/review-start
          :desc "Resume review"   "c" #'lg/review-resume
          :desc "Cancel review"   "k" #'lg/review-cancel
          :desc "Diff only"       "d" #'lg/review-diff
          :desc "Publish"         "p" #'lg/review-publish))))

;; evil-snipe registers as a minor mode (higher keymap precedence than major mode),
;; so s/S would be captured by snipe before reaching lg/review-diff-mode-map.
;; Disable snipe in this mode the same way doom does for magit buffers.
(after! evil-snipe
  (add-to-list 'evil-snipe-disabled-modes 'lg/review-diff-mode))

;; C/D bindings on standard magit diff buffers (used by lg/review-diff)
(after! magit-diff
  (define-key magit-diff-section-base-map (kbd "C") #'lg/review-comment)
  (define-key magit-diff-section-base-map (kbd "D") #'lg/review-delete-comment)
  (evil-define-key* 'visual magit-diff-mode-map
    "C" #'lg/review-comment))
