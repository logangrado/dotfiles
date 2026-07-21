;;; ../.dotfiles/doom.d/packages/00-worktree.el -*- lexical-binding: t; -*-

;; Worktree-aware utilities for centaur-tabs, ibuffer, and doom-modeline.
;; In a git worktree the `.git` entry is a *file* containing
;; "gitdir: <main-repo>/.git/worktrees/<name>", whereas in the main
;; tree it is a directory.

(defvar lg/worktree--cache (make-hash-table :test 'equal)
  "Cache mapping project-root -> (worktree-name . main-repo-name).
Cleared when projectile switches projects.")

(defun lg/worktree--invalidate-cache (&rest _)
  "Drop the worktree cache (called on project switch)."
  (clrhash lg/worktree--cache))

(add-hook 'projectile-after-switch-project-hook #'lg/worktree--invalidate-cache)

(defvar lg/worktree--project-root-cache (make-hash-table :test 'equal)
  "default-directory -> (TIMESTAMP . ROOT) cache for `projectile-project-root'.
`lg/worktree--lookup' is called on every doom-modeline redisplay (via the
`worktree' segment), and needs a root before it can even check its own
result cache below — without this, that's a fresh `projectile-project-root'
call (which walks up the filesystem looking for project markers) on every
redisplay.")

(defconst lg/worktree--project-root-ttl 2.0
  "Seconds a `lg/worktree--project-root-cache' entry stays valid.")

(defun lg/worktree--cached-project-root ()
  "Like `projectile-project-root', cached per `default-directory' for
`lg/worktree--project-root-ttl' seconds."
  (let* ((key default-directory)
         (cached (gethash key lg/worktree--project-root-cache)))
    (if (and cached (< (- (float-time) (car cached)) lg/worktree--project-root-ttl))
        (cdr cached)
      (let ((root (projectile-project-root)))
        (puthash key (cons (float-time) root) lg/worktree--project-root-cache)
        root))))

(defun lg/worktree--lookup ()
  "Return (WORKTREE-NAME . MAIN-REPO-NAME) for the current project root.
WORKTREE-NAME is nil when in the main tree."
  (when-let* ((root (lg/worktree--cached-project-root)))
    (let ((cached (gethash root lg/worktree--cache 'miss)))
      (if (not (eq cached 'miss))
          cached
        (let* ((git-path (expand-file-name ".git" root))
               (result
                (cond
                 ;; Main tree: .git is a directory
                 ((file-directory-p git-path)
                  (cons nil (file-name-nondirectory (directory-file-name root))))
                 ;; Worktree: .git is a file
                 ((file-regular-p git-path)
                  (with-temp-buffer
                    (insert-file-contents git-path)
                    (if (looking-at "gitdir: \\(.+\\)")
                        (let ((gitdir (match-string 1)))
                          ;; gitdir looks like: <path>/.git/worktrees/<name>
                          (if (string-match "/\\.git/worktrees/\\([^/]+\\)\\'" gitdir)
                              (let* ((wt-name (match-string 1 gitdir))
                                     ;; main repo root is parent of the .git dir
                                     (git-common (replace-match "" t t gitdir))
                                     (main-root (file-name-nondirectory
                                                 (directory-file-name git-common))))
                                (cons wt-name main-root))
                            ;; .git file but not a worktree pattern — treat as main
                            (cons nil (file-name-nondirectory
                                       (directory-file-name root)))))
                      ;; Unrecognised .git file
                      (cons nil (file-name-nondirectory
                                 (directory-file-name root))))))
                 ;; No .git at all
                 (t (cons nil (projectile-project-name))))))
          (puthash root result lg/worktree--cache)
          result)))))

;;;###autoload
(defun lg/git-worktree-p ()
  "Return non-nil if the current project root is a git worktree."
  (car (lg/worktree--lookup)))

;;;###autoload
(defun lg/git-worktree-name ()
  "Return the worktree name, or nil when in the main tree."
  (car (lg/worktree--lookup)))

;;;###autoload
(defun lg/git-main-repo-name ()
  "Return the main repository name, even from inside a worktree."
  (cdr (lg/worktree--lookup)))

;;;###autoload
(defun lg/project-display-name ()
  "Return a display name for the current project.
Main tree  -> \"<repo>\"
Worktree   -> \"<repo>:<worktree>\""
  (let ((info (lg/worktree--lookup)))
    (if (car info)
        (concat (cdr info) ":" (car info))
      (cdr info))))


;; ---------------------------------------------------------------------------
;; Doom-modeline: worktree indicator segment
;; ---------------------------------------------------------------------------
(after! doom-modeline
  (doom-modeline-def-segment worktree
    "Show the worktree name when editing inside a git worktree."
    (when-let* ((wt (lg/git-worktree-name)))
      (concat
       (doom-modeline-spc)
       (propertize (concat (nerd-icons-mdicon "nf-md-file_tree" :face 'doom-modeline-warning)
                          " " wt " ")
                   'face 'doom-modeline-warning
                   'help-echo (format "Git worktree: %s" wt)))))

  ;; Redefine the main modeline to include worktree segment before vcs.
  ;; This is the default Doom modeline with `worktree` inserted.
  (doom-modeline-def-modeline 'main
    '(eldoc bar workspace-name window-number modals matches follow
      buffer-info remote-host buffer-position word-count parrot
      selection-info)
    '(compilation objed-state misc-info battery grip irc mu4e gnus
      github debug repl lsp minor-modes input-method indent-info
      buffer-encoding major-mode process worktree vcs check time)))
