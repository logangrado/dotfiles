;;; ../.dotfiles/doom.d/packages/persp-worktree.el -*- lexical-binding: t; -*-

;; First-class worktree switching: each worktree of a repo is its own real
;; perspective, named via `lg/worktree-persp-name' ("repo" for the main
;; tree, "repo:worktree" for a linked one) — so switching worktrees gets
;; its own window layout for free from persp-mode, instead of just
;; swapping a buffer in the current window.
;;
;; Two distinct operations, both scoped to the current repo's worktrees:
;;  - "open a file in a worktree" (`lg/worktree-switch', SPC p w) always
;;    prompts, like `projectile-find-file' rooted elsewhere.
;;  - "switch to a worktree" (`lg/worktree-quick-switch' and friends) jumps
;;    to whatever perspective you were last looking at there — no file
;;    prompt unless the worktree has no perspective yet.
;;
;; Public surface (what other files in this config may depend on — keybinds
;; live in keybindings.el, tab-bar composition in persp.el; everything else
;; here is private implementation detail and may change freely):
;;  - Naming: `lg/worktree-persp-name'
;;  - Commands: `lg/worktree-switch', `lg/worktree-quick-switch',
;;    `lg/worktree-kill', `lg/worktree-move-left'/`-right',
;;    `lg/worktree-switch-left'/`-right', `lg/worktree-switch-to-N',
;;    `lg/repo-switch-to-repo', `lg/repo-switch-left'/`-right',
;;    `lg/repo-switch-to-N', `lg/worktree-move-buffer-to-path'
;;  - Queries/rendering: `lg/worktree-list', `lg/worktree-open-list',
;;    `lg/worktree-repo-list', `lg/worktree-persp-repo', `lg/worktree-root-p',
;;    `lg/worktree-display-name', `lg/worktree-current-path',
;;    `lg/worktree-bar-formatted'

(defvar lg/worktree--root-p-cache (make-hash-table :test 'equal)
  "PATH -> (TIMESTAMP . RESULT) cache for `lg/worktree-root-p'.
Called once per worktree from several different functions
\(`lg/worktree--repo-key', `lg/worktree-persp-name', `lg/worktree-display-name',
`lg/worktree--show-bar-p'), each on every tab-bar redisplay — without a
shared cache, that's a `file-directory-p' stat call per worktree per call
site per redisplay.")

(defconst lg/worktree--root-p-ttl 5.0
  "Seconds a `lg/worktree--root-p-cache' entry stays valid.
Longer than `lg/worktree--raw-list-ttl': whether a given path is a repo's
main worktree essentially never changes during a session.")

(defun lg/worktree-root-p (path)
  "Non-nil if PATH is the main worktree (its `.git' is a directory),
as opposed to a linked worktree (whose `.git' is a gitdir-pointer file).
Cached per PATH for `lg/worktree--root-p-ttl' seconds."
  (let ((cached (gethash path lg/worktree--root-p-cache)))
    (if (and cached (< (- (float-time) (car cached)) lg/worktree--root-p-ttl))
        (cdr cached)
      (let ((result (file-directory-p (expand-file-name ".git" path))))
        (puthash path (cons (float-time) result) lg/worktree--root-p-cache)
        result))))

(defun lg/worktree-display-name (path)
  "Short display name for worktree PATH: \"root\" for the main worktree
\(rather than the repo name, which is confusing next to sibling worktree
names), otherwise its directory name."
  (if (lg/worktree-root-p path) "root" (file-name-nondirectory path)))

(defun lg/worktree--head-file (path)
  "Return the path to the `HEAD' file that tracks worktree PATH's checked-out
branch. For the main worktree, PATH's own `.git' is a directory and
\"PATH/.git/HEAD\" is it directly. For a linked worktree, PATH's `.git' is a
plain text file containing a `gitdir: X' line pointing at
\"ROOT/.git/worktrees/NAME\", whose own `HEAD' file is the one that matters.
No subprocess either way, just small local file reads."
  (let ((dot-git (expand-file-name ".git" path)))
    (if (file-directory-p dot-git)
        (expand-file-name "HEAD" dot-git)
      (with-temp-buffer
        (insert-file-contents dot-git)
        (goto-char (point-min))
        (when (looking-at "gitdir: \\(.+\\)$")
          (expand-file-name "HEAD" (string-trim (match-string 1))))))))

(defun lg/worktree--branch-of (path)
  "Return worktree PATH's checked-out branch name, or nil if detached.
Reads `HEAD' directly (see `lg/worktree--head-file'); no subprocess, no
caching needed since it's a single small local file read."
  (when-let* ((head-file (lg/worktree--head-file path))
              ((file-readable-p head-file)))
    (with-temp-buffer
      (insert-file-contents head-file)
      (goto-char (point-min))
      (when (looking-at "ref: refs/heads/\\(.+\\)$")
        (match-string 1)))))

(defun lg/worktree--repo-root-of (path)
  "Return the main worktree's path for the repo that worktree PATH belongs
to. For the main worktree, that's PATH itself. For a linked worktree, it's
the `ROOT' in `.git's `gitdir: ROOT/.git/worktrees/NAME' line. Used as a
stable per-repo key for `lg/worktree--order-table', in place of the
git-list-derived `lg/worktree--repo-key' -- this version works even when
the repo's root worktree has no persp open at all."
  (let ((dot-git (expand-file-name ".git" path)))
    (if (file-directory-p dot-git)
        path
      (with-temp-buffer
        (insert-file-contents dot-git)
        (goto-char (point-min))
        (when (looking-at "gitdir: \\(.+\\)$")
          (let ((gitdir (string-trim (match-string 1))))
            (directory-file-name
             (replace-regexp-in-string "/\\.git/worktrees/[^/]+/?\\'" "" gitdir))))))))

(defun lg/worktree--porcelain-list ()
  "Return an ordered list of (PATH . BRANCH) for the current repo's
worktrees, PATH with no trailing slash, BRANCH nil if detached.

Parses `git worktree list --porcelain' directly in a single subprocess
call, rather than going through `magit-list-worktrees' — which calls
`magit-toplevel' (a separate `git rev-parse --show-toplevel' subprocess)
once per worktree to canonicalize each path to the exact form magit uses
as an internal cache key elsewhere. This callsite only needs path and
branch, both already present in the porcelain output, so it can skip
that canonicalization and its N extra subprocesses entirely."
  (require 'magit)
  (let (worktrees path branch)
    (dolist (line (magit-git-lines "worktree" "list" "--porcelain"))
      (cond
       ((string-prefix-p "worktree " line)
        (when path (push (cons path branch) worktrees))
        (setq path (directory-file-name (substring line 9))
              branch nil))
       ((string-prefix-p "branch refs/heads/" line)
        (setq branch (substring line 18)))))
    (when path (push (cons path branch) worktrees))
    (nreverse worktrees)))

(defvar lg/worktree--raw-list-cache (make-hash-table :test 'equal)
  "default-directory -> (TIMESTAMP . RESULT) cache for `lg/worktree--raw-list'.
`lg/worktree--raw-list' sits underneath `lg/worktree-bar-formatted', which
is part of `tab-bar-format' and so gets called on every redisplay —
without caching, that's a git subprocess on every keystroke.")

(defconst lg/worktree--raw-list-ttl 1.0
  "Seconds a `lg/worktree--raw-list-cache' entry stays valid.
Short enough that adding/removing a worktree elsewhere is reflected
almost immediately, long enough to absorb redisplay-frequency calls.")

(defun lg/worktree--raw-list ()
  "Return an alist of (LABEL . PATH) for worktrees of the current repo,
in `git worktree list' order (before any user reordering is applied).
Cached per `default-directory' for `lg/worktree--raw-list-ttl' seconds."
  (require 'magit)
  (let* ((key default-directory)
         (cached (gethash key lg/worktree--raw-list-cache)))
    (if (and cached (< (- (float-time) (car cached)) lg/worktree--raw-list-ttl))
        (cdr cached)
      (let ((result (mapcar (lambda (wt)
                               (let* ((path (car wt))
                                      (branch (cdr wt))
                                      (label (format "%s%s"
                                                      (lg/worktree-display-name path)
                                                      (if branch (format " (%s)" branch) ""))))
                                 (cons label path)))
                             (lg/worktree--porcelain-list))))
        (puthash key (cons (float-time) result) lg/worktree--raw-list-cache)
        result))))

(defvar lg/worktree--order-table (make-hash-table :test 'equal)
  "Per-repo custom worktree tab order: main-repo-root path -> ordered
list of worktree paths, set by `lg/worktree-move-left'/`-right'.")

(defun lg/worktree--repo-key (raw-worktrees)
  "Stable key identifying the repo RAW-WORKTREES (as from
`lg/worktree--raw-list') belong to: the root worktree's path, which
doesn't change when the tab order does."
  (cdr (cl-find-if (lambda (wt) (lg/worktree-root-p (cdr wt))) raw-worktrees)))

(defun lg/worktree-list ()
  "Return an alist of (LABEL . PATH) for worktrees of the current repo,
in the user's custom tab order if one has been established via
`lg/worktree-move-left'/`-right', else in `magit-list-worktrees' order.
Worktrees added or removed since the order was set are reconciled
automatically: unknown paths are appended in `magit-list-worktrees' order."
  (let* ((raw (lg/worktree--raw-list))
         (key (lg/worktree--repo-key raw))
         (order (and key (gethash key lg/worktree--order-table))))
    (if (not order)
        raw
      (append
       (delq nil (mapcar (lambda (path) (cl-find-if (lambda (wt) (string= (cdr wt) path)) raw))
                          order))
       (cl-remove-if (lambda (wt) (member (cdr wt) order)) raw)))))

(defun lg/worktree-jump-to-path (path)
  "Open a buffer rooted at worktree PATH, in the current perspective."
  (let ((default-directory (file-name-as-directory path)))
    (condition-case nil
        (projectile-find-file)
      (error (dired default-directory)))))

(defun lg/worktree--repo-name ()
  "Directory name of the current buffer's repo's main worktree.
Reuses `lg/worktree--raw-list' for `default-directory' as-is, rather than
rebinding it to some other worktree's path and fetching a second, separately
-cached raw list — every caller only ever needs this for a worktree of the
*current* repo, so the current context's (already-cached) raw list already
contains it. This is called once per worktree from `lg/worktree-persp-name',
which `lg/worktree-open-list' calls per worktree on every redisplay via the
tab bar, so a fresh `magit-list-worktrees' call per worktree here would
defeat the `lg/worktree--raw-list' cache almost entirely."
  (file-name-nondirectory
   (directory-file-name
    (cdr (cl-find-if (lambda (wt) (lg/worktree-root-p (cdr wt))) (lg/worktree--raw-list))))))

(defun lg/worktree-persp-name (path)
  "The persp name for worktree PATH: \"repo\" for the main worktree, or
\"repo:worktree\" for a linked one. PATH is always a project root (from
`magit-list-worktrees'), so this resolves directly without an upward
directory search."
  (if (lg/worktree-root-p path)
      (lg/worktree--repo-name)
    (format "%s:%s" (lg/worktree--repo-name) (lg/worktree-display-name path))))

(defvar lg/worktree--persp-path-table (make-hash-table :test 'equal)
  "Persp name -> worktree path, recorded once by `lg/worktree--persp-switch'
at the moment a worktree's persp is switched to. Lets `lg/worktree-open-list'
recover each open worktree's path without ever asking git for it: the path
is already known for certain at every callsite that switches to a
worktree persp, so there's nothing to look up later. Stale entries for
closed persps are harmless -- persp names are a deterministic function of
path (`lg/worktree-persp-name'), so a reopened persp just gets the same
entry again.")

(defun lg/worktree--persp-switch (name path)
  "Switch to persp NAME (for worktree PATH), auto-creating it (as an empty
perspective) if it doesn't exist yet. Records PATH in
`lg/worktree--persp-path-table' so it can be recovered later without git."
  (puthash name path lg/worktree--persp-path-table)
  (+workspace-switch name t))

(defun lg/worktree--maybe-record-current-path (&rest _)
  "Backfill `lg/worktree--persp-path-table' for the current persp if it has
no entry yet. Covers persps that became current without ever going
through `lg/worktree--persp-switch' — chiefly the persp Emacs starts you
in (e.g. root), which persp-mode activates on its own. Hooked onto the
same buffer/selection-change events as `lg/refresh-workspace-tab-bar-light'
in persp.el (cheap: a hash lookup, plus `lg/worktree-current-path' only
once that lookup misses), so it catches the path as soon as the current
buffer lands inside a project, without needing git."
  (let ((name (safe-persp-name (get-current-persp))))
    (unless (or (string= name persp-nil-name)
                (gethash name lg/worktree--persp-path-table))
      (when-let* ((path (lg/worktree-current-path)))
        (puthash name path lg/worktree--persp-path-table)))))

(add-hook 'window-buffer-change-functions #'lg/worktree--maybe-record-current-path)
(add-hook 'window-selection-change-functions #'lg/worktree--maybe-record-current-path)

;; ---------------------------------------------------------------------------
;; Repo axis: dedupe the tab bar on repo, and remember which worktree of a
;; repo you were last looking at, so switching back to a repo (SPC TAB) lands
;; you where you left off rather than always at its root.
;; ---------------------------------------------------------------------------
(defun lg/worktree-persp-repo (persp-name)
  "Extract the repo portion of PERSP-NAME (\"repo\" or \"repo:worktree\")."
  (car (split-string persp-name ":" t)))

(defvar lg/worktree--repo-mru (make-hash-table :test 'equal)
  "Per-repo MRU: repo name -> last persp name visited for that repo.")

(defun lg/worktree--record-repo-mru (&optional _type _frame-or-window persp)
  "Record the current persp as the MRU entry for its repo.
Added to `persp-activated-functions', which calls its functions with
\(TYPE FRAME-OR-WINDOW PERSP) after a switch completes -- not
`persp-switch-hook', which isn't a real persp-mode hook variable."
  (when-let* ((name (safe-persp-name (or persp (get-current-persp))))
              ((not (string= name persp-nil-name))))
    (puthash (lg/worktree-persp-repo name) name lg/worktree--repo-mru)))

(add-hook 'persp-activated-functions #'lg/worktree--record-repo-mru)

(defun lg/worktree-repo-list ()
  "Open repos, deduped, in first-seen order of `+workspace-list-names'."
  (let (seen result)
    (dolist (name (+workspace-list-names))
      (unless (string= name persp-nil-name)
        (let ((repo (lg/worktree-persp-repo name)))
          (unless (member repo seen)
            (push repo seen)
            (push repo result)))))
    (nreverse result)))

(defun lg/worktree--repo-target (repo)
  "Persp name to switch to for REPO: its MRU persp if still open, else
the first open persp belonging to it."
  (let* ((mru (gethash repo lg/worktree--repo-mru))
         (mru-open (and mru (+workspace-exists-p mru))))
    (if mru-open
        mru
      (cl-find-if (lambda (name) (string= (lg/worktree-persp-repo name) repo))
                  (+workspace-list-names)))))

;;;###autoload
(defun lg/repo-switch-to-repo (repo)
  "Switch to REPO: its MRU worktree persp if one is open, else its first
open persp. See `lg/worktree--repo-target'."
  (when-let* ((target (lg/worktree--repo-target repo)))
    (+workspace-switch target)))

(defun lg/repo-switch-to-number (n)
  "Switch to the Nth (1-indexed) open repo, in the order shown in the tab bar."
  (when-let* ((repo (nth (1- n) (lg/worktree-repo-list))))
    (lg/repo-switch-to-repo repo)))

(dotimes (i 9)
  (let ((n (1+ i)))
    (defalias (intern (format "lg/repo-switch-to-%d" n))
      (lambda () (interactive) (lg/repo-switch-to-number n))
      (format "Switch to repo #%d in the tab bar." n))))

(defun lg/repo-switch-relative (delta)
  "Switch to the open repo DELTA positions away from the current one,
in `lg/worktree-repo-list' order, wrapping around."
  (let* ((repos (lg/worktree-repo-list))
         (n (length repos)))
    (when (> n 0)
      (let* ((current (lg/worktree-persp-repo (safe-persp-name (get-current-persp))))
             (pos (or (cl-position current repos :test #'string=) 0))
             (target (mod (+ pos delta) n)))
        (lg/repo-switch-to-repo (nth target repos))))))

;;;###autoload
(defun lg/repo-switch-left ()
  "Switch to the previous repo in the tab bar (wraps around)."
  (interactive)
  (lg/repo-switch-relative -1))

;;;###autoload
(defun lg/repo-switch-right ()
  "Switch to the next repo in the tab bar (wraps around)."
  (interactive)
  (lg/repo-switch-relative 1))

;;;###autoload
(defun lg/worktree-switch ()
  "Open a file in a different worktree of the current repo, each worktree
its own perspective. Always opens the picked file, whether or not the
perspective already existed."
  (interactive)
  (let* ((worktrees (lg/worktree-list))
         (choice (completing-read "Open file in worktree: " (mapcar #'car worktrees))))
    (when-let* ((path (cdr (assoc choice worktrees))))
      (lg/worktree--persp-switch (lg/worktree-persp-name path) path)
      (lg/worktree-jump-to-path path))))

(defun lg/worktree-switch-to-path (path)
  "Switch to the perspective for worktree PATH, giving it its own window
layout like switching perspectives. Seeds a brand-new perspective with a
file/dired buffer via `lg/worktree-jump-to-path'; an existing one is left
showing whatever it last had open."
  (let* ((name (lg/worktree-persp-name path))
         (existed (+workspace-exists-p name)))
    (lg/worktree--persp-switch name path)
    (unless existed
      (lg/worktree-jump-to-path path))))

(defun lg/worktree-open-list ()
  "Like `lg/worktree-list', but only worktrees that have a perspective open.
Mirrors how the persp tab bar only lists currently-open perspectives rather
than every project you could switch to — the worktree bar and the switch/
number/left-right commands all work off this \"what's open\" list, while
`lg/worktree-switch' (open a file) still offers every worktree, since its
whole point is to open one you haven't visited yet.

Unlike `lg/worktree-list', never touches git: this is on the redisplay-hot
path (via `lg/worktree-bar-formatted', part of `tab-bar-format'), so it's
built entirely from already-open persps and cheap local file reads. Open
persps for the current repo come from `+workspace-list-names' (already
git-free, same as `lg/worktree-repo-list'); each one's path comes from
`lg/worktree--persp-path-table' (recorded by `lg/worktree--persp-switch'
whenever we switched there); each path's branch comes from
`lg/worktree--branch-of' (a `HEAD' file read, no subprocess). A persp
with no recorded path (never switched-to via this code, e.g. left over
from before it existed) is dropped rather than falling back to git --
switching to it once repopulates the table."
  (let* ((repo (lg/worktree-persp-repo (safe-persp-name (get-current-persp))))
         (names (cl-remove-if-not
                 (lambda (name)
                   (and (not (string= name persp-nil-name))
                        (string= (lg/worktree-persp-repo name) repo)))
                 (+workspace-list-names)))
         (worktrees
          (delq nil
                (mapcar
                 (lambda (name)
                   (when-let* ((path (gethash name lg/worktree--persp-path-table)))
                     (let* ((branch (lg/worktree--branch-of path))
                            (label (format "%s%s"
                                           (lg/worktree-display-name path)
                                           (if branch (format " (%s)" branch) ""))))
                       (cons label path))))
                 names)))
         (key (and worktrees (lg/worktree--repo-root-of (cdr (car worktrees)))))
         (order (and key (gethash key lg/worktree--order-table))))
    (if (not order)
        worktrees
      (append
       (delq nil (mapcar (lambda (path) (cl-find-if (lambda (wt) (string= (cdr wt) path)) worktrees))
                          order))
       (cl-remove-if (lambda (wt) (member (cdr wt) order)) worktrees)))))

(defun lg/worktree-current-path ()
  "Return the worktree path the current buffer is rooted in, or nil."
  (and (fboundp 'projectile-project-root)
       (when-let* ((root (projectile-project-root)))
         (directory-file-name (expand-file-name root)))))

(defun lg/worktree--current-index (worktrees)
  "Return the 0-based index of the current buffer's worktree within WORKTREES
\(an alist as returned by `lg/worktree-list'), or nil."
  (when-let* ((current (lg/worktree-current-path)))
    (cl-position-if (lambda (wt) (string= (expand-file-name (cdr wt)) current))
                     worktrees)))

;;;###autoload
(defun lg/worktree-quick-switch ()
  "Switch to an already-open worktree of the current repo, within this
perspective. Type to select; only offers worktrees with a buffer open
already, since there's nothing to \"switch to\" otherwise — use
`lg/worktree-switch' to open one you haven't visited yet."
  (interactive)
  (let* ((worktrees (lg/worktree-open-list))
         (choice (completing-read "Switch to worktree: " (mapcar #'car worktrees))))
    (when-let* ((path (cdr (assoc choice worktrees))))
      (lg/worktree-switch-to-path path))))

(defun lg/worktree-switch-relative (delta)
  "Switch to the open worktree DELTA positions away from the current one,
in the canonical `lg/worktree-open-list' order, wrapping around."
  (let* ((worktrees (lg/worktree-open-list))
         (n (length worktrees)))
    (when (> n 0)
      (let* ((current (or (lg/worktree--current-index worktrees) 0))
             (target (mod (+ current delta) n)))
        (lg/worktree-switch-to-path (cdr (nth target worktrees)))))))

;;;###autoload
(defun lg/worktree-switch-left ()
  "Switch to the previous worktree of the current repo (wraps around)."
  (interactive)
  (lg/worktree-switch-relative -1))

;;;###autoload
(defun lg/worktree-switch-right ()
  "Switch to the next worktree of the current repo (wraps around)."
  (interactive)
  (lg/worktree-switch-relative 1))

(defun lg/worktree-switch-to-number (n)
  "Switch to the Nth (1-indexed) open worktree of the current repo,
in the same order shown in the worktree bar."
  (let* ((worktrees (lg/worktree-open-list)))
    (when-let* ((wt (nth (1- n) worktrees)))
      (lg/worktree-switch-to-path (cdr wt)))))

(dotimes (i 9)
  (let ((n (1+ i)))
    (defalias (intern (format "lg/worktree-switch-to-%d" n))
      (lambda () (interactive) (lg/worktree-switch-to-number n))
      (format "Switch to worktree #%d of the current repo." n))))

(defun lg/worktree-move (delta)
  "Move the current worktree DELTA positions in the tab order, swapping it
with its neighbor *among open worktrees* — the ones actually visible and
reorderable in the bar. Persists per-repo in `lg/worktree--order-table',
like `+workspace/swap-left'/`-right' does for persps.

Swap targets are chosen from `lg/worktree-open-list', not the full
`lg/worktree-list': the repo may have other worktrees that aren't open
in this persp, and swapping against one of those would silently update
the stored order without moving anything the bar shows."
  (let* ((open (lg/worktree-open-list))
         (n (length open))
         (current (lg/worktree--current-index open)))
    (when (and current (> n 1))
      (let* ((target (mod (+ current delta) n))
             (current-path (cdr (nth current open)))
             (target-path (cdr (nth target open)))
             (key (lg/worktree--repo-root-of current-path))
             (paths (mapcar #'cdr (lg/worktree-list)))
             (i (cl-position current-path paths :test #'string=))
             (j (cl-position target-path paths :test #'string=)))
        (when (and key i j)
          (cl-rotatef (nth i paths) (nth j paths))
          (puthash key paths lg/worktree--order-table)
          (when (fboundp 'lg/refresh-workspace-tab-bar) (lg/refresh-workspace-tab-bar)))))))

;;;###autoload
(defun lg/worktree-move-left ()
  "Move the current worktree one position left in the tab order."
  (interactive)
  (lg/worktree-move -1))

;;;###autoload
(defun lg/worktree-move-right ()
  "Move the current worktree one position right in the tab order."
  (interactive)
  (lg/worktree-move 1))

;;;###autoload
(defun lg/worktree-kill ()
  "Kill the perspective for the current worktree, dropping it from the
worktree bar. Only closes the view: doesn't touch the worktree, its
branch, or anything on disk. If another worktree of this repo has a
perspective open, switches there first; otherwise falls back to Doom's
default behavior for killing your only/last workspace."
  (interactive)
  (if-let* ((path (lg/worktree-current-path)))
      (let* ((name (lg/worktree-persp-name path))
             (other (cl-find-if (lambda (wt) (not (string= (expand-file-name (cdr wt)) path)))
                                 (lg/worktree-open-list))))
        ;; Switch away first when a sibling worktree-persp is open, so we
        ;; don't kill the persp we're currently standing in. With no
        ;; sibling, just kill it in place — `+workspace/kill' already knows
        ;; what to do when it's your only/last workspace.
        (when other
          (lg/worktree-switch-to-path (cdr other)))
        (+workspace/kill name)
        ;; Recompute after killing, not just before: whether the bar should
        ;; still show at all can depend on the worktree we just closed.
        (when (fboundp 'lg/refresh-workspace-tab-bar) (lg/refresh-workspace-tab-bar)))
    (message "Not inside a git worktree.")))

;;;###autoload
(defun lg/worktree-move-buffer-to-path (buffer path)
  "Reassign BUFFER from its current perspective to the perspective for
worktree PATH, then return to the source perspective — moving a buffer
out of the way shouldn't make you follow it. If the target perspective
already exists, BUFFER is added to it directly without ever switching
there, so we don't disrupt whatever window layout it already has.

If it doesn't exist yet, hand-building a window-conf around BUFFER
turned out to be unreliable (persp-mode's restore didn't consistently
pick it up), so instead we bootstrap the target the same proven way
`SPC p w' (`lg/worktree-switch-to-path') always has: switch there and
prompt for a file to open, which gives persp-mode an ordinary buffer to
build a real window-conf around. Once that's done, BUFFER is added and
we switch straight back to the source.

Adds BUFFER to the target before removing it from the source, so it's
never briefly persp-less in between. Also updates BUFFER's
`default-directory' to PATH, since `lg/worktree-current-path' and
friends derive \"which worktree is this buffer in\" from it via
`projectile-project-root' — left pointing at the old worktree, the tab
bar's highlight and `lg/worktree-switch-left'/`-right' would still treat
BUFFER as belonging to the source. Returns the target perspective's name."
  (let* ((name (lg/worktree-persp-name path))
         (source (get-current-persp))
         (source-name (safe-persp-name source))
         (source-path (lg/worktree-current-path)))
    (if (+workspace-exists-p name)
        (persp-add-buffer buffer (+workspace-get name) nil)
      (lg/worktree-switch-to-path path)
      (persp-add-buffer buffer (get-current-persp) nil)
      (lg/worktree--persp-switch source-name source-path))
    (persp-remove-buffer buffer source)
    (with-current-buffer buffer
      (setq default-directory (file-name-as-directory path)))
    name))

;; ---------------------------------------------------------------------------
;; Worktree bar rendering: which worktrees of the current repo are open, the
;; dual of `lg/worktree-repo-list' (which worktrees are open per repo). Kept
;; here rather than in persp.el's tab-bar wiring since it's core package
;; behavior, not personal tab-bar formatting choices.
;; ---------------------------------------------------------------------------
(defun lg/worktree--show-bar-p (worktrees)
  "Non-nil if the worktree bar should be shown for WORKTREES (an open-list
as from `lg/worktree-open-list'). Hidden when there's nothing to
disambiguate: no worktrees open, or only the root worktree open."
  (not (or (not worktrees)
           (and (= (length worktrees) 1)
                (lg/worktree-root-p (cdr (car worktrees)))))))

(defun lg/worktree-bar-formatted ()
  "Render open worktrees of the current buffer's repo, for the tab bar.
Mirrors `lg/workspaces-formatted' (in persp.el): only worktrees with a
buffer already open show up here (via `lg/worktree-open-list'), numbered to
match `lg/worktree-switch-to-N'. Unvisited worktrees of the repo don't
clutter this row — open one with `lg/worktree-switch' (SPC p w) first.
Highlights whichever worktree the current buffer belongs to. Empty when
there's nothing to disambiguate, or the current buffer isn't inside a git
repo."
  (let ((worktrees (ignore-errors (lg/worktree-open-list)))
        (current (ignore-errors (lg/worktree-current-path))))
    (if (not (lg/worktree--show-bar-p worktrees))
        ""
      (concat
       "  "
       (let ((i 0))
         (mapconcat
          (lambda (wt)
            (cl-incf i)
            (let* ((path (cdr wt))
                   (name (lg/worktree-display-name path))
                   (is-current (and current (string= (expand-file-name path) current))))
              (concat
               (propertize (format " %d" i)
                           'face `(:inherit ,(if is-current
                                                 '+workspace-tab-selected-face
                                               '+workspace-tab-face)
                                   :weight bold))
               (propertize (format " %s " name)
                           'face (if is-current
                                     '+workspace-tab-selected-face
                                   '+workspace-tab-face)
                           'help-echo (format "Switch to worktree: %s" path)
                           'mouse-face 'highlight
                           'keymap (let ((map (make-sparse-keymap)))
                                     (define-key map [tab-bar mouse-1]
                                       (lambda () (interactive) (lg/worktree-switch-to-path path)))
                                     map)))))
          worktrees
          " "))))))
