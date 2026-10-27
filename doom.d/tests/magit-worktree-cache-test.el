;;; magit-worktree-cache-test.el --- Worktree reader regression tests -*- lexical-binding: t; -*-
;; Run with Magit and its dependencies on load-path:
;; emacs -Q --batch -l doom.d/tests/magit-worktree-cache-test.el
;;                 -f ert-run-tests-batch-and-exit

(require 'ert)
(require 'cl-lib)
(require 'magit)

(defconst lg/worktree-test--packages-directory
  (expand-file-name "../packages/"
                    (file-name-directory (or load-file-name buffer-file-name))))

(defun lg/worktree-test--load-definitions (package names)
  "Load actual definitions named NAMES from PACKAGE, without Doom setup."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name (concat package ".el") lg/worktree-test--packages-directory))
    (goto-char (point-min))
    (let (found)
      (cl-labels ((visit (form)
                    (cond
                     ((eq (car-safe form) 'use-package)
                      (mapc #'visit (cdr (memq :config form))))
                     ((and (memq (car-safe form) '(defun defvar))
                           (memq (cadr form) names))
                      (eval form t)
                      (push (cadr form) found)))))
        (condition-case nil
            (while t (visit (read (current-buffer))))
          (end-of-file nil)))
      (dolist (name names)
        (unless (memq name found)
          (error "Missing %s definition in %s" name package))))))

(defun lg/worktree-test--load-order (order)
  (dolist (package order)
    (lg/worktree-test--load-definitions
     package
     (if (equal package "magit")
         '(lg/worktree--branch-cache
           lg/worktree--porcelain-full-list-cache
           lg/magit--worktree-path-branches
           lg/worktree--refresh-cache
           lg/magit--worktree-ref-labels
           lg/worktree--porcelain-full-list
           lg/magit-insert-worktrees)
       '(lg/worktree--porcelain-list)))))

;; Declare the source's special variables before byte-compiling test bindings.
(defvar lg/worktree--branch-cache)
(defvar lg/worktree--porcelain-full-list-cache)

(defun lg/worktree-test--porcelain (branch)
  (list "worktree /repo/" "HEAD aaaaaaaaaa" "branch refs/heads/main"
        "worktree /other/" "HEAD bbbbbbbbbb" (concat "branch refs/heads/" branch)
        "worktree /detached/" "HEAD cccccccccc" "detached"
        "worktree /bare/" "bare"))

(defun lg/worktree-test--refreshes (order reader-first)
  "Exercise two warm refresh cycles with ORDER and READER-FIRST."
  (lg/worktree-test--load-order order)
  (let ((lg/worktree--branch-cache (make-hash-table :test #'equal))
        (lg/worktree--porcelain-full-list-cache nil)
        (calls 0)
        (branch "feature"))
    ;; Warm decoration cache: cold lazy initialization is not this regression.
    (puthash "/repo" '("feature") lg/worktree--branch-cache)
    (cl-letf (((symbol-function 'magit-toplevel) (lambda (&rest _) "/repo"))
              ((symbol-function 'magit-git-lines)
               (lambda (&rest args)
                 (should (equal args '("worktree" "list" "--porcelain")))
                 (cl-incf calls)
                 (lg/worktree-test--porcelain branch))))
      (dolist (next-branch '("feature" "renamed"))
        (setq branch next-branch)
        ;; A warm ref-label read must not trigger lazy initialization or Git.
        (let* ((previous-calls calls)
               (label (propertize (car (gethash "/repo" lg/worktree--branch-cache))
                                  'font-lock-face 'magit-branch-local)))
          (should (eq (get-text-property 0 'face (lg/magit--worktree-ref-labels label))
                      'magit-branch-worktree))
          (should (= previous-calls calls)))
        (when reader-first (lg/worktree--porcelain-full-list))
        (with-temp-buffer
          (magit-insert-section (status)
            (lg/magit-insert-worktrees))
          (should (string-match-p "Worktrees" (buffer-string)))
          (should (string-match-p branch (buffer-string)))
          (should (string-match-p "ccccccc" (buffer-string)))
          (should (string-match-p "(bare)" (buffer-string))))
        (should (equal (lg/magit--worktree-path-branches)
                       `(("/repo" . "main") ("/other" . ,branch)
                         ("/detached") ("/bare"))))
        ;; Model the last consumer, installed on magit-refresh-buffer-hook.
        (lg/worktree--refresh-cache)
        (should (equal (gethash "/repo" lg/worktree--branch-cache) (list branch)))
        (should-not lg/worktree--porcelain-full-list-cache)
        (should (= calls (if (equal branch "feature") 1 2)))))))

(ert-deftest lg/magit-worktree-cache-magit-then-tab-bar ()
  (lg/worktree-test--refreshes '("magit" "persp-worktree") nil))

(ert-deftest lg/magit-worktree-cache-tab-bar-then-magit ()
  (lg/worktree-test--refreshes '("persp-worktree" "magit") nil))

(ert-deftest lg/magit-worktree-cache-reader-before-section ()
  (dolist (order '(("magit" "persp-worktree") ("persp-worktree" "magit")))
    (lg/worktree-test--refreshes order t)))

(ert-deftest lg/magit-worktree-cache-tab-bar-remains-independent ()
  (dolist (order '(("magit" "persp-worktree") ("persp-worktree" "magit")))
    (lg/worktree-test--load-order order)
    (let ((lg/worktree--porcelain-full-list-cache nil)
          (calls 0)
          (branch "feature"))
      (cl-letf (((symbol-function 'magit-git-lines)
                 (lambda (&rest args)
                   (should (equal args '("worktree" "list" "--porcelain")))
                   (cl-incf calls)
                   (lg/worktree-test--porcelain branch))))
        (lg/worktree--porcelain-full-list)
        (setq branch "tab-bar-fresh")
        (should (equal (lg/worktree--porcelain-list)
                       '(("/repo" . "main") ("/other" . "tab-bar-fresh")
                         ("/detached") ("/bare"))))
        (should (= calls 2))
        ;; Independent tab-bar reads neither consume nor replace Magit's data.
        (should (equal (cdr (assoc "/other" (lg/magit--worktree-path-branches)))
                       "feature"))
        (should (= calls 2))))))

(ert-deftest lg/magit-worktree-cache-refresh-preserves-other-repos-and-cleans-up ()
  (lg/worktree-test--load-order '("magit" "persp-worktree"))
  (let ((lg/worktree--branch-cache (make-hash-table :test #'equal))
        (lg/worktree--porcelain-full-list-cache '(("/repo" "main" "aaaa" nil))))
    (puthash "/unrelated/" '("keep") lg/worktree--branch-cache)
    (cl-letf (((symbol-function 'magit-toplevel) (lambda (&rest _) "/repo")))
      (lg/worktree--refresh-cache))
    (should-not (gethash "/repo" lg/worktree--branch-cache 'missing))
    (should (equal (gethash "/unrelated/" lg/worktree--branch-cache) '("keep")))
    (should-not lg/worktree--porcelain-full-list-cache)
    ;; Keep the existing unwind-protect cleanup even outside a repo/on errors.
    (dolist (top-function (list (lambda (&rest _) nil)
                              (lambda (&rest _) (error "Refresh failed"))))
      (setq lg/worktree--porcelain-full-list-cache '(("/repo" "main" "aaaa" nil)))
      (cl-letf (((symbol-function 'magit-toplevel) top-function))
        (condition-case nil (lg/worktree--refresh-cache) (error nil)))
      (should-not lg/worktree--porcelain-full-list-cache))))

(ert-deftest lg/magit-worktree-cache-existing-trailing-slash-exclusion-caveat ()
  (lg/worktree-test--load-order '("magit" "persp-worktree"))
  (let ((lg/worktree--branch-cache (make-hash-table :test #'equal))
        (lg/worktree--porcelain-full-list-cache '(("/repo" "main" "aaaa" nil))))
    (cl-letf (((symbol-function 'magit-toplevel) (lambda (&rest _) "/repo/")))
      (lg/worktree--refresh-cache))
    ;; Characterize, do not fix here: expand-file-name retains the trailing
    ;; slash from magit-toplevel, so the existing comparison misses /repo.
    (should (equal (gethash "/repo/" lg/worktree--branch-cache) '("main")))
    (should-not lg/worktree--porcelain-full-list-cache)))

;;; magit-worktree-cache-test.el ends here
