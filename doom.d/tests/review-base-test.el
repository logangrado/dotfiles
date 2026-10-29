;;; review-base-test.el --- Review base selection -*- lexical-binding: t; -*-
;; Run: emacs -Q --batch -l doom.d/tests/review-base-test.el
;;                         -f ert-run-tests-batch-and-exit
(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)

;; Mocking completing-read must not require a native compiler.
(setq native-comp-enable-subr-trampolines nil)

;; Load only the two helpers, without evaluating Doom's after! wrapper.
(with-temp-buffer
  (insert-file-contents
   (expand-file-name "../packages/review.el"
                     (file-name-directory (or load-file-name buffer-file-name))))
  (dolist (form (cddr (read (current-buffer))))
    (when (and (eq (car-safe form) 'defun)
               (memq (cadr form) '(lg/review--base-ref lg/review--read-base-ref)))
      (eval form t))))

(ert-deftest lg/review-base-preserves-origin-head ()
  (cl-letf (((symbol-function 'magit-git-string)
             (lambda (&rest _) "refs/remotes/origin/trunk"))
            ((symbol-function 'magit-rev-verify)
             (lambda (&rest _) (ert-fail "Origin HEAD should take precedence"))))
    (should (equal "origin/trunk" (lg/review--base-ref)))))

(ert-deftest lg/review-base-prefers-origin-over-local ()
  (cl-letf (((symbol-function 'magit-git-string) (lambda (&rest _) nil))
            ((symbol-function 'magit-rev-verify)
             (lambda (ref) (member ref '("origin/dev" "refs/heads/main")))))
    (should (equal "origin/dev" (lg/review--base-ref)))))

(ert-deftest lg/review-base-falls-back-to-local-branches ()
  (dolist (case '(("main" "refs/heads/main")
                  ("master" "refs/heads/master")
                  ("dev" "refs/heads/dev")
                  ("main" "refs/heads/dev" "refs/heads/master" "refs/heads/main")))
    (cl-letf (((symbol-function 'magit-git-string) (lambda (&rest _) nil))
              ((symbol-function 'magit-rev-verify)
               (lambda (ref) (member ref (cdr case)))))
      (should (equal (car case) (lg/review--base-ref))))))

(ert-deftest lg/review-base-prompts-without-default ()
  (cl-letf (((symbol-function 'magit-git-string) (lambda (&rest _) nil))
            ;; A tag named main must not become a local branch default.
            ((symbol-function 'magit-rev-verify)
             (lambda (ref) (equal ref "main")))
            ((symbol-function 'magit-list-branch-names)
             (lambda () '("topic" "trunk")))
            ((symbol-function 'completing-read)
             (lambda (prompt candidates _predicate match _initial _history default)
               (should (equal "Base branch: " prompt))
               (should (equal '("topic" "trunk") candidates))
               (should match)
               (should-not default)
               "trunk")))
    (should-not (lg/review--base-ref))
    (should (equal "trunk" (lg/review--read-base-ref)))))

(ert-deftest lg/review-base-empty-selection-needs-a-default ()
  (cl-letf (((symbol-function 'lg/review--base-ref) (lambda () nil))
            ((symbol-function 'magit-list-branch-names) (lambda () '("trunk")))
            ((symbol-function 'completing-read) (lambda (&rest _) "")))
    (should-error (lg/review--read-base-ref) :type 'user-error)))

(ert-deftest lg/review-base-empty-selection-uses-default ()
  (cl-letf (((symbol-function 'lg/review--base-ref) (lambda () "main"))
            ((symbol-function 'magit-list-branch-names) (lambda () '("main" "topic")))
            ((symbol-function 'completing-read)
             (lambda (prompt _candidates _predicate _match _initial _history default)
               (should (equal "Base branch (default main): " prompt))
               (should (equal "main" default))
               "")))
    (should (equal "main" (lg/review--read-base-ref)))))

(ert-deftest lg/review-base-explicit-choice-overrides-default ()
  (cl-letf (((symbol-function 'lg/review--base-ref) (lambda () "main"))
            ((symbol-function 'magit-list-branch-names) (lambda () '("main" "trunk")))
            ((symbol-function 'completing-read) (lambda (&rest _) "trunk")))
    (should (equal "trunk" (lg/review--read-base-ref)))))

(defun lg/review-test--git (&rest args)
  (with-temp-buffer
    (should (zerop (apply #'process-file "git" nil t nil args)))
    (string-trim (buffer-string))))

(ert-deftest lg/review-base-real-local-repo-and-origin-precedence ()
  (skip-unless (and (executable-find "git") (require 'magit nil t)))
  (let ((dir (make-temp-file "review-base-" t))
        (process-environment
         (append '("GIT_CONFIG_GLOBAL=/dev/null" "GIT_CONFIG_SYSTEM=/dev/null")
                 process-environment)))
    (unwind-protect
        (let ((default-directory (file-name-as-directory dir)))
          (lg/review-test--git "init" "-q" "-b" "topic")
          (lg/review-test--git "-c" "user.name=Test" "-c" "user.email=test@example.com"
                               "commit" "-qm" "base" "--allow-empty")
          (should (equal "" (lg/review-test--git "remote")))
          (should-not (lg/review--base-ref))
          (lg/review-test--git "branch" "dev")
          (should (equal "dev" (lg/review--base-ref)))
          (lg/review-test--git "branch" "master")
          (should (equal "master" (lg/review--base-ref)))
          (lg/review-test--git "branch" "main")
          (should (equal "main" (lg/review--base-ref)))
          (lg/review-test--git "update-ref" "refs/remotes/origin/master" "HEAD")
          (should (equal "origin/master" (lg/review--base-ref)))
          (lg/review-test--git "update-ref" "refs/remotes/origin/main" "HEAD")
          (should (equal "origin/main" (lg/review--base-ref)))
          (lg/review-test--git "symbolic-ref" "refs/remotes/origin/HEAD"
                               "refs/remotes/origin/master")
          (should (equal "origin/master" (lg/review--base-ref))))
      (delete-directory dir t))))

;;; review-base-test.el ends here
