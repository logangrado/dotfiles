;;; magit-delta-cache-test.el --- Delta refresh regression tests -*- lexical-binding: t; -*-
;; Run: emacs -Q --batch -l doom.d/tests/magit-delta-cache-test.el
;;                         -f ert-run-tests-batch-and-exit
;; Set DELTA to enable real-process tests.  Add xterm-color to load-path to
;; additionally exercise ANSI conversion and the full context wrapper.

(require 'ert)
(require 'cl-lib)
(load (expand-file-name "../magit-delta-cache.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

;; Load only the standalone context helpers, without evaluating Doom's
;; use-package!/map!/load! forms or requiring a configured Magit installation.
(let ((file (expand-file-name "../packages/magit.el"
                              (file-name-directory (or load-file-name buffer-file-name)))))
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (condition-case nil
        (while t
          (let ((form (read (current-buffer))))
            (when (and (memq (car-safe form) '(defun defvar))
                       (symbolp (cadr form))
                       (string-prefix-p "lg/magit-delta-" (symbol-name (cadr form))))
              (eval form t))))
      (end-of-file nil))))

(defvar magit-delta-delta-executable)
(defvar magit-delta-hide-plus-minus-markers)
(defconst lg/delta-test--args '("--no-gitconfig" "--color-only" "--true-color" "always"))

(defun lg/delta-test--diff (file &optional added)
  (format "diff --git a/%s b/%s\nindex 1111111..2222222 100644\n--- a/%s\n+++ b/%s\n@@ -3 +3 @@\n-old\n+%s\n"
          file file file file (or added "new")))

(defmacro lg/delta-test--with-cache (&rest body)
  (declare (indent 0))
  `(let ((lg/magit-delta--cache (make-hash-table :test #'equal))
         (lg/magit-delta--cache-order nil)
         (lg/magit-delta--cache-bytes 0)
         (lg/magit-delta-cache-max-entries lg/magit-delta-cache-max-entries)
         (lg/magit-delta-cache-max-bytes lg/magit-delta-cache-max-bytes)
         (lg/magit-delta--prefix-cache nil)
         ;; Mocking a C primitive must not require a native compiler.
         (native-comp-enable-subr-trampolines nil)
         (default-directory temporary-file-directory))
     ,@body))

(ert-deftest lg/delta-cache-splits-ansi-without-losing-bytes ()
  (let* ((a (lg/delta-test--diff "a.py"))
         (b (lg/delta-test--diff "b.py"))
         (colored (concat "\e[31m" a "\e[0m\e[32m" b "\e[0m")))
    (should (= 2 (length (lg/magit-delta--file-parts colored))))
    (should (equal colored (apply #'concat (lg/magit-delta--file-parts colored))))))

(ert-deftest lg/delta-cache-batches-misses-reuses-hits-and-file-transfers ()
  (lg/delta-test--with-cache
    (let* ((a (lg/delta-test--diff "a.py"))
           (b (lg/delta-test--diff "b.py"))
           (changed (lg/delta-test--diff "a.py" "edited"))
           calls)
      (cl-letf (((symbol-function 'lg/magit-delta--run)
                 (lambda (input _exe _args) (push input calls) input)))
        (should (equal (concat a b)
                       (lg/magit-delta--render-cached (concat a b) "delta" lg/delta-test--args)))
        (should (= 1 (length calls)))
        (should (equal (concat a b)
                       (lg/magit-delta--render-cached (concat a b) "delta" lg/delta-test--args)))
        ;; Moving a whole unchanged file to the other diff section is a hit.
        (should (equal a (lg/magit-delta--render-cached a "delta" lg/delta-test--args)))
        (should (= 1 (length calls)))
        (should (equal (concat changed b)
                       (lg/magit-delta--render-cached (concat changed b) "delta" lg/delta-test--args)))
        (should (= 2 (length calls)))
        (should (equal changed (car calls)))))))

(ert-deftest lg/delta-cache-invalidates-options-executable-directory-and-context ()
  (lg/delta-test--with-cache
    (let ((input (lg/delta-test--diff "a.py"))
          (calls 0))
      (cl-letf (((symbol-function 'lg/magit-delta--run)
                 (lambda (text _exe _args) (cl-incf calls) text)))
        (lg/magit-delta--render-cached input "delta" lg/delta-test--args)
        (lg/magit-delta--render-cached input "delta" (append lg/delta-test--args '("--light")))
        (lg/magit-delta--render-cached input "other-delta" lg/delta-test--args)
        (let ((default-directory "/"))
          (lg/magit-delta--render-cached input "delta" lg/delta-test--args))
        (lg/magit-delta--render-cached (concat input " context\n") "delta" lg/delta-test--args)
        (let ((process-environment (cons "DELTA_TEST_SETTING=changed" process-environment)))
          (lg/magit-delta--render-cached input "delta" lg/delta-test--args))
        (should (= calls 6))))))

(ert-deftest lg/delta-cache-bypasses-external-config-and-unknown-input ()
  (lg/delta-test--with-cache
    (let ((calls 0))
      (cl-letf (((symbol-function 'lg/magit-delta--run)
                 (lambda (text _exe _args) (cl-incf calls) text)))
        (dotimes (_ 2)
          (lg/magit-delta--render-cached (lg/delta-test--diff "a.py") "delta" '("--color-only"))
          (lg/magit-delta--render-cached "diff --cc merge.py\n" "delta" lg/delta-test--args))
        (should (= calls 4))
        (should (= 0 (hash-table-count lg/magit-delta--cache)))))))

(ert-deftest lg/delta-cache-format-change-falls-back-to-full-render ()
  (lg/delta-test--with-cache
    (let ((a (lg/delta-test--diff "a.py"))
          (b (lg/delta-test--diff "b.py"))
          calls)
      (cl-letf (((symbol-function 'lg/magit-delta--run)
                 (lambda (text _exe _args) (push text calls) text)))
        (lg/magit-delta--render-cached a "delta" lg/delta-test--args))
      (cl-letf (((symbol-function 'lg/magit-delta--run)
                 (lambda (text _exe _args)
                   (push text calls)
                   (concat "unexpected preamble\n" text))))
        (should (equal (concat "unexpected preamble\n" a b)
                       (lg/magit-delta--render-cached (concat a b) "delta" lg/delta-test--args)))
        (should (equal (list (concat a b) b a) calls))
        (should (= 1 (hash-table-count lg/magit-delta--cache)))))))

(ert-deftest lg/delta-cache-bounds-and-clear ()
  (lg/delta-test--with-cache
    (let ((lg/magit-delta-cache-max-entries 2)
          (lg/magit-delta-cache-max-bytes 5))
      (lg/magit-delta--cache-put 'a "aa" 2)
      (lg/magit-delta--cache-put 'b "bb" 2)
      (lg/magit-delta--cache-put 'c "ccc" 3)
      (should-not (lg/magit-delta--cache-get 'a))
      (should (= 5 lg/magit-delta--cache-bytes))
      (lg/magit-delta--cache-put 'huge "123456" 6)
      (should-not (lg/magit-delta--cache-get 'huge))
      (lg/magit-delta--cache-put 'b "b" 1)
      (should (= 4 lg/magit-delta--cache-bytes))
      (should (= 2 (hash-table-count lg/magit-delta--cache)))
      (lg/magit-delta-clear-cache)
      (should (= 0 lg/magit-delta--cache-bytes))
      (should-not lg/magit-delta--cache-order)
      (should (= 0 (hash-table-count lg/magit-delta--cache))))))

(ert-deftest lg/delta-cache-default-budget-retains-large-warm-working-set ()
  ;; Both old limits were too small for this fixture.  Inherit the real
  ;; defaults rather than supplying a test-only budget that masks regressions.
  (should (= 512 lg/magit-delta-cache-max-entries))
  (should (= (* 128 1024 1024) lg/magit-delta-cache-max-bytes))
  (lg/delta-test--with-cache
    (let* ((files 280)
           (sources 24)
           (body (make-string (* 64 1024) ?x))
           (input (mapconcat
                   (lambda (index)
                     (lg/delta-test--diff (format "large-%d.py" index) body))
                   (number-sequence 0 (1- files)) ""))
           (runs 0)
           (decodes 0)
           (decode (lambda (ansi _input _index)
                     (cl-incf decodes)
                     (lg/magit-delta--snapshot-create
                      :text (propertize ansi 'face '(:foreground "#aabbcc")))))
           cold)
      (dotimes (index sources)
        (lg/magit-delta--cache-put
         (list 'source index) (list body) (string-bytes body)))
      (cl-letf (((symbol-function 'lg/magit-delta--run)
                 (lambda (text _exe _args) (cl-incf runs) text)))
        (setq cold (lg/magit-delta--render-files
                    input "delta" lg/delta-test--args decode 'large-working-set))
        (should (= files decodes))
        (should (> lg/magit-delta--cache-bytes (* 16 1024 1024)))
        (should (< lg/magit-delta--cache-bytes lg/magit-delta-cache-max-bytes))
        (should (= (+ sources files) (hash-table-count lg/magit-delta--cache)))
        (dotimes (_ 2)
          (dotimes (index sources)
            (should (lg/magit-delta--cache-get (list 'source index))))
          (should (equal cold (lg/magit-delta--render-files
                               input "delta" lg/delta-test--args
                               decode 'large-working-set))))
        (should (= 1 runs))
        (should (= files decodes))))))

(ert-deftest lg/delta-cache-enforces-entry-and-byte-limits-independently ()
  (lg/delta-test--with-cache
    (dolist (limits '((2 100) (100 5)))
      (lg/magit-delta-clear-cache)
      (let ((lg/magit-delta-cache-max-entries (car limits))
            (lg/magit-delta-cache-max-bytes (cadr limits)))
        (dolist (key '(a b c))
          (lg/magit-delta--cache-put key "xx" 2))
        (should-not (lg/magit-delta--cache-get 'a))
        (should (lg/magit-delta--cache-get 'b))
        (should (lg/magit-delta--cache-get 'c))
        (should (= 2 (hash-table-count lg/magit-delta--cache)))
        (should (= 4 lg/magit-delta--cache-bytes))))))

(ert-deftest lg/delta-cache-does-not-cache-process-errors ()
  (lg/delta-test--with-cache
    (cl-letf (((symbol-function 'call-process-region) (lambda (&rest _) 2)))
      (should-error (lg/magit-delta--render-cached
                     (lg/delta-test--diff "a.py") "delta" lg/delta-test--args))
      (should (= 0 (hash-table-count lg/magit-delta--cache))))))

(ert-deftest lg/delta-context-reuses-immutable-head-across-refreshes ()
  (lg/delta-test--with-cache
    (let ((head "commit-one") (reads 0) (resolves 0))
      (cl-letf (((symbol-function 'magit-rev-parse)
                 (lambda (&rest _) (cl-incf resolves) head))
                ((symbol-function 'magit-git-insert)
                 (lambda (&rest args)
                   (cl-incf reads)
                   (insert (cadr args))
                   0)))
        (dotimes (_ 2)
          (lg/magit-delta--with-refresh-cache
           (lambda ()
             (should (equal (list "commit-one:a.py")
                            (lg/magit-delta--file-lines "a.py")))
             (should (equal (list "commit-one:a.py")
                            (lg/magit-delta--file-lines "a.py"))))))
        (should (= resolves 2))
        (should (= reads 1))
        (setq head "commit-two")
        (lg/magit-delta--with-refresh-cache
         (lambda ()
           (should (equal (list "commit-two:a.py")
                          (lg/magit-delta--file-lines "a.py")))))
        (should (= reads 2))))))

(ert-deftest lg/delta-context-working-tree-fallback-is-not-persistent ()
  (lg/delta-test--with-cache
    (let* ((dir (make-temp-file "delta-context-" t))
           (default-directory (file-name-as-directory dir)))
      (unwind-protect
          (cl-letf (((symbol-function 'magit-rev-parse) (lambda (&rest _) nil))
                    ((symbol-function 'magit-toplevel) (lambda () default-directory)))
            (with-temp-file "new.py" (insert "first\n"))
            (lg/magit-delta--with-refresh-cache
             (lambda () (should (equal '("first" "") (lg/magit-delta--file-lines "new.py")))))
            (with-temp-file "new.py" (insert "second\n"))
            (lg/magit-delta--with-refresh-cache
             (lambda () (should (equal '("second" "") (lg/magit-delta--file-lines "new.py")))))
            (should (= 0 (hash-table-count lg/magit-delta--cache))))
        (delete-directory dir t)))))

(ert-deftest lg/delta-wrapper-strips-before-conversion-without-overlays ()
  (lg/delta-test--with-cache
    (let ((input (lg/delta-test--diff "a.py"))
          (magit-delta-delta-executable "delta")
          (magit-delta-hide-plus-minus-markers nil)
          (lg/magit-delta-extra-context-lines 2)
          (calls 0))
      (cl-letf (((symbol-function 'magit-rev-parse) (lambda (&rest _) "commit"))
                ((symbol-function 'magit-git-insert)
                 (lambda (&rest _) (insert "prefix one\nprefix two\nold") 0))
                ((symbol-function 'magit-delta--make-delta-args) (lambda () lg/delta-test--args))
                ((symbol-function 'lg/magit-delta--run)
                 (lambda (text _exe _args) (cl-incf calls) text))
                ((symbol-function 'xterm-color-colorize-buffer)
                 (lambda (&rest _)
                   (should (equal input (buffer-string)))
                   (put-text-property (point-min) (point-max)
                                      'face '(:foreground "red")))))
        (dotimes (_ 2)
          (with-temp-buffer
            (insert input)
            (lg/magit-delta-call-delta-with-context (lambda () (ert-fail "uncached upstream call")))
            (should (equal input (buffer-string)))
            (should-not (overlays-in (point-min) (point-max)))
            (should-not (cl-some (lambda (ov) (overlay-get ov 'xterm-color))
                                 (overlays-in (point-min) (point-max))))))
        (should (= 1 calls))))))

(ert-deftest lg/delta-real-process-cold-warm-and-partial-renders-match ()
  (let ((delta (or (getenv "DELTA") (executable-find "delta"))))
    (skip-unless delta)
    (lg/delta-test--with-cache
      (let* ((a (lg/delta-test--diff "a.py"))
             (b (lg/delta-test--diff "b.py"))
             (changed (lg/delta-test--diff "a.py" "edited"))
             (input (concat a b))
             (expected (lg/magit-delta--run input delta lg/delta-test--args)))
        (should (equal expected (lg/magit-delta--render-cached input delta lg/delta-test--args)))
        (should (= 2 (hash-table-count lg/magit-delta--cache)))
        (should (equal expected (lg/magit-delta--render-cached input delta lg/delta-test--args)))
        (should (equal (lg/magit-delta--run (concat changed b) delta lg/delta-test--args)
                       (lg/magit-delta--render-cached (concat changed b) delta lg/delta-test--args)))))))

(ert-deftest lg/delta-real-wrapper-keeps-text-without-coloring-overlays ()
  (let ((delta (or (getenv "DELTA") (executable-find "delta"))))
    (skip-unless (and delta (require 'xterm-color nil t)))
    (lg/delta-test--with-cache
      (let ((input (lg/delta-test--diff "a.py"))
            (magit-delta-delta-executable delta)
            (magit-delta-hide-plus-minus-markers nil)
            (lg/magit-delta-extra-context-lines 2))
        (cl-letf (((symbol-function 'magit-rev-parse) (lambda (&rest _) "commit"))
                  ((symbol-function 'magit-git-insert)
                   (lambda (&rest _) (insert "prefix one\nprefix two\nold") 0))
                  ((symbol-function 'magit-delta--make-delta-args) (lambda () lg/delta-test--args)))
          (dotimes (_ 2)
            (with-temp-buffer
              (insert input)
              (lg/magit-delta-call-delta-with-context #'ignore)
              (should (equal input (buffer-substring-no-properties (point-min) (point-max))))
              (should-not (overlays-in (point-min) (point-max))))))))))

(ert-deftest lg/delta-context-preserves-blank-lines-and-does-not-cache-errors ()
  (lg/delta-test--with-cache
    (let ((status 0))
      (cl-letf (((symbol-function 'magit-rev-parse) (lambda (&rest _) "commit"))
                ((symbol-function 'magit-git-insert)
                 (lambda (&rest _) (insert "first\n\nthird\n") status))
                ((symbol-function 'magit-toplevel) (lambda () default-directory)))
        (should (equal '("first" "" "third" "")
                       (lg/magit-delta--file-lines "a.py")))
        (lg/magit-delta-clear-cache)
        (setq status 1)
        (should-not (lg/magit-delta--file-lines "nonexistent-file-for-delta-test"))
        (should (= 0 (hash-table-count lg/magit-delta--cache)))))))

(ert-deftest lg/delta-real-process-special-file-diffs ()
  (let ((delta (or (getenv "DELTA") (executable-find "delta"))))
    (skip-unless delta)
    (lg/delta-test--with-cache
      (let ((input
             (concat
              "diff --git a/space name.py b/space name.py\nnew file mode 100644\nindex 0000000..2222222\n--- /dev/null\n+++ b/space name.py\n@@ -0,0 +1 @@\n+print('héllo')\n"
              "diff --git a/gone.py b/gone.py\ndeleted file mode 100644\nindex 1111111..0000000\n--- a/gone.py\n+++ /dev/null\n@@ -1 +0,0 @@\n-old\n"
              "diff --git a/old.py b/renamed.py\nsimilarity index 100%\nrename from old.py\nrename to renamed.py\n"
              "diff --git a/blob.bin b/blob.bin\nindex 1111111..2222222 100644\nBinary files a/blob.bin and b/blob.bin differ\n")))
        (let ((expected (lg/magit-delta--run input delta lg/delta-test--args)))
          (dotimes (_ 2)
            (should (equal expected
                           (lg/magit-delta--render-cached input delta lg/delta-test--args)))))))))

(defun lg/delta-test--git (&rest args)
  "Run Git in the integration fixture without invoking user hooks."
  (should (eq 0 (apply #'process-file "git" nil nil nil
                       "-c" "core.hooksPath=/dev/null" args))))

(ert-deftest lg/delta-real-magit-stage-and-unstage-hunk ()
  (let ((delta (or (getenv "DELTA") (executable-find "delta")))
        (native-comp-enable-subr-trampolines nil))
    (skip-unless (and delta (require 'magit-delta nil t)))
    (lg/delta-test--with-cache
      (let* ((dir (make-temp-file "delta-magit-" t))
             (default-directory (file-name-as-directory dir))
             (magit-delta-delta-executable delta)
             (magit-delta-delta-args lg/delta-test--args)
             (lg/magit-delta-extra-context-lines 2)
             (magit-refresh-verbose nil)
             (magit-process-verbose nil)
             (magit-status-mode-hook '(magit-delta-mode))
             (magit-diff-wash-diffs-hook nil)
             (real-run (symbol-function 'lg/magit-delta--run))
             (real-decode (symbol-function 'lg/magit-delta--decode-file))
             (delta-calls 0)
             (decodes 0)
             (rendered-inputs nil)
             status-buffer)
        (unwind-protect
            (progn
              (lg/delta-test--git "init" "-q")
              (lg/delta-test--git "config" "user.name" "Delta Test")
              (lg/delta-test--git "config" "user.email" "delta-test@example.invalid")
              (dolist (file '("a.py" "b.py"))
                (with-temp-file file
                  (dotimes (n 60) (insert (format "line_%d = %d\n" n n)))))
              (lg/delta-test--git "add" ".")
              (lg/delta-test--git "-c" "commit.gpgsign=false" "commit" "-qm" "base")
              (dolist (file '("a.py" "b.py"))
                (with-temp-buffer
                  (insert-file-contents file)
                  (goto-char (point-min))
                  (forward-line 10)
                  (insert "extra = True\n")
                  (when (equal file "a.py")
                    (forward-line 30)
                    (insert "another = True\n"))
                  (write-region (point-min) (point-max) file nil 'silent)))
              (advice-add 'magit-refresh-buffer :around #'lg/magit-delta--with-refresh-cache)
              (advice-add 'magit-delta-call-delta-and-convert-ansi-escape-sequences
                          :around #'lg/magit-delta-call-delta-with-context)
              (cl-letf (((symbol-function 'lg/magit-delta--run)
                         (lambda (input exe args)
                           (cl-incf delta-calls)
                           (push input rendered-inputs)
                           (funcall real-run input exe args)))
                        ((symbol-function 'lg/magit-delta--decode-file)
                         (lambda (&rest args)
                           (cl-incf decodes)
                           (apply real-decode args))))
                (setq status-buffer (magit-status-setup-buffer default-directory))
                (with-current-buffer status-buffer
                  (should (> delta-calls 0))
                  (should (= 2 decodes))
                  (lg/delta-test--assert-magit-text-faces)
                  (setq delta-calls 0 decodes 0 rendered-inputs nil)
                  (magit-refresh)
                  (should (= 0 delta-calls))
                  (should (= 0 decodes))
                  (lg/delta-test--assert-magit-text-faces)
                  ;; Use an actual hunk section, not whole-file staging.
                  (let* ((unstaged (magit-get-section '((unstaged) (status))))
                         (file (car (oref unstaged children)))
                         (hunk (car (oref file children))))
                    (should (eq 'hunk (oref hunk type)))
                    (goto-char (oref hunk start))
                    (magit-stage))
                  (should (= 2 delta-calls)) ; only changed a.py in each section
                  (should (= 2 decodes))
                  (lg/delta-test--assert-magit-text-faces)
                  (dolist (input rendered-inputs)
                    (should-not (string-match-p "diff --git a/b.py" input)))
                  (should (magit-git-string "diff" "--cached" "--name-only"))
                  (let* ((staged (magit-get-section '((staged) (status))))
                         (file (car (oref staged children)))
                         (hunk (car (oref file children))))
                    (goto-char (oref hunk start))
                    (magit-unstage))
                  ;; Returning to the original diff reuses its cached output.
                  (should (= 2 delta-calls))
                  (should (= 2 decodes))
                  (lg/delta-test--assert-magit-text-faces)
                  (should-not (magit-git-string "diff" "--cached" "--name-only"))
                  (should (magit-get-section '((unstaged) (status))))
                  ;; Cached output in the same live status buffer must remain
                  ;; completely free of syntax and background coloring overlays.
                  (dotimes (_ 20)
                    (magit-refresh)
                    (lg/delta-test--assert-no-coloring-overlays))
                  (dotimes (_ 10)
                    (let* ((section (magit-get-section '((unstaged) (status))))
                           (file (car (oref section children)))
                           (hunk (car (oref file children))))
                      (goto-char (oref hunk start))
                      (magit-stage))
                    (lg/delta-test--assert-no-coloring-overlays)
                    (let* ((section (magit-get-section '((staged) (status))))
                           (file (car (oref section children)))
                           (hunk (car (oref file children))))
                      (goto-char (oref hunk start))
                      (magit-unstage))
                    (lg/delta-test--assert-no-coloring-overlays)
                    (should-not (magit-git-string "diff" "--cached" "--name-only")))
                  (should (= 2 delta-calls))
                  (should (= 2 decodes))
                  (lg/delta-test--assert-magit-text-faces)
                  (dolist (ov (overlays-in (point-min) (point-max)))
                    (should (<= (point-min) (overlay-start ov) (overlay-end ov) (point-max)))))))
          (advice-remove 'magit-refresh-buffer #'lg/magit-delta--with-refresh-cache)
          (advice-remove 'magit-delta-call-delta-and-convert-ansi-escape-sequences
                         #'lg/magit-delta-call-delta-with-context)
          (dolist (buf (buffer-list))
            (when (and (buffer-live-p buf)
                       (with-current-buffer buf
                         (string-prefix-p (file-name-as-directory dir) default-directory)))
              (kill-buffer buf)))
          (delete-directory dir t))))))

(ert-deftest lg/delta-parsed-cache-skips-warm-decoding-and-relocates-spans ()
  (lg/delta-test--with-cache
    (let* ((a (lg/delta-test--diff "a.py"))
           (b (lg/delta-test--diff "b.py"))
           (changed (lg/delta-test--diff "a.py" "a_much_longer_value"))
           (magit-delta-delta-executable "delta")
           (magit-delta-hide-plus-minus-markers nil)
           (lg/magit-delta-extra-context-lines 0)
           (parses 0) (strips 0) (scans 0)
           (strip (symbol-function 'lg/magit-delta--strip-augmentations))
           (scan (symbol-function 'lg/magit-delta--extend-line-backgrounds)))
      (cl-letf (((symbol-function 'magit-delta--make-delta-args) (lambda () lg/delta-test--args))
                ((symbol-function 'lg/magit-delta--run) (lambda (text _exe _args) text))
                ((symbol-function 'lg/magit-delta--strip-augmentations)
                 (lambda (augs) (cl-incf strips) (funcall strip augs)))
                ((symbol-function 'lg/magit-delta--extend-line-backgrounds)
                 (lambda () (cl-incf scans) (funcall scan)))
                ((symbol-function 'xterm-color-colorize-buffer)
                 (lambda (&rest _)
                   (cl-incf parses)
                   (goto-char (point-min))
                   (search-forward "+new")
                   (put-text-property (- (point) 3) (point)
                                      'face '(:foreground "red")))))
        (dolist (input (list (concat a b) (concat a b)
                            ;; Reorder files: cached spans must relocate.
                            (concat b a)))
          (with-temp-buffer
            (insert input)
            (lg/magit-delta-call-delta-with-context #'ignore)
            (should (equal input (buffer-string)))
            (goto-char (point-min))
            (while (search-forward "+new" nil t)
              (should (equal "red"
                             (cdr (assq :foreground
                                        (lg/delta-test--attributes-at (1- (point))))))))))
        (should (= parses 2))
        (should (= strips 2))
        (should (= scans 2))
        ;; Only changed a.py is decoded.  b.py moves to a new offset.
        (cl-letf (((symbol-function 'xterm-color-colorize-buffer)
                   (lambda (&rest _)
                     (cl-incf parses)
                     (goto-char (point-min))
                     (search-forward "+a_much_longer_value"))))
          (with-temp-buffer
            (insert changed b)
            (lg/magit-delta-call-delta-with-context #'ignore)
            (goto-char (point-min))
            (search-forward "+new")
            (should (equal "red"
                           (cdr (assq :foreground
                                      (lg/delta-test--attributes-at (1- (point)))))))))
        (should (= parses 3))
        (should (= strips 3))
        (should (= scans 3))))))

(ert-deftest lg/delta-parsed-cache-invalidates-marker-background-and-palette-settings ()
  (lg/delta-test--with-cache
    (let ((input (lg/delta-test--diff "a.py"))
          (magit-delta-delta-executable "delta")
          (magit-delta-hide-plus-minus-markers nil)
          (lg/magit-delta-extra-context-lines 0)
          (parses 0) (hides 0))
      (cl-letf (((symbol-function 'magit-delta--make-delta-args) (lambda () lg/delta-test--args))
                ((symbol-function 'lg/magit-delta--run) (lambda (text _exe _args) text))
                ((symbol-function 'xterm-color-colorize-buffer) (lambda (&rest _) (cl-incf parses)))
                ((symbol-function 'magit-delta-hide-plus-minus-markers)
                 (lambda ()
                   (cl-incf hides)
                   (goto-char (point-min))
                   (search-forward "+new")
                   (put-text-property (- (point) 4) (- (point) 3) 'display " "))))
        (cl-labels ((render ()
                      (with-temp-buffer
                        (insert input)
                        (lg/magit-delta-call-delta-with-context #'ignore)
                        (goto-char (point-min))
                        (search-forward "+new")
                        (get-text-property (- (point) 4) 'display))))
          (should-not (render))
          (setq magit-delta-hide-plus-minus-markers t)
          (should (equal " " (render)))
          (should (equal " " (render)))
          (should (= parses 2))
          (should (= hides 1))
          (let ((lg/magit-delta-plus-bg "#123456")) (render))
          (should (= parses 3))
          ;; Use an existing bound conversion setting, so this also works
          ;; when xterm-color itself is not installed.
          (let ((font-lock-mode t)) (render))
          (should (= parses 4)))))))

(ert-deftest lg/delta-file-augmentation-indices-and-cache-identity ()
  (let* ((a (concat (lg/delta-test--diff "a.py") "@@ -9 +9 @@\n-old\n+new\n"))
         (b (lg/delta-test--diff "b.py"))
         (augs '((0 2 "@@ -3 +3 @@") (1 3 "@@ -9 +9 @@")
                 (2 2 "@@ -3 +3 @@"))))
    (should (equal '(((0 2 "@@ -3 +3 @@") (1 3 "@@ -9 +9 @@"))
                     ((0 2 "@@ -3 +3 @@")))
                   (lg/magit-delta--file-augmentations (list a b) augs))))
  (lg/delta-test--with-cache
    (let ((input (lg/delta-test--diff "a.py")) (decodes 0))
      (cl-letf (((symbol-function 'lg/magit-delta--run) (lambda (text _exe _args) text)))
        (let ((decode (lambda (ansi _input _index) (cl-incf decodes) ansi)))
          (lg/magit-delta--render-files input "delta" lg/delta-test--args decode 'test
                                       nil '(((0 1 "original header one"))))
          (lg/magit-delta--render-files input "delta" lg/delta-test--args decode 'test
                                       nil '(((0 2 "original header two"))))
          (should (= decodes 2)))))))

(ert-deftest lg/delta-parsed-snapshot-payload-is-bounded-and-has-no-live-objects ()
  (lg/delta-test--with-cache
    (let ((snapshot
           (lg/magit-delta--snapshot-create
            :text (propertize "colored" 'face
                              '(:foreground "#aabbcc" :background "#111111" :extend t))
            ;; Existing caches can still have these obsolete span records.
            :spans (list (vector 0 7 '(face (:background "#111111")
                                      lg-magit-delta-line-bg t))))))
      (should (> (lg/magit-delta--payload-bytes snapshot) (string-bytes "colored")))
      (let ((lg/magit-delta-cache-max-bytes 8))
        (lg/magit-delta--cache-put 'parsed snapshot (lg/magit-delta--payload-bytes snapshot))
        (should-not (lg/magit-delta--cache-get 'parsed)))
      (lg/magit-delta--cache-put 'parsed snapshot (lg/magit-delta--payload-bytes snapshot))
      (cl-labels ((check (obj)
                    (should-not (or (overlayp obj) (markerp obj) (bufferp obj)))
                    (cond ((consp obj) (check (car obj)) (check (cdr obj)))
                          ((or (vectorp obj) (recordp obj)) (mapc #'check obj)))))
        (check snapshot))
      (with-temp-buffer
        (insert "prefix ")
        (dotimes (_ 2)
          (let ((start (point)))
            (lg/magit-delta--insert-snapshot snapshot)
            (let ((attrs (lg/delta-test--attributes-at start)))
              (should (equal "#aabbcc" (cdr (assq :foreground attrs))))
              (should (equal "#111111" (cdr (assq :background attrs))))
              (should (cdr (assq :extend attrs))))))
        (should-not (overlays-in (point-min) (point-max)))
        (should (= 7 (length (lg/magit-delta--snapshot-text snapshot))))))))

(ert-deftest lg/delta-wrapper-restores-original-on-decoder-error ()
  (lg/delta-test--with-cache
    (let ((input (lg/delta-test--diff "a.py"))
          (magit-delta-delta-executable "delta")
          (lg/magit-delta-extra-context-lines 2))
      (cl-letf (((symbol-function 'magit-rev-parse) (lambda (&rest _) "commit"))
                ((symbol-function 'magit-git-insert)
                 (lambda (&rest _) (insert "prefix one\nprefix two\nold") 0))
                ((symbol-function 'magit-delta--make-delta-args) (lambda () lg/delta-test--args))
                ((symbol-function 'lg/magit-delta--run) (lambda (text _exe _args) text))
                ((symbol-function 'xterm-color-colorize-buffer) (lambda (&rest _) (error "decode failed"))))
        (with-temp-buffer
          (insert input)
          (should-error (lg/magit-delta-call-delta-with-context #'ignore))
          (should (equal input (buffer-string))))
        (maphash (lambda (_key entry)
                   (should-not (lg/magit-delta--snapshot-p (cdr entry))))
                 lg/magit-delta--cache)))))

(defun lg/delta-test--attributes-at (pos)
  "Resolve face attributes at POS, merging text before prioritized overlays.
This models the attributes used by redisplay, rather than requiring an
identical overlay/text-property representation."
  (let ((attrs (make-hash-table :test #'eq)))
    (cl-labels
        ((merge-face (face)
           (cond ((keywordp (car-safe face))
                  (cl-loop for (key value) on face by #'cddr
                           do (puthash key value attrs)))
                 ((consp face) (mapc #'merge-face (reverse face)))
                 ((and face (symbolp face) (facep face))
                  (dolist (attr (face-all-attributes face))
                    (unless (eq (cdr attr) 'unspecified)
                      (puthash (car attr) (cdr attr) attrs)))))))
      (merge-face (or (get-text-property pos 'face)
                      (get-text-property pos 'font-lock-face)))
      (dolist (ov (sort (overlays-at pos)
                       (lambda (a b) (< (or (overlay-get a 'priority) 0)
                                        (or (overlay-get b 'priority) 0)))))
        (merge-face (or (overlay-get ov 'face) (overlay-get ov 'font-lock-face)))))
    (let (result)
      (maphash (lambda (key value) (push (cons key value) result)) attrs)
      (sort result (lambda (a b) (string< (symbol-name (car a))
                                        (symbol-name (car b))))))))

(defun lg/delta-test--appearance ()
  "Capture visible text, marker display, and resolved face attributes."
  (list (buffer-substring-no-properties (point-min) (point-max))
        (cl-loop for pos from (point-min) below (point-max)
                 collect (list (get-text-property pos 'display)
                               (lg/delta-test--attributes-at pos)))))

(ert-deftest lg/delta-real-parsed-cache-matches-full-buffer-color-pipeline ()
  (let ((delta (or (getenv "DELTA") (executable-find "delta"))))
    (skip-unless (and delta (require 'magit-delta nil t)))
    (lg/delta-test--with-cache
      (let ((input (concat (lg/delta-test--diff "a.py" "print('héllo')")
                           (lg/delta-test--diff "b.py" "42")))
            (magit-delta-delta-executable delta)
            (lg/magit-delta-extra-context-lines 2)
            (real-decode (symbol-function 'lg/magit-delta--decode-file))
            (decodes 0))
        (cl-letf (((symbol-function 'magit-rev-parse) (lambda (&rest _) "commit"))
                  ((symbol-function 'magit-git-insert)
                   (lambda (&rest _) (insert "# prefix\n\nold") 0))
                  ((symbol-function 'magit-delta--make-delta-args) (lambda () lg/delta-test--args))
                  ((symbol-function 'lg/magit-delta--decode-file)
                   (lambda (&rest args) (cl-incf decodes) (apply real-decode args))))
          (dolist (hide '(nil t))
            (let* ((magit-delta-hide-plus-minus-markers hide)
                   (expected
                    (with-temp-buffer
                      (insert input)
                      (let ((augs (lg/magit-delta--augment-buffer)))
                        (let ((output (lg/magit-delta--run
                                       (buffer-string) delta lg/delta-test--args)))
                          (erase-buffer)
                          (insert output))
                        (lg/magit-delta--strip-augmentations augs)
                        (xterm-color-colorize-buffer 'use-overlays)
                        (when hide (magit-delta-hide-plus-minus-markers))
                        (lg/delta-test--legacy-backgrounds)
                        (lg/delta-test--appearance)))))
              (dotimes (_ 2)
                (with-temp-buffer
                  (insert input)
                  (lg/magit-delta-call-delta-with-context #'ignore)
                  (should (equal expected (lg/delta-test--appearance)))))))
          (should (= 4 decodes)))))))

(ert-deftest lg/delta-real-decoder-honors-font-lock-and-palette-settings ()
  (skip-unless (require 'xterm-color nil t))
  (let ((magit-delta-hide-plus-minus-markers nil)
        (xterm-color-names (copy-sequence xterm-color-names)))
    (aset xterm-color-names 1 "#123456")
    (dolist (mode '(nil t))
      (with-temp-buffer
        (setq-local font-lock-mode mode)
        (let ((snapshot (lg/magit-delta--decode-file
                         "\e[31mred\e[0m\n" nil
                         (lg/magit-delta--conversion-settings))))
          (lg/magit-delta--insert-snapshot snapshot)
          (should (equal "red\n" (buffer-string)))
          (should (equal '(:foreground "#123456")
                         (get-text-property (point-min) 'face)))
          (should-not (get-text-property (point-min) 'font-lock-face))
          (should-not (overlays-in (point-min) (point-max))))))))

(ert-deftest lg/delta-parsed-fallback-strips-all-files-and-does-not-cache ()
  (lg/delta-test--with-cache
    (let* ((input (concat "unrecognized preamble\n"
                          (lg/delta-test--diff "a.py")
                          (lg/delta-test--diff "b.py")))
           (magit-delta-delta-executable "delta")
           (magit-delta-hide-plus-minus-markers nil)
           (lg/magit-delta-extra-context-lines 2)
           (parses 0))
      (cl-letf (((symbol-function 'magit-rev-parse) (lambda (&rest _) "commit"))
                ((symbol-function 'magit-git-insert)
                 (lambda (&rest _) (insert "prefix one\nprefix two\nold") 0))
                ((symbol-function 'magit-delta--make-delta-args) (lambda () lg/delta-test--args))
                ((symbol-function 'lg/magit-delta--run) (lambda (text _exe _args) text))
                ((symbol-function 'xterm-color-colorize-buffer) (lambda (&rest _) (cl-incf parses))))
        (dotimes (_ 2)
          (with-temp-buffer
            (insert input)
            (lg/magit-delta-call-delta-with-context #'ignore)
            (should (equal input (buffer-string)))))
        (should (= 2 parses))
        (maphash (lambda (_key entry)
                   (should-not (lg/magit-delta--snapshot-p (cdr entry))))
                 lg/magit-delta--cache)))))

(defun lg/delta-test--legacy-backgrounds ()
  "Reproduce the previous full-line overlays for independent equivalence tests."
  (save-excursion
    (goto-char (point-min))
    (let (in-hunk)
      (while (not (eobp))
        (cond
         ((looking-at "^diff ") (setq in-hunk nil))
         ((looking-at "^@@ ") (setq in-hunk t))
         ((and in-hunk (looking-at "^[+-]")
               (not (looking-at "^\\(\\+\\+\\+\\|---\\) ")))
          (let* ((bg (if (eq (char-after) ?+)
                         lg/magit-delta-plus-bg lg/magit-delta-minus-bg))
                 (ov (make-overlay (line-beginning-position)
                                   (min (point-max) (1+ (line-end-position))))))
            (overlay-put ov 'face `(:background ,bg :extend t))
            (overlay-put ov 'priority -1)
            (overlay-put ov 'lg-magit-delta-line-bg t))))
        (forward-line 1)))))

(defun lg/delta-test--assert-magit-text-faces ()
  "Check real Magit painting/navigation never requires syntax overlays."
  (should-not (cl-some (lambda (ov) (overlay-get ov 'xterm-color))
                       (overlays-in (point-min) (point-max))))
  (should-not (cl-some (lambda (ov) (overlay-get ov 'lg-magit-delta-line-bg))
                       (overlays-in (point-min) (point-max))))
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "^+extra = True$")
    (let* ((inhibit-read-only t)
           (pos (1+ (match-beginning 0)))
           (section (magit-section-at pos))
           (color (get-text-property pos 'face)))
      (should color)
      (should (assq :foreground (lg/delta-test--attributes-at pos)))
      (should (eq 'hunk (oref section type)))
      (magit-diff-paint-hunk section nil t)
      (should (equal color (get-text-property pos 'face)))
      (magit-diff-paint-hunk section nil nil)
      (should (equal color (get-text-property pos 'face)))
      (goto-char pos)
      (magit-section-update-highlight)
      (font-lock-ensure pos (line-end-position))
      (should (equal color (get-text-property pos 'face)))
      (magit-section-hide section)
      (magit-section-show section)
      (should (equal color (get-text-property pos 'face)))
      (should-not (cl-some (lambda (ov) (overlay-get ov 'xterm-color))
                           (overlays-in (point-min) (point-max)))))))

(ert-deftest lg/delta-text-face-properties-count-toward-payload-limit ()
  (lg/delta-test--with-cache
    (let* ((snapshot (lg/magit-delta--snapshot-create
                      :text (propertize "x" 'face '(:foreground "#123456"))
                      :spans nil))
           (bytes (lg/magit-delta--payload-bytes snapshot))
           (lg/magit-delta-cache-max-bytes 1))
      (should (> bytes 1))
      (lg/magit-delta--cache-put 'colored snapshot bytes)
      (should-not (lg/magit-delta--cache-get 'colored)))))

(ert-deftest lg/delta-real-text-faces-preserve-word-emphasis-and-line-extension ()
  (let ((delta (or (getenv "DELTA") (executable-find "delta"))))
    (skip-unless (and delta (require 'xterm-color nil t)))
    (lg/delta-test--with-cache
      (let* ((input (concat "diff --git a/a.py b/a.py\n"
                            "index 1111111..2222222 100644\n--- a/a.py\n+++ b/a.py\n"
                            "@@ -1 +1 @@\n-value = 'old'\n+value = 'new'\n"))
             (args (append lg/delta-test--args
                           '("--plus-style" "syntax #1f4a28"
                             "--minus-style" "syntax #4a1f1f"
                             "--plus-emph-style" "syntax #123456 bold"
                             "--minus-emph-style" "syntax #654321 bold")))
             (magit-delta-delta-executable delta)
             (magit-delta-hide-plus-minus-markers nil)
             (lg/magit-delta-extra-context-lines 0))
        (cl-letf (((symbol-function 'magit-delta--make-delta-args) (lambda () args)))
          (dotimes (_ 2)
            (with-temp-buffer
              (insert input)
              (lg/magit-delta-call-delta-with-context #'ignore)
              (should-not (cl-some (lambda (ov) (overlay-get ov 'xterm-color))
                                   (overlays-in (point-min) (point-max))))
              (goto-char (point-min))
              (search-forward "'new'")
              (let ((attrs (lg/delta-test--attributes-at (- (point) 3))))
                (should (equal "#123456" (cdr (assq :background attrs))))
                (should (assq :foreground attrs)))
              (let ((attrs (lg/delta-test--attributes-at (line-end-position))))
                (should (equal lg/magit-delta-plus-bg (cdr (assq :background attrs))))
                (should (cdr (assq :extend attrs))))
              (let ((backgrounds (cl-remove-if-not
                                  (lambda (ov) (overlay-get ov 'lg-magit-delta-line-bg))
                                  (overlays-in (point-min) (point-max)))))
                (should-not backgrounds)))))))))

(defun lg/delta-test--backgrounds ()
  "Return all of our backgrounds, including empty and inaccessible overlays."
  (save-restriction
    (widen)
    ;; overlay-lists omits empty overlays exactly at point-max.
    (cl-remove-if-not (lambda (ov) (overlay-get ov 'lg-magit-delta-line-bg))
                      (overlays-in (point-min) (point-max)))))

(defun lg/delta-test--assert-no-coloring-overlays ()
  "Assert neither syntax nor background coloring uses overlays anywhere."
  (save-restriction
    (widen)
    (should-not (lg/delta-test--backgrounds))
    (should-not (cl-some (lambda (ov) (overlay-get ov 'xterm-color))
                         (overlays-in (point-min) (point-max))))))

(ert-deftest lg/delta-background-text-faces-never-create-overlays ()
  (with-temp-buffer
    (dotimes (_ 50)
      (insert (lg/delta-test--diff "a.py"))
      (lg/magit-delta--extend-line-backgrounds)
      (lg/delta-test--assert-no-coloring-overlays)
      (erase-buffer)
      (should-not (lg/delta-test--backgrounds)))))

(ert-deftest lg/delta-background-ignores-legacy-cached-spans ()
  (let ((snapshot
         (lg/magit-delta--snapshot-create
          :text (propertize "one\ntwo\n" 'face '(:background "#1f4a28" :extend t))
          ;; Already-cached data may still have old overlay descriptors.
          :spans (list (vector 3 4 '(lg-magit-delta-line-bg t))
                       (vector 7 8 '(lg-magit-delta-line-bg t evaporate nil))
                       (vector 0 0 '(lg-magit-delta-line-bg t))
                       (vector 8 8 '(lg-magit-delta-line-bg t))))))
    (with-temp-buffer
      (dotimes (_ 50)
        (lg/magit-delta--insert-snapshot snapshot)
        (lg/delta-test--assert-no-coloring-overlays)
        (should (equal '(:background "#1f4a28" :extend t)
                       (get-text-property (point-min) 'face)))
        (delete-region 4 5)
        (lg/delta-test--assert-no-coloring-overlays)
        (erase-buffer)
        (should-not (lg/delta-test--backgrounds))))
    ;; Ignoring old spans must not mutate the cached entry.
    (should-not (plist-get (aref (cadr (lg/magit-delta--snapshot-spans snapshot)) 2)
                           'evaporate))))

(ert-deftest lg/delta-background-refresh-cleans-legacy-overlays-without-collateral ()
  (with-temp-buffer
    (insert "before\ninside\nafter\n")
    (let ((foreign (make-overlay 1 1))
          (lo 8) (hi 14))
      (overlay-put foreign 'lg/review-comment t)
      ;; Non-evaporating survivors from previous versions, including both
      ;; buffer boundaries and regions outside the later narrowing.
      (dolist (range (list (cons 1 1) (cons 1 4) (cons 9 10)
                          (cons (point-max) (point-max))))
        (overlay-put (make-overlay (car range) (cdr range))
                     'lg-magit-delta-line-bg t))
      (narrow-to-region lo hi)
      (let ((buffer-read-only t))
        (lg/magit-delta--with-refresh-cache
         (lambda ()
           (should (= lo (point-min)))
           (should (= hi (point-max)))
           (should-not (lg/delta-test--backgrounds))
           (should (eq (current-buffer) (overlay-buffer foreign))))))
      (should (= lo (point-min)))
      (should (= hi (point-max)))
      (should (eq (current-buffer) (overlay-buffer foreign))))))

(ert-deftest lg/delta-background-refresh-cleans-up-before-failed-refresh ()
  (with-temp-buffer
    (insert "old\n")
    (overlay-put (make-overlay 1 1) 'lg-magit-delta-line-bg t)
    (should-error
     (lg/magit-delta--with-refresh-cache (lambda () (error "refresh failed"))))
    (should-not (lg/delta-test--backgrounds))))

(ert-deftest lg/delta-background-unterminated-line-never-creates-empty-overlay ()
  (with-temp-buffer
    (insert "diff --git a/a.py b/a.py\n@@ -1 +1 @@\n+new")
    (lg/magit-delta--extend-line-backgrounds)
    (should-not (lg/delta-test--backgrounds))
    ;; The base face still supplies background extension without a newline.
    (let ((attrs (lg/delta-test--attributes-at (1- (point-max)))))
      (should (equal lg/magit-delta-plus-bg (cdr (assq :background attrs))))
      (should (cdr (assq :extend attrs))))))

(ert-deftest lg/delta-large-diff-cold-and-warm-rendering-is-overlay-free ()
  (lg/delta-test--with-cache
    (let* ((lines 50000)
           (input (concat "diff --git a/large.py b/large.py\n"
                          "--- /dev/null\n+++ b/large.py\n"
                          (format "@@ -0,0 +1,%d @@\n" lines)
                          (mapconcat #'identity (make-list lines "+value = 1\n") "")))
           (magit-delta-delta-executable "delta")
           (magit-delta-hide-plus-minus-markers nil)
           (lg/magit-delta-extra-context-lines 0)
           (runs 0) (parses 0))
      (cl-letf (((symbol-function 'magit-delta--make-delta-args)
                 (lambda () lg/delta-test--args))
                ((symbol-function 'lg/magit-delta--run)
                 (lambda (text _exe _args) (cl-incf runs) text))
                ((symbol-function 'xterm-color-colorize-buffer)
                 (lambda (&rest _) (cl-incf parses))))
        (with-temp-buffer
          (dotimes (_ 3)
            (erase-buffer)
            (insert input)
            (lg/magit-delta-call-delta-with-context #'ignore)
            (should (equal input (buffer-substring-no-properties (point-min) (point-max))))
            (lg/delta-test--assert-no-coloring-overlays)
            (let ((attrs (lg/delta-test--attributes-at (1- (point-max)))))
              (should (equal lg/magit-delta-plus-bg (cdr (assq :background attrs))))
              (should (cdr (assq :extend attrs))))))
        (should (= 1 runs))
        (should (= 1 parses))
        (maphash (lambda (_key entry)
                   (should-not (lg/magit-delta--snapshot-spans (cdr entry))))
                 lg/magit-delta--cache)))))

(ert-deftest lg/delta-warm-legacy-cache-retains-colors-but-never-replays-spans ()
  (lg/delta-test--with-cache
    (let ((input (lg/delta-test--diff "a.py"))
          (magit-delta-delta-executable "delta")
          (magit-delta-hide-plus-minus-markers nil)
          (lg/magit-delta-extra-context-lines 0)
          (decode (symbol-function 'lg/magit-delta--decode-file))
          (runs 0)
          expected)
      (cl-letf (((symbol-function 'magit-delta--make-delta-args)
                 (lambda () lg/delta-test--args))
                ((symbol-function 'lg/magit-delta--run)
                 (lambda (text _exe _args) (cl-incf runs) text))
                ((symbol-function 'xterm-color-colorize-buffer) #'ignore)
                ((symbol-function 'lg/magit-delta--decode-file)
                 (lambda (&rest args)
                   (let* ((snapshot (apply decode args))
                          (end (length (lg/magit-delta--snapshot-text snapshot))))
                     ;; Seed a real render-cache entry with the old shape.
                     (setf (lg/magit-delta--snapshot-spans snapshot)
                           (list (vector (1- end) end
                                         '(face (:background "#1f4a28" :extend t)
                                           lg-magit-delta-line-bg t evaporate t))))
                     snapshot))))
        (with-temp-buffer
          (insert input)
          (lg/magit-delta-call-delta-with-context #'ignore)
          (setq expected (lg/delta-test--appearance))
          (lg/delta-test--assert-no-coloring-overlays))
        (cl-letf (((symbol-function 'lg/magit-delta--decode-file)
                   (lambda (&rest _) (ert-fail "Warm legacy cache was not reused"))))
          (with-temp-buffer
            (insert input)
            (lg/magit-delta-call-delta-with-context #'ignore)
            (should (equal expected (lg/delta-test--appearance)))
            (lg/delta-test--assert-no-coloring-overlays)))
        (should (= 1 runs))
        ;; Old descriptors remain harmless data until normal cache eviction.
        (maphash (lambda (_key entry)
                   (should (lg/magit-delta--snapshot-spans (cdr entry))))
                 lg/magit-delta--cache)))))

;;; magit-delta-cache-test.el ends here
