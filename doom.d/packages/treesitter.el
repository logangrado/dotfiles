(setq treesit-font-lock-level 4)

;; Use treesit-auto to remap modes + install grammars on demand
(use-package! treesit-auto
  :after treesit
  :custom
  ;; auto-install grammars the first time you open a file
  (treesit-auto-install 'prompt)
  :config
  ;; populate auto-mode-alist with ts modes where available
  (treesit-auto-add-to-auto-mode-alist)
  (global-treesit-auto-mode))

(use-package! treesit-fold
  :hook (prog-mode . treesit-fold-mode)
  :config
  ;; Some builds/package versions provide these; if not, define simple fallbacks.
  ;; (unless (fboundp 'treesit-fold-next)
  ;;   (defun treesit-fold-next ()
  ;;     "Move to the start of the next foldable node."
  ;;     (interactive)
  ;;     (let ((pos (point)))
  ;;       ;; Jump to end of current line to avoid re-hitting same node
  ;;       (end-of-line)
  ;;       (unless (treesit-fold-toggle 'peek)
  ;;         (when (re-search-forward ".*" nil t)
  ;;           (beginning-of-line)))
  ;;       (when (<= (point) pos)
  ;;         (goto-char pos)))))
  ;; (unless (fboundp 'treesit-fold-previous)
  ;;   (defun treesit-fold-previous ()
  ;;     "Move to the start of the previous foldable node."
  ;;     (interactive)
  ;;     (beginning-of-line)
  ;;     (let ((here (point)))
  ;;       (when (re-search-backward "^.*" nil t)
  ;;         (beginning-of-line))
  ;;       (when (>= (point) here)
  ;;         (goto-char here)))))

  ;; DEFAULT FOLDING FOR PYTHON:
  ;; (python-ts-mode (import_statement . treesit-fold-range-seq)
  ;; (import_from_statement . treesit-fold-range-seq)
  ;; (future_import_statement . treesit-fold-range-seq)
  ;; (function_definition . treesit-fold-range-python-def)
  ;; (class_definition . treesit-fold-range-python-def)
  ;; (while_statement . treesit-fold-range-python-block)
  ;; (for_statement . treesit-fold-range-python-block)
  ;; (if_statement . treesit-fold-range-python-block)
  ;; (elif_clause . treesit-fold-range-python-block)
  ;; (else_clause . treesit-fold-range-python-block)
  ;; (match_statement . treesit-fold-range-python-block)
  ;; (case_clause . treesit-fold-range-python-block)
  ;; (try_statement . treesit-fold-range-python-block)
  ;; (except_clause . treesit-fold-range-python-block)
  ;; (with_statement . treesit-fold-range-python-block)
  ;; (list . treesit-fold-range-seq)
  ;; (dictionary . treesit-fold-range-seq)
  ;; (parenthesized_expression . treesit-fold-range-seq)
  ;; (expression_statement
  ;;  . treesit-fold-range-python-expression-statement)
  ;; (comment lambda (node offset)
  ;;          (treesit-fold-range-line-comment node offset "#")))
  (setf (alist-get 'python-ts-mode treesit-fold-range-alist)
        '((class_definition    . treesit-fold-range-python-def)
          (function_definition . treesit-fold-range-python-def)
          ;; optional: fold blocks of `#` comments
          (comment . (lambda (node offset)
                       (treesit-fold-range-line-comment node offset "#")))))

  (setq treesit-fold-line-count-show nil)

  ;; Keybindings: mirror your Origami layout under evil normal state
  (map! :map evil-normal-state-map
        :prefix ("c" . "fold/treesit")
        "t" #'treesit-fold-toggle
        ;; "o" #'treesit-fold-open-recursively
        "o" #'treesit-fold-open
        "O" #'treesit-fold-open-all
        "c" #'treesit-fold-close
        "C" #'treesit-fold-close-all
        )
  )
