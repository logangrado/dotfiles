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
  (global-treesit-auto-mode)

  ;; ==========================================================
  ;; FIX: TREESIT-AUTO MAKES OPENING FILES SLOW
  ;; Global treesit mode makes opening files slow -> this is especially painful for magit commit buffer.
  ;; Issue: https://github.com/renzmann/treesit-auto/issues/135
  ;; One commenter suggests caching the treesit-language-available-p call: https://github.com/jeremyf/dotemacs/blob/75410e2f56273b2be4abf10d0d72627ec4ad6a85/emacs.d/init.el#L5176-L5197

  (defvar lg/treesit-lang-cache
    (make-hash-table :test 'equal)
    "Cache the expensive computation of treelit language availability.
     See `lg/treesit-language-available-p' for usage.")

  (defun lg/treesit-language-available-p (fn lang &rest rest)
    "Caching around the CPU expensive `treesit-language-available-p'."
    ;; I did some profiling of `treesit-language-available-p', and found
    ;; that when moving around via consult (and therefore preview) this
    ;; function was contributing to 75% of the CPU time.  And it was run
    ;; each time.
    (let ((cached-value
           (gethash lang lg/treesit-lang-cache 'miss)))
      (if (eq 'miss cached-value)
          (let ((value
                 (apply fn lang rest)))
            (puthash lang value lg/treesit-lang-cache)
            value)
        cached-value)))
  (advice-add #'treesit-language-available-p
              :around #'lg/treesit-language-available-p)
  ;; END FIX
  ;; ==========================================================
  )

(use-package! treesit-fold
  :hook (prog-mode . treesit-fold-mode)
  :config

  (defun lg/treesit-fold--top-level-overlays-in (beg end)
    "Return top-level treesit-fold overlays between BEG and END.

Top-level here means overlays that are not strictly contained
inside another treesit-fold overlay in the same region."
    (let* ((ovs (treesit-fold--overlays-in 'invisible 'treesit-fold beg end))
           (sorted (sort (copy-sequence ovs)
                         (lambda (a b)
                           (< (overlay-start a) (overlay-start b)))))
           res)
      (dolist (ov sorted)
        (let ((s (overlay-start ov))
              (e (overlay-end ov))
              (add t))
          ;; skip overlays strictly contained in any already-accepted overlay
          (dolist (prev res)
            (when (and (<= (overlay-start prev) s)
                       (>= (overlay-end prev)   e)
                       (or (< (overlay-start prev) s)
                           (> (overlay-end prev)   e)))
              (setq add nil)))
          (when add
            (push ov res)))
        (nreverse res))))

  (defun lg/treesit-fold-cycle ()
    "Cycle folding at the treesit node on this line's indentation.

States:
1. Node and children fully open:
     -> fold node and all children.
2. Node folded (but children also have folds underneath):
     -> open just this node (keep children folded).
3. Node open, some children folded:
     -> open children."
    (interactive)
    (treesit-fold--ensure-ts
      (save-excursion
        (back-to-indentation)
        (when-let* ((node (treesit-fold--foldable-node-at-pos (point))))
          (let* ((beg      (treesit-node-start node))
                 (end      (treesit-node-end   node))
                 (self-ov  (treesit-fold-overlay-at node))
                 (all-ovs  (treesit-fold--overlays-in 'invisible 'treesit-fold beg end))
                 (child-ovs (if self-ov (remq self-ov all-ovs) all-ovs)))
            (cond
             ;; 2. Node folded -> open just the node, keep children folded
             (self-ov
              (delete-overlay self-ov)
              (run-hooks 'treesit-fold-on-fold-hook))

             ;; 3. Node open, children folded -> open children
             (child-ovs
              (mapc #'delete-overlay child-ovs)
              (run-hooks 'treesit-fold-on-fold-hook))

             ;; 1. Fully open -> fold node + all children
             (t
              (save-restriction
                (narrow-to-region beg end)
                (treesit-fold-close-all)))))))))
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
        "x" #'lg/treesit-fold-cycle
        )
  )
