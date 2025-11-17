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

  ;; Keybindings: mirror your Origami layout under evil normal state
  (map! :map evil-normal-state-map
        :prefix ("c" . "fold/treesit")
        ;; "c" #'treesit-fold-toggle
        "o" #'treesit-fold-open-recursively
        ;; "o" #'treesit-fold-open
        "O" #'treesit-fold-open-all
        "c" #'treesit-fold-close
        "C" #'treesit-fold-close-all
        )
  )

