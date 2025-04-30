;;; ../.dotfiles/doom.d/packages/treesit-fold.el -*- lexical-binding: t; -*-

(use-package! treesit-fold
  :after treesit
  :hook (prog-mode . my/treesit-fold-setup)
  :config
  (defun my/treesit-fold-setup ()
    "Enable treesit-fold-mode if current major mode is a -ts-mode."
    (when (and (boundp 'treesit-language-at) ;; treesit available
               (treesit-language-at (point-min))) ;; buffer has treesit parse tree
      (treesit-fold-mode)))

  (setq treesit-fold-replacement " [...] ")

  ;; (defun my/treesit-print-tree-at-point ()
  ;;   "Traverse and print the tree rooted at the node at point."
  ;;   (interactive)
  ;;   (unless (treesit-parser-list)
  ;;     (error "No Tree-sitter parser active in this buffer"))
  ;;   (let ((node (treesit-node-at (point))))
  ;;     (my/treesit--print-node-recursively node 0)))

  ;; (defun my/treesit--print-node-recursively (node depth)
  ;;   "Recursively print NODE and all descendants, indented by DEPTH."
  ;;   (when (treesit-node-p node) ;; Check it's a real node
  ;;     (let ((indent (make-string (* 2 depth) ?\s))
  ;;           (type (treesit-node-type node)))
  ;;       (message "%s%s" indent type))
  ;;     (message "Node: %s" node)
  ;;     (message "Num children %s" (treesit-node-child-count node))
  ;;     ;; Recurse into children
  ;;     (let ((child (treesit-node-child node 0)))
  ;;       (while child
  ;;         (my/treesit--print-node-recursively child (1+ depth))
  ;;         (setq child (treesit-node-next-sibling child))))))
  (defun my/treesit-print-tree-at-point ()
    "Traverse and print the tree rooted at the meaningful node at point."
    (interactive)
    (unless (treesit-parser-list)
      (error "No Tree-sitter parser active in this buffer"))
    (let ((node (my/treesit--meaningful-node-at-point)))
      (if node
          (my/treesit--print-node-recursively node 0)
        (message "No meaningful node found."))))

  (defun my/treesit--print-node-recursively (node depth)
    "Recursively print NODE and all descendants, indented by DEPTH."
    (when (treesit-node-p node)
      (let ((indent (make-string (* 2 depth) ?\s))
            (type (treesit-node-type node)))
        (message "%s%s" indent type))
      (dolist (child (treesit-node-children node))
        (my/treesit--print-node-recursively child (1+ depth)))))

  ;;----------
  (defun my/treesit--meaningful-node-at-point ()
    "Return a 'big' node at point, skipping tokens like identifiers, keywords, punctuation."
    (interactive)
    (let ((node (treesit-node-at (point))))
      (while (and node
                  (member (treesit-node-type node)
                          '("identifier" "comment" "string" "integer" "class" "def" ":" "(" ")" "=")))
        (setq node (treesit-node-parent node)))
      (message "Node: %s" node)
      node))

  (defun my/treesit--node-folded-p (node)
    "Return non-nil if NODE is folded (invisible overlay at start)."
    (let ((start (treesit-node-start node)))
      (seq-some (lambda (ov)
                  (overlay-get ov 'invisible))
                (overlays-at start))))
  (defun my/treesit--any-folded-child-p (node)
    "Return t if NODE has any immediate folded child."
    (seq-some #'my/treesit--node-folded-p
              (treesit-node-children node)))
  (defun my/treesit-fold-cycle ()
    "Cycle folding at point: progressively unfold one level deeper each time, then refold."
    (interactive)
    (unless (treesit-parser-list)
      (error "No Tree-sitter parser active in this buffer"))
    (let ((node (my/treesit--meaningful-node-at-point)))
      (cond
       ;; If node itself is folded, unfold it
       ((my/treesit--node-folded-p node)
        (treesit-fold-open node))

       ;; If any immediate children are folded, unfold them
       ((my/treesit--any-folded-child-p node)
        (dolist (child (treesit-node-children node))
          (when (my/treesit--node-folded-p child)
            (treesit-fold-open child))))

       ;; Else refold the node
       (t
        (treesit-fold-close node)))))

  (map! :leader
        :prefix "z"
        :desc "Toggle fold" "t" #'treesit-fold-toggle
        :desc "Fold"         "f" #'treesit-fold-fold
        :desc "Unfold"       "u" #'treesit-fold-unfold)

  (map! :map evil-normal-state-map
        :prefix ("c" . "ts-fold")
        ;; "j" #'origami-forward-fold  ;;origami-next-fold
        ;; "k" #'origami-previous-fold  ;;origami-previous-fold
        ;; "s" #'origami-open-node-recursively
        ;; "S" #'origami-open-all-nodes
        ;; "h" #'origami-close-node-recursively
        ;; "H" #'origami-close-all-nodes
        "c" #'treesit-fold-toggle
        "C" #'treesit-fold-open-recursively
        "h" #'treesit-fold-close
        "H" #'treesit-fold-close-all
        "s" #'treesit-fold-open
        "S" #'treesit-fold-open-all
        )
  )
