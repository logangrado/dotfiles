;;; ../.dotfiles/doom.d/packages/origami.el -*- lexical-binding: t; -*-

(use-package!  origami
  :hook python-mode
  :init
  (origami-mode)
  :config
  (map! :map evil-normal-state-map
        :prefix ("z" . "origami")
        "j" #'origami-forward-fold  ;;origami-next-fold
        "k" #'origami-previous-fold  ;;origami-previous-fold
        "o" #'origami-open-node-recursively
        "O" #'origami-open-all-nodes
        "c" #'origami-close-node-recursively
        "C" #'origami-close-all-nodes
        "t" #'origami-recursively-toggle-node
        "T" #'origami-toggle-all-nodes)
 )
