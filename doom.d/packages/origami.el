;;; ../.dotfiles/doom.d/packages/origami.el -*- lexical-binding: t; -*-

(use-package!  origami
  :hook python-mode
  :init
  (origami-mode)
  :config
  (map! :map evil-normal-state-map
        :prefix ("c" . "origami")
        "j" #'origami-forward-fold  ;;origami-next-fold
        "k" #'origami-previous-fold  ;;origami-previous-fold
        "s" #'origami-open-node-recursively
        "S" #'origami-open-all-nodes
        "h" #'origami-close-node-recursively
        "H" #'origami-close-all-nodes
        "c" #'origami-recursively-toggle-node
        "C" #'origami-toggle-all-nodes)
 )
