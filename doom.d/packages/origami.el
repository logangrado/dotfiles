;;; ../.dotfiles/doom.d/packages/origami.el -*- lexical-binding: t; -*-

;;;; ACTUALLY DEPRECATED IN FAVOR OF TS-FOLD
;;; DEPRECATED IN FAVOR OF OUTLINE-MINOR-MODE
;;; Origami mode doesn't seem to work anymore. no idea what changed
;; (use-package!  origami
;;   :hook (prog-mode . origami-mode)
;;   :config
;;   (map! :map evil-normal-state-map
;;         :prefix ("c" . "origami")
;;         "j" #'origami-forward-fold  ;;origami-next-fold
;;         "k" #'origami-previous-fold  ;;origami-previous-fold
;;         "s" #'origami-open-node-recursively
;;         "S" #'origami-open-all-nodes
;;         "h" #'origami-close-node-recursively
;;         "H" #'origami-close-all-nodes
;;         "c" #'origami-recursively-toggle-node
;;         "C" #'origami-toggle-all-nodes)
;;   )
