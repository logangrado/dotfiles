;;; ../.dotfiles/doom.d/packages/vterm.el -*- lexical-binding: t; -*-


(use-package! vterm
  :config
  (setq vterm-shell "zsh")
  ;; Define keys in vterm-mode-map, active in normal-state
  (map! :map vterm-mode-map
        "<normal-state> '" #'(lambda()(interactive) (vterm-send-key "<up>"))
        "<normal-state> \"" #'(lambda()(interactive) (vterm-send-key "<down>"))
        ;; These are already bound
        ;; "<normal-state> C-c C-c" #'(lambda()(interactive) (vterm-send-key "c" nil nil 0))
        ;;"<normal-state> C-c C-d" #'(lambda()(interactive) (vterm-send-key "d" nil nil 0))
        "<normal-state> C-d C-d" #'vterm--self-insert
        ;;
        ;; Example keybindings using vterm-send-X (deprecated)
        ;; "<normal-state> '" #'vterm-send-up
        ;; "<normal-state> \"" #'vterm-send-down
        ;; "<normal-state> c c" #'vterm-send-C-c
        ;; "<normal-state> c d" #'vterm-send-C-d
        )
  )
