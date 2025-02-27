;;; ../.dotfiles/doom.d/keybindings.el -*- lexical-binding: t; -*-

(map! :map (evil-normal-state-map evil-visual-state-map)
      "s" #'evil-snipe-s
      "S" #'evil-snipe-S
      "f" #'evil-snipe-f
      "F" #'evil-snipe-F
      ;; "x" #'evil-snipe-x
      ;; "X" #'evil-snipe-X
      )
