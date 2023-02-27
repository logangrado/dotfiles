;;; ../.dotfiles/doom.d/packages/drag-stuff.el -*- lexical-binding: t; -*-

(use-package! drag-stuff
  :init
  (drag-stuff-mode)
  :config
  (map! :map evil-normal-state-map
        "M-j" #'drag-stuff-down
        "M-k" #'drag-stuff-up)
  )
