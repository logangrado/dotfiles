;;; ../.dotfiles/doom.d/packages/dockerfile-mode.el -*- lexical-binding: t; -*-

(use-package! dockerfile-mode
  :init
  (add-to-list 'auto-mode-alist '("Dockerfile.*\\'" . dockerfile-mode))
 )
