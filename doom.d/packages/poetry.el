;;; ../.dotfiles/doom.d/packages/poetry.el -*- lexical-binding: t; -*-

(use-package! poetry
  :init
  (remove-hook 'python-mode-hook #'poetry-tracking-mode)
  )
