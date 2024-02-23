;;; ../.dotfiles/doom.d/packages/projectile.el -*- lexical-binding: t; -*-

(use-package! projectile
  :config

  (add-to-list 'projectile-globally-ignored-directories "^venv$")
  (add-to-list 'projectile-globally-ignored-directories "^\\.venv$")
  )
