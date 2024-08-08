;;; ../.dotfiles/doom.d/packages/visual-fill-column.el -*- lexical-binding: t; -*-

(use-package! visual-fill-column
  :hook
  (markdown-mode . visual-fill-column-mode)
  (org-mode . visual-fill-column-mode)
  :config
  (setq! visual-fill-column-width 120)
  )
