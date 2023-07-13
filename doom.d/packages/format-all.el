;;; ../.dotfiles/doom.d/packages/format-all.el -*- lexical-binding: t; -*-

(use-package format-all
  :init
  ;; Set formatter for jsonnet mode
  (set-formatter! 'jsonnetfmt "jsonnetfmt -" :modes '(jsonnet-mode))
  )
