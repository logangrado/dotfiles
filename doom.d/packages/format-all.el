;;; ../.dotfiles/doom.d/packages/format-all.el -*- lexical-binding: t; -*-

(use-package format-all
  :init
  ;; For some reason, we need this in order for format-all to run on save for jsonnet-mode.
  ;; https://github.com/doomemacs/doomemacs/issues/6936#issuecomment-1366030502
  (advice-remove 'format-all-buffer--from-hook '+format--all-buffer-from-hook-a)
  ;; Set formatter for jsonnet mode
  (set-formatter! 'jsonnetfmt "jsonnetfmt -" :modes '(jsonnet-mode))
  )
