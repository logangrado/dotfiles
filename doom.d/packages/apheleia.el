;;; ../.dotfiles/doom.d/packages/apheleia.el -*- lexical-binding: t; -*-

(use-package apheleia
  :config
  ;; (setq python-black-extra-args (list "-l 120"))
  (setf (alist-get 'black apheleia-formatters)
        '("black" "-l" "120" "-"))
  )
