;;; ../.dotfiles/doom.d/packages/python-black.el -*- lexical-binding: t; -*-

;; (use-package! python-black
;;   :demand t
;;   :after python
;;   :config
;;   (add-hook! 'python-mode-hppk #'python-black-on-save-mode))

(use-package python-black
  :ensure t
  :after python
  :hook (python-mode . python-black-on-save-mode)
  :config
  (setq python-black-extra-args (list "-l 120"))
  )
