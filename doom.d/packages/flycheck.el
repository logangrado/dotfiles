;;; ../.dotfiles/doom.d/packages/flycheck.el -*- lexical-binding: t; -*-

(use-package! flycheck
  :config

  (setq flycheck-flake8-maximum-line-length 120)

  (map! :map evil-normal-state-map
        :prefix ("e" . "flycheck")
        "j" #'flycheck-next-error
        "k" #'flycheck-previous-error
        "e" #'flycheck-explain-error-at-point
        "l" #'flycheck-list-errors
        )

  )
