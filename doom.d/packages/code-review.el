;;; ../.dotfiles/doom.d/packages/code-review.el -*- lexical-binding: t; -*-

;; Ensure org’s link API is present before code-review touches it
(use-package! code-review
  :after (org ol magit forge))     ;; `ol` is the Org link library (provides `org-link-set-parameters`)
