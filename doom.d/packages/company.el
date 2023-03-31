;;; ../.dotfiles/doom.d/packages/company.el -*- lexical-binding: t; -*-

(use-package! company
  :bind
  (:map company-active-map ("<return>" . newline))  ;; Make RET enter a newline
  (:map company-active-map ("<tab>" . company-complete-selection)) ;; Use TAB for completion (instead of complete common)
  )
