;;; ../.dotfiles/doom.d/packages/company.el -*- lexical-binding: t; -*-

(use-package! company
  ;; Complete Anything (company). Autocomplete popup for code.
  ;; Distinct from ivy, which auto-completes buffer commands
  :ensure t
  :bind
  (:map company-active-map ("RET" . newline))  ;; Make RET enter a newline
  (:map company-active-map ("TAB" . company-complete-selection)) ;; Use TAB for completion (instead of complete common)
  )
