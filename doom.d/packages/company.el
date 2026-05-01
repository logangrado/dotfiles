;;; ../.dotfiles/doom.d/packages/company.el -*- lexical-binding: t; -*-

(use-package! company
  :bind
  (:map company-active-map
        ("<return>" . newline)  ;; Make RET enter a newline
        ("<tab>" . company-complete-selection)
        ) ;; Use TAB for completion (instead of complete common)
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0)
  (company-tooltip-idle-delay 0)

  :config
  (after! lsp-mode
    (map! :map lsp-mode-map
          "<tab>" #'company-indent-or-complete-common))
  )
