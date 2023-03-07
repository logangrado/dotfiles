;;; ../.dotfiles/doom.d/packages/lsp.el -*- lexical-binding: t; -*-

(use-package! lsp
  ;; https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/
  :config
  (lsp-headerline-breadcrumb-mode)
  (setq
    lsp-enable-symbol-highlighting t                    ;; Highlight symbol at point
    lsp-ui-doc-enable t                                 ;; Docs on hover
    lsp-lens-enable t                                   ;; Lens? Number of references?
    lsp-ui-sideline-enable t                            ;; Enable sideline info
    lsp-ui-sideline-show-code-actions nil               ;; Show code actions in sideline
    lsp-ui-sideline-show-hover nil                      ;; Sideline hover
    lsp-ui-sideline-show-diagnostics t                  ;; Diagnostics (errors) in sideline
    lsp-eldoc-enable-hover t                            ;; Docs on hover (not sure what this is)
    lsp-modeline-diagnostics-enable t                   ;; Enable modeline diagnostic
    lsp-headerline-breadcrumb-enable t                  ;; Show breadcrumbs (project file path)
    lsp-headerline-breadcrumb-enable-diagnostics nil    ;; Flycheck breadcrumbs
    lsp-signature-auto-activate t                       ;; Show function signature
    lsp-signature-render-documentation nil              ;; Show full documentation
    lsp-completion-show-detail t                        ;; Detail in completion
    lsp-completion-show-kind t                          ;; Kind (class, function) in completion
   )
  )
