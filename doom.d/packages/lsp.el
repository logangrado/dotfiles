;;; ../.dotfiles/doom.d/packages/lsp.el -*- lexical-binding: t; -*-

(use-package! lsp
  ;; https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/
  ;; :hook (lsp-mode . lsp-enable-which-key-integration) ;; DON'T DO THIS! It breaks lsp-ui-doc-glance
  :init
  (setq lsp-keymap-prefix "C-l")
  :config
  (map! :map lsp-mode-map
        :desc "lsp" "C-l" lsp-command-map
        )
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
  (setq
   ;; Ensure LSP spanws one project per root
   lsp-keep-workspace-alive nil ;; Don't keep workspaces alive when switching projects
   lsp-auto-guess-root t ;; Better guessing of project root
   )


  ;; (setq lsp-pyright-langserver-command '("uvx" "basedpyright"))
  (setq lsp-pyright-langserver-command "basedpyright")
  (setq lsp-python-ty-clients-server-command '("uvx" "ty" "server"))

  ;; comment out the server we dont want
  (setq lsp-disabled-clients '
        (
         ty-ls
         ;; pyright
         ))

  ;; (optional) auto-refresh when switching projects
  (add-hook 'projectile-after-switch-project-hook #'lsp-restart-workspace)
  )

(after! lsp-treemacs
  ;; Smaller icons
  (setq treemacs-width 25)                    ;; narrower panel
  (treemacs-resize-icons 12)                  ;; smaller icons (default is 22)

  ;; More compact spacing
  (setq treemacs-indentation 1)               ;; less indentation (default 2)
  (setq treemacs-indentation-string " ")      ;; indentation character

  ;; Smaller text
  ;; (setq treemacs-text-scale -1)
  )              ;; scale down text
