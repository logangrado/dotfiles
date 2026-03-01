(use-package! claude-code
  :bind
  (:repeat-map my-claude-code-map
               ("M" . claude-code-cycle-mode))

  :config
  (after! monet
    (add-hook 'claude-code-process-environment-functions
              #'monet-start-server-function)
    (monet-mode 1))

  (setq claude-code-terminal-backend 'vterm)
  (claude-code-mode 1))
