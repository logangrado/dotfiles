;;; ../.dotfiles/doom.d/packages/poetry.el -*- lexical-binding: t; -*-

(use-package! poetry
  :init
  (defun poetry-workspace-reload ()
    "Restart LSP workspace with poetry environment activated"
    (interactive)

    ;; This does the right things in the right order, but does not work as a part of a function
    ;; It seems like the `lsp-workspace-restart' doesn't respect the poetry venv when called here

    ;; (when (not (poetry-venv-activated-p)) (poetry-venv-toggle))
    ;; (lsp-workspace-restart (lsp--read-workspace))
    ;; (poetry-venv-toggle)

    (poetry-venv-workon)
    (lsp-workspace-restart (lsp--read-workspace))
    (poetry-venv-deactivate)

    )

  :after python
  ;; :init
  ;; (remove-hook 'python-mode-hook #'poetry-tracking-mode)
  )

;; Config from DOOM
;; (use-package! poetry
;;   :when (modulep! +poetry)
;;   :after python
;;   :init
;;   (setq poetry-tracking-strategy 'switch-buffer)
;;   (add-hook 'python-mode-hook #'poetry-tracking-mode))
