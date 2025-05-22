;;; ../.dotfiles/doom.d/packages/exec-path-from-shell.el -*- lexical-binding: t; -*-


(use-package! exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs '("SSH_AUTH_SOCK" "OTHER_VAR1" "OTHER_VAR2"))
  )
