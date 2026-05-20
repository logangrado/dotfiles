;;; ../.dotfiles/doom.d/packages/tramp.el -*- lexical-binding: t; -*-

(after! tramp
  ;; SSH ControlMaster: one persistent socket per host.
  ;; %%r@%%h:%%p → user@host:port, unique socket per destination.
  ;; ControlPersist=yes keeps the master alive after last client disconnects.
  (setq tramp-ssh-controlmaster-options
        (concat "-o ControlMaster=auto "
                "-o ControlPath=/tmp/tramp-ssh-%%r@%%h:%%p "
                "-o ControlPersist=yes"))

  ;; "sshx" skips pty allocation — faster than "ssh" for TRAMP's non-interactive shell commands.
  (setq tramp-default-method "sshx")

  ;; Prevent vc-mode from probing every remote directory for VC backends.
  ;; Each backend check is a file-exists-p call → TRAMP round-trip.
  ;; Magit is unaffected (talks to git directly via tramp-process, not vc-mode).
  (setq vc-ignore-dir-regexp
        (format "%s\\|%s" vc-ignore-dir-regexp tramp-file-name-regexp))

  ;; Disable LSP for remote editing if it becomes a problem.
  ;; Note: lsp-enable-lsp doesn't exist — the correct fix is (lsp-mode -1) in the hook.
  ;; (defun lg/lsp-mode-setup ()
  ;;   (when (file-remote-p default-directory)
  ;;     (lsp-mode -1)))
  ;; (add-hook 'lsp-mode-hook #'lg/lsp-mode-setup)

  ;; Mode-line: avoid expensive remote call for project name on every modeline refresh.
  ;; ControlMaster reduces the cost, but it's still a round-trip. Uncomment if
  ;; you want the real project name in the mode-line for remote projects.
  ;; https://emacs.stackexchange.com/a/17579
  ;; (setq projectile-mode-line "Projectile")
  ;; (setq projectile--mode-line "Projectile")

  ;; Inherit the remote user's full $PATH so tools like git are findable.
  ;; Without this, TRAMP uses a hardcoded minimal PATH and misses user-installed binaries.
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)

  ;; Auto-save locally for speed
  (setq tramp-auto-save-directory "/tmp"))
