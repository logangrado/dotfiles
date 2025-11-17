;;; ../.dotfiles/doom.d/packages/tramp.el -*- lexical-binding: t; -*-

(after! tramp
  ;; Disable LSP for remote editing
  (defun lg/lsp-mode-setup ()
    ;; Disable LSP for remote files (TRAMP)
    (when (file-remote-p default-directory)
      (setq lsp-enable-lsp nil)))
  (add-hook 'lsp-mode-hook 'lg/lsp-mode-setup)


  ;; Optimizations to make tramp run faster with projectile
  ;; PROJECTILE
  (setq projectile-mode-line "Projectile") ;;https://emacs.stackexchange.com/a/17579
  (setq projectile--mode-line "Projectile") ;;https://emacs.stackexchange.com/a/17579

  ;; Below is a bunch of code I was using to try to optimize magit over tramp
  ;; Turns out magit over tramp is fine (tested against local server), it's just really slow on AMD server
  ;; Unclear if due to higher ping, or server configuration
  ;; ===========================================================================================================
  ;; (setq lsp-log-io t)
  ;; (setq vc-ignore-dir-regexp
  ;;       (format "%s\\|%s"
  ;;               vc-ignore-dir-regexp
  ;;               tramp-file-name-regexp))
  ;; (setq tramp-verbose 1)

  ;; (setq tramp-ssh-controlmaster-options "-o ControlMaster=auto -o ControlPath='/tmp/tramp-controlpath-%%C' -o ControlPersist=no")
  ;; (setq tramp-default-method "ssh")
  ;; (setq tramp-verbose 6)

  ;; Make tramp autosave on local machine, for speed!
  (setq tramp-auto-save-directory "/tmp")
  ;; (setq tramp-default-remote-shell "/bin/bash")
  ;; (setq explicit-shell-file-name "/bin/bash")

  ;; (customize-set-variable 'tramp-encoding-shell "/usr/bin/zsh")


  ;; (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  ;; (setq tramp-default-method "ssh")
  ;; (setq tramp-inline-compress-start-size 1000000)

  ;; (defun tramp-check-for-regexp (proc regexp)
  ;;   "Check, whether REGEXP is contained in process buffer of PROC.
  ;; Erase echoed commands if exists."
  ;;   (with-current-buffer (process-buffer proc)
  ;;     (if (> (point-max) 1000)
  ;;         (progn
  ;;           (goto-char (point-max))
  ;;           (ignore-errors (re-search-backward regexp (- (point-max) 1000) t)))
  ;;       (progn
  ;;         (goto-char (point-min))

  ;;         ;; Check whether we need to remove echo output.
  ;;         (when (and (tramp-get-connection-property proc "check-remote-echo" nil)
  ;;                    (re-search-forward tramp-echoed-echo-mark-regexp nil t))
  ;;           (let ((begin (match-beginning 0)))
  ;;             (when (re-search-forward tramp-echoed-echo-mark-regexp nil t)
  ;;               ;; Discard echo from remote output.
  ;;               (tramp-set-connection-property proc "check-remote-echo" nil)
  ;;               (tramp-message proc 5 "echo-mark found")
  ;;               (forward-line 1)
  ;;               (delete-region begin (point))
  ;;               (goto-char (point-min)))))

  ;;         (when (or (not (tramp-get-connection-property proc "check-remote-echo" nil))
  ;;                   ;; Sometimes, the echo string is suppressed on the remote side.
  ;;                   (not (string-equal
  ;;                         (tramp-compat-funcall
  ;;                          'substring-no-properties tramp-echo-mark-marker
  ;;                          0 (min tramp-echo-mark-marker-length (1- (point-max))))
  ;;                         (tramp-compat-funcall
  ;;                          'buffer-substring-no-properties
  ;;                          (point-min)
  ;;                          (min (+ (point-min) tramp-echo-mark-marker-length)
  ;;                               (point-max))))))
  ;;           ;; No echo to be handled, now we can look for the regexp.
  ;;           ;; Sometimes, lines are much to long, and we run into a "Stack
  ;;           ;; overflow in regexp matcher".  For example, //DIRED// lines of
  ;;           ;; directory listings with some thousand files.  Therefore, we
  ;;           ;; look from the end.
  ;;           (goto-char (point-max))
  ;;           (ignore-errors (re-search-backward regexp nil t)))))))

  ;; TRAMP
  ;; Make TRAMP faster
  ;; (customize-set-variable
  ;;  'tramp-ssh-controlmaster-options
  ;;  (concat
  ;;   "-o ControlPath=/tmp/ssh-ControlPath-%%r@%%h:%%p "
  ;;   "-o ControlMaster=auto -o ControlPersist=yes"))
  )
