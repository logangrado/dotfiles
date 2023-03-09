;;; ../.dotfiles/doom.d/packages/vterm.el -*- lexical-binding: t; -*-


(use-package! vterm
  :init
  (defun +vterm/get-workspace-buffer-name ()
    "Retrieve vterm buffer name for this workspace"
    (format "*vterm:%s*"
            (if
                (bound-and-true-p persp-mode)
                (safe-persp-name (get-current-persp))
              "main"))

    )

  (defun +vterm/here-workspace (arg)
    "Open a terminal buffer in the current window at project root.

  If prefix ARG is non-nil, cd into `default-directory' instead of project root.

  Returns the vterm buffer."
    (interactive "P")
    (+vterm--configure-project-root-and-display
     arg
     (lambda()
       (require 'vterm)
       ;; HACK forces vterm to redraw, fixing strange artefacting in the tty.
       (save-window-excursion
         (pop-to-buffer "*scratch*"))
       (let (display-buffer-alist)
         (vterm (+vterm/get-workspace-buffer-name))))))

  :config
  (setq vterm-shell "zsh")
  ;; Define keys in vterm-mode-map, active in normal-state

  (map! :map vterm-mode-map
        "<normal-state> '" #'(lambda()(interactive) (vterm-send-key "<up>"))
        "<normal-state> \"" #'(lambda()(interactive) (vterm-send-key "<down>"))
        ;; These are already bound
        ;; "<normal-state> C-c C-c" #'(lambda()(interactive) (vterm-send-key "c" nil nil 0))
        ;;"<normal-state> C-c C-d" #'(lambda()(interactive) (vterm-send-key "d" nil nil 0))
        "<normal-state> C-d C-d" #'vterm--self-insert
        ;;
        ;; Example keybindings using vterm-send-X (deprecated)
        ;; "<normal-state> '" #'vterm-send-up
        ;; "<normal-state> \"" #'vterm-send-down
        ;; "<normal-state> c c" #'vterm-send-C-c
        ;; "<normal-state> c d" #'vterm-send-C-d
        )

  (map! :leader
        "o T" #'+vterm/here-workspace
        )
  )
