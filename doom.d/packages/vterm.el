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

  (defun +vterm/split-and-here-workspace (arg)
    "Split window vertically, follow, and vterm!"
    (interactive "P")
    (split-window-vertically (floor (* 0.66 (window-height))))
    (other-window 1)
    (+vterm/here-workspace arg)
    )

  (defun +vterm/toggle-window (arg)
    "Toggles a terminal popup window at project root.

    If prefix ARG is non-nil, recreate vterm buffer in the current
    project's root.

    Returns the vterm buffer."
    (interactive "P")
    (+vterm--configure-project-root-and-display
      arg
      (lambda ()
        (let
          (
            ;; Get target buffer name, unique per window
            (target_buffer_name
              (if (string-prefix-p "*doom:vterm-popup:" (buffer-name))
                (buffer-name)
                (format "*doom:vterm-popup:%s:%s*"
                        (if (bound-and-true-p persp-mode)
                        (safe-persp-name (get-current-persp))
                        "main")
                        (nth 1 (split-string (format "%s" (selected-window))))
                        )
                )
             )
             confirm-kill-processes
             current-prefix-arg
          )
          (when arg
            (let ((buffer (get-buffer target_buffer_name))
              (window (get-buffer-window target_buffer_name)))
              (when (buffer-live-p buffer)
              (kill-buffer buffer))
              (when
                (window-live-p window)
                (delete-window window)
              )
            )
          )
          (if-let (win (get-buffer-window target_buffer_name))
            (delete-window win)
            (let
              (
                (buffer (or (cl-loop for buf in (doom-buffers-in-mode 'vterm-mode)
                                        if (equal (buffer-local-value '+vterm--id buf)
                                                target_buffer_name)
                                        return buf)
                                (get-buffer-create target_buffer_name)
                         )
                )
              )
              (with-current-buffer buffer
                (setq-local +vterm--id target_buffer_name)
                (unless (eq major-mode 'vterm-mode)
                  (vterm-mode)
                )
              )
              ;; Split window vertically, follow, and display the buffer!
              (split-window-vertically (floor (* 0.65 (window-height))))
              (other-window 1)
              (set-window-buffer (selected-window) buffer)
              (set-window-dedicated-p (selected-window) t) ;; Make dedicated! Maybe helps prevent resize
            )
          )
          (get-buffer target_buffer_name)
        )
      )
    )
  )
  :config
  (setq vterm-shell "zsh")
  ;; Define keys in vterm-mode-map, active in normal-state

  (map! :map vterm-mode-map
        "<normal-state> '" #'(lambda()(interactive) (vterm-send-key "<up>"))
        "<normal-state> \"" #'(lambda()(interactive) (vterm-send-key "<down>"))
        ;; Bind C-c/C-d to work the same way in insert AND normal mode: require double press to have effect
        "<normal-state> C-c C-c" #'vterm--self-insert
        "<normal-state> C-d C-d" #'vterm--self-insert
        "<insert-state> C-c C-c" #'vterm--self-insert
        "<insert-state> C-d C-d" #'vterm--self-insert
        ;; These are already bound
        ;;"<normal-state> C-c C-c" #'(lambda()(interactive) (vterm-send-key "c" nil nil 0))
        ;;"<normal-state> C-c C-d" #'(lambda()(interactive) (vterm-send-key "d" nil nil 0))
        ;;
        ;; Example keybindings using vterm-send-X (deprecated)
        ;; "<normal-state> '" #'vterm-send-up
        ;; "<normal-state> \"" #'vterm-send-down
        ;; "<normal-state> c c" #'vterm-send-C-c
        ;; "<normal-state> c d" #'vterm-send-C-d
        )

  (map! :leader
        "o h" #'+vterm/here-workspace
        "o t" #'+vterm/toggle-window
        "o T" #'+vterm/toggle
        )
  )
