;;; ../.dotfiles/doom.d/packages/vterm.el -*- lexical-binding: t; -*-


(use-package! vterm
  :init
  (defun +vterm/get-workspace-buffer-name ()
    "Retrieve vterm buffer name for this workspace"
    (format "*vterm:%s"
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

  (defun +vterm/get-or-create-for-workspace ()
    "Switch to an existing vterm buffer for the current workspace, or create one if none exists."
    (interactive)
    (let ((workspace-vterm-buffer-name (+vterm/get-workspace-buffer-name)))
      ;; Check if the workspace-specific vterm buffer already exists
      (if (get-buffer workspace-vterm-buffer-name)
          ;; If it exists, switch to the vterm buffer
          (switch-to-buffer workspace-vterm-buffer-name)
        ;; If it does not exist, call +vterm/here-workspace to create it
        (+vterm/here-workspace nil))))

  (defun +vterm/toggle-window ()
    "Switch to an existing vterm buffer below the current window, or create one
     if none exists. Do nothing if in a vterm buffer."
    (interactive)
    (unless (eq major-mode 'vterm-mode) ;; Check if current window is vterm. IF not, proceed
      (let ((window-below (window-in-direction 'below))) ;; Get the window below the current one
        (if (and window-below ;; If there is a window below
                 (with-current-buffer (window-buffer window-below) ;; Check if the buffer is a vterm buffer
                   (eq major-mode 'vterm-mode)))
            (select-window window-below) ;; If it's a vterm buffer, switch to it
          ;; Else, create a new vterm window below
          (split-window-vertically (floor (* 0.65 (window-height)))) ;; Split vertically
          (other-window 1) ;; Move to the new window
          ;; (+vterm/here-workspace nil) ;; Start vterm in the new window
          (+vterm/get-or-create-for-workspace)
          ;; (set-window-dedicated-p (selected-window) t) ;; Make dedicated! Maybe helps prevent resize
          ))))

  (defun +vterm/split-and-here-workspace (arg)
    "Split window vertically, follow, and vterm!"
    (interactive "P")
    (split-window-vertically (floor (* 0.66 (window-height))))
    (other-window 1)
    (+vterm/here-workspace arg)
    )

  (defun +vterm/toggle-window-old (arg)
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
             (if (string-prefix-p (+vterm/get-workspace-buffer-name) (buffer-name)) ;
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
             ;; (set-window-buffer (selected-window) buffer)
             ;; (set-window-dedicated-p (selected-window) t) ;; Make dedicated! Maybe helps prevent resize
             (set-window-parameter (selected-window) 'window-size-fixed 'height)
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
