;;; ../.dotfiles/doom.d/packages/vterm.el -*- lexical-binding: t; -*-


(use-package! vterm
  :init
  (load! "vterm-reindex-buffers")

  (defun lg/vterm-configure-project-root-and-display-custom (arg display-fn)
    "Sets the environment variable PROOT and displays a terminal using `display-fn`.
     If prefix ARG is non-nil, cd into `default-directory' instead of project root.
     Returns the vterm buffer.

     NOTE: I (Logan) copied this from source. For some reason, this function is
     undefined on startup, unless you call the built in vterm/toggle. Defining a
     custom version here fixes this issue.
     Additionally, customizing this will hopefully let me change a workspace directory
     correctly in the future"
    (unless (fboundp 'module-load)
      (user-error "Your build of Emacs lacks dynamic modules support and cannot load vterm"))
    (let* ((project-root (or
                          (doom-project-root)     ;; Try doom project root first
                          (expand-file-name "~/") ;; Then home
                          ;;default-directory     ;; Don't  use `default-directory`
                          ))
           (default-directory
            (if arg
                default-directory
              project-root)))
      (setenv "PROOT" project-root)
      (funcall display-fn)))


  (defun lg/vterm-get-workspace-buffer-name ()
    "Retrieve vterm buffer name for this workspace"
    (format "*v:%s"
            (if
                (bound-and-true-p persp-mode)
                (safe-persp-name (get-current-persp))
              "main"))

    )

  (defun lg/vterm-here-workspace (arg)
    "Switch to the workspace vterm buffer, or create one if none exists.
     If already in a vterm buffer, always create a new one.
     If prefix ARG is non-nil, cd into `default-directory' instead of project root."
    (interactive "P")
    (let ((existing (get-buffer (lg/vterm-get-workspace-buffer-name))))
      (if (and existing (not (derived-mode-p 'vterm-mode)))
          (switch-to-buffer existing)
        (lg/vterm-configure-project-root-and-display-custom
         arg
         (lambda ()
           (require 'vterm)
           (save-window-excursion
             (pop-to-buffer "*scratch*"))
           (let (display-buffer-alist)
             (vterm (lg/vterm-get-workspace-buffer-name))))))))

  (defun lg/vterm-get-or-create-for-workspace ()
    "Switch to an existing vterm buffer for the current workspace, or create one if none exists."
    (interactive)
    (let ((workspace-vterm-buffer-name (lg/vterm-get-workspace-buffer-name)))
      ;; Check if the workspace-specific vterm buffer already exists
      (if (get-buffer workspace-vterm-buffer-name)
          ;; If it exists, switch to the vterm buffer
          (switch-to-buffer workspace-vterm-buffer-name)
        ;; If it does not exist, call lg/vterm-here-workspace to create it
        (lg/vterm-here-workspace nil))))

  (defun lg/vterm-toggle-window ()
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
          ;; (lg/vterm-here-workspace nil) ;; Start vterm in the new window
          (lg/vterm-get-or-create-for-workspace)
          ;; (set-window-dedicated-p (selected-window) t) ;; Make dedicated! Maybe helps prevent resize
          ))))

  :config
  (setq vterm-shell "zsh")

  ;; --- Helper functions ---

  (defun lg/vterm-redraw ()
    "Send Ctrl-L to the terminal to redraw the screen without clearing scrollback."
    (interactive)
    (vterm-send-key "l" nil nil t))

  (defun lg/vterm-clear-scrollback ()
    "Clear the vterm scrollback buffer, preserving the visible screen."
    (interactive)
    (vterm-clear-scrollback))

  (defun lg/vterm-adjust-cursor ()
    "Set cursor shape based on evil state: bar for insert, box for normal.
Only takes effect in vterm buffers."
    (when (derived-mode-p 'vterm-mode)
      (setq-local cursor-type
                  (if (evil-insert-state-p) 'bar 'box))))

  ;; --- Hooks for cursor synchronization ---

  (add-hook 'evil-insert-state-entry-hook #'lg/vterm-adjust-cursor)
  (add-hook 'evil-normal-state-entry-hook #'lg/vterm-adjust-cursor)
  (add-hook 'vterm-mode-hook #'lg/vterm-adjust-cursor)

  ;; --- Keybindings ---

  ;; Define keys in vterm-mode-map, active in normal-state
  (map! :map vterm-mode-map
        "<normal-state> '" #'(lambda()(interactive) (vterm-send-key "<up>"))
        "<normal-state> \"" #'(lambda()(interactive) (vterm-send-key "<down>"))
        ;; Bind C-c/C-d to work the same way in insert AND normal mode: require double press to have effect
        "<normal-state> C-c C-c" #'vterm--self-insert
        "<normal-state> C-d C-d" #'vterm--self-insert
        "<insert-state> C-c C-c" #'vterm--self-insert
        "<insert-state> C-d C-d" #'vterm--self-insert
        "<insert-state> C-c C-e" #'(lambda () (interactive) (vterm-send-key "g" nil nil t)) ;; Send Ctrl-g
        "<normal-state> C-c C-e" #'(lambda () (interactive) (vterm-send-key "g" nil nil t)) ;; Send Ctrl-g
        "<insert-state> S-<return>" #'(lambda () (interactive) (vterm-send-string "\e[13;2u"))
        ;; Raw key passthrough — C-q then any key sends it directly to the terminal
        "<insert-state> C-q" #'vterm-send-next-key
        "<normal-state> C-q" #'vterm-send-next-key
        ;; Redraw terminal (Ctrl-L) — preserves scrollback
        "<insert-state> C-c C-l" #'lg/vterm-redraw
        "<normal-state> C-c C-l" #'lg/vterm-redraw
        )

  (map! :leader
        "o h" #'lg/vterm-here-workspace
        "o t" #'lg/vterm-toggle-window
        "o T" #'lg/vterm-toggle
        "o r" #'lg/vterm-reindex-buffers
        ;; Terminal management
        "o c" #'vterm-clear
        "o l" #'lg/vterm-clear-scrollback
        )
  )
