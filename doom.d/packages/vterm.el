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
    "Retrieve the first vterm buffer name for this workspace (*v:WS<1>)."
    (format "*v:%s<1>"
            (if (bound-and-true-p persp-mode)
                (safe-persp-name (get-current-persp))
              "main")))

  (defun lg/vterm-next-workspace-buffer-name ()
    "Return the next available vterm buffer name *v:WS<N> for this workspace."
    (let* ((ws (if (bound-and-true-p persp-mode)
                   (safe-persp-name (get-current-persp))
                 "main"))
           (base (format "*v:%s" ws))
           (idx 1))
      (while (get-buffer (format "%s<%d>" base idx))
        (setq idx (1+ idx)))
      (format "%s<%d>" base idx)))

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
             (vterm (lg/vterm-next-workspace-buffer-name))))))))

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

  ;; Show modeline in vterm buffers (Doom hides it by default).
  ;; This lets doom-modeline display evil state (INSERT/NORMAL/VISUAL).
  (remove-hook 'vterm-mode-hook #'hide-mode-line-mode)

  ;; Show 📋 indicator next to evil state badge when vterm-copy-mode is active.
  (after! doom-modeline
    (defun lg/vterm-copy-mode-modeline-indicator (result)
      "Append 📋 to the modals segment when vterm-copy-mode is active."
      (if (bound-and-true-p vterm-copy-mode)
          (concat result (propertize " 📋" 'face 'doom-modeline-warning))
        result))
    (advice-add 'doom-modeline-segment--modals
                :filter-return #'lg/vterm-copy-mode-modeline-indicator))

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

  (defun lg/vterm--read-zsh-history ()
    "Return commands from zsh's HISTFILE as a list, newest-first, deduped.
Strips the `: <epoch>:<elapsed>;' prefix that `extended_history' writes.
Multi-line entries (backslash-continuation in extended_history) are
returned as separate lines — good enough for v1."
    (let ((file (or (getenv "HISTFILE")
                    (expand-file-name "~/.zsh_history"))))
      (when (file-readable-p file)
        (with-temp-buffer
          ;; zsh writes history in its locale; force utf-8 read so unusual
          ;; bytes don't choke the parser.
          (let ((coding-system-for-read 'utf-8-auto))
            (insert-file-contents file))
          (let (cmds)
            (goto-char (point-min))
            (while (not (eobp))
              (let ((line (buffer-substring-no-properties
                           (line-beginning-position) (line-end-position))))
                (when (string-match
                       "\\`\\(?:: [0-9]+:[0-9]+;\\)?\\(.+\\)\\'" line)
                  (push (match-string 1 line) cmds)))
              (forward-line 1))
            (delete-dups cmds))))))

  (defun lg/vterm-consult-history ()
    "Fuzzy-search ~/.zsh_history via vertico; send the chosen entry to the
live vterm prompt via `vterm-send-string' so it can be edited before
Enter. Replaces `consult-history', which doesn't know about vterm-mode
and uses `insert' (wrong) instead of `vterm-send-string'."
    (interactive)
    (unless (derived-mode-p 'vterm-mode)
      (user-error "Not in a vterm buffer"))
    (let ((entries (lg/vterm--read-zsh-history)))
      (unless entries
        (user-error "No zsh history found at %s"
                    (or (getenv "HISTFILE") "~/.zsh_history")))
      (let ((cmd (consult--read entries
                                :prompt "zsh history: "
                                :sort nil
                                :require-match t
                                :category 'consult-history)))
        (when (and cmd (not (string-empty-p cmd)))
          (vterm-send-string cmd)))))

  (defun lg/consult-history-dwim ()
    "Search history for the current buffer's shell.
In `vterm-mode' use `lg/vterm-consult-history' (sends the chosen entry
to the live zsh prompt). Elsewhere fall back to `consult-history',
which handles eshell/comint/term/minibuffer via `consult-mode-histories'."
    (interactive)
    (if (derived-mode-p 'vterm-mode)
        (lg/vterm-consult-history)
      (call-interactively #'consult-history)))

  ;; --- Hooks for cursor synchronization ---

  (add-hook 'evil-insert-state-entry-hook #'lg/vterm-adjust-cursor)
  (add-hook 'evil-normal-state-entry-hook #'lg/vterm-adjust-cursor)
  (add-hook 'vterm-mode-hook #'lg/vterm-adjust-cursor)

  ;; --- Auto-toggle vterm-copy-mode around navigation ---
  ;; In vterm + evil-normal-state, plain h/j/k/l (and arrow keys) move point but
  ;; vterm resets point to the live cursor on every render, so navigation feels
  ;; broken. Auto-enter `vterm-copy-mode' on basic motion to freeze the buffer,
  ;; and auto-exit on insert-state entry so typing works again.

  (defvar lg/vterm-auto-copy-motion-commands
    '(evil-next-line
      evil-previous-line
      evil-next-visual-line
      evil-previous-visual-line
      evil-collection-vterm-next-line
      evil-forward-char
      evil-backward-char
      lg/evil-up-10
      lg/evil-down-10)
    "Motion commands that auto-enable `vterm-copy-mode' in vterm buffers.
`evil-collection-vterm-next-line' is `j' in normal-state vterm buffers
under evil-collection (it clamps motion to the prompt area in live
mode), so it must be listed alongside the plain `evil-next-line'
variants for the auto-entry to recognise it.")

  (defvar lg/vterm-auto-copy-jk-window 0.1
    "Seconds to wait for `k' after a `j' before entering copy mode.
Matches `evil-escape''s default delay. Pressing `j' in vterm
normal/visual state pauses for this long peeking for `k'; if it
arrives, both keys are swallowed and copy mode is not entered.
Only the `j' direction is peeked — `k' enters copy mode immediately
because `kj' isn't an `evil-escape' habit.")

  (defun lg/vterm--peek-for-k ()
    "Peek the next input event for `k' within `lg/vterm-auto-copy-jk-window'.
Returns t and consumes the event on match. Pushes any non-matching
event back onto `unread-command-events'. Returns nil on timeout."
    (let ((evt (read-event nil nil lg/vterm-auto-copy-jk-window)))
      (cond
       ((eq evt ?k) t)
       ((null evt) nil)
       (t (push evt unread-command-events) nil))))

  (defun lg/vterm-maybe-enter-copy-mode ()
    "Enter `vterm-copy-mode' when about to run a basic motion in vterm.
Fires in both normal and visual states so J/K and selection extension
work without vterm fighting point.

If the triggering key is `j', first peek for `k' within
`lg/vterm-auto-copy-jk-window' (analogous to `evil-escape''s `jk'
detection). If `k' arrives, swallow both keys without entering copy
mode or moving point — this handles the habit of double-tapping `jk'
to confirm normal state after `evil-escape'."
    (when (and (derived-mode-p 'vterm-mode)
               (or (evil-normal-state-p) (evil-visual-state-p))
               (not (bound-and-true-p vterm-copy-mode))
               (memq this-command lg/vterm-auto-copy-motion-commands))
      (if (and (eq last-command-event ?j) (lg/vterm--peek-for-k))
          (setq this-command 'ignore)
        (vterm-copy-mode 1))))

  (defun lg/vterm-exit-copy-mode-on-insert ()
    "Disable `vterm-copy-mode' when entering insert state in a vterm buffer."
    (when (and (derived-mode-p 'vterm-mode)
               (bound-and-true-p vterm-copy-mode))
      (vterm-copy-mode -1)))

  (add-hook 'pre-command-hook #'lg/vterm-maybe-enter-copy-mode)
  (add-hook 'evil-insert-state-entry-hook #'lg/vterm-exit-copy-mode-on-insert)

  ;; --- Scroll to live cursor when re-entering a vterm buffer ---
  ;; When the user switches focus to a vterm buffer (window selection change or
  ;; buffer swap) and `vterm-copy-mode' is off, jump point to vterm's tracked
  ;; cursor (the prompt) and recenter so the window scroll matches.

  (defun lg/vterm-jump-to-bottom-if-live ()
    "If current buffer is vterm and copy-mode is off, jump to vterm's live cursor."
    (when (and (derived-mode-p 'vterm-mode)
               (not (bound-and-true-p vterm-copy-mode)))
      (when (fboundp 'vterm-reset-cursor-point)
        (vterm-reset-cursor-point))
      (recenter -1)))

  (defun lg/vterm-on-window-selection-change (frame)
    "Run `lg/vterm-jump-to-bottom-if-live' in FRAME's newly-selected window."
    (with-selected-frame frame
      (when (window-live-p (selected-window))
        (with-selected-window (selected-window)
          (lg/vterm-jump-to-bottom-if-live)))))

  (defun lg/vterm-on-window-buffer-change (window)
    "Run `lg/vterm-jump-to-bottom-if-live' when WINDOW's buffer changes."
    (when (and (window-live-p window) (eq window (selected-window)))
      (with-selected-window window
        (lg/vterm-jump-to-bottom-if-live))))

  (add-hook 'window-selection-change-functions #'lg/vterm-on-window-selection-change)
  (add-hook 'window-buffer-change-functions #'lg/vterm-on-window-buffer-change)

  ;; --- Fix auto-dim-other-buffers for vterm ---
  ;; A vterm commit (e96c53f) changed `vterm--get-color' to return hardcoded
  ;; hex strings instead of nil for default colors.  When it returned nil the
  ;; C module fell back to the Emacs `default' face, which respects face
  ;; remapping from auto-dim-other-buffers.  Restore the old behavior.
  ;; See: https://github.com/mina86/auto-dim-other-buffers.el/issues/35

  (defun lg/vterm--get-color (index &rest _args)
    "Resolve a vterm color INDEX via face-foreground so that face
remapping (e.g. auto-dim-other-buffers) is respected.
Returns nil for default fg/bg so the C module uses the `default' face."
    (cond
     ((and (>= index 0) (< index 16))
      (face-foreground
       (elt vterm-color-palette index)
       nil 'default))
     ((= index -11)
      (face-foreground 'vterm-color-underline nil 'default))
     ((= index -12)
      (face-background 'vterm-color-inverse-video nil 'default))
     (t nil)))

  (advice-add 'vterm--get-color :override #'lg/vterm--get-color)

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
        ;; Search shell history. In vterm sends to live prompt via
        ;; vterm-send-string; elsewhere falls back to consult-history's
        ;; built-in handling (eshell/comint/term/minibuffer). C-r stays
        ;; with zsh's native reverse-i-search (vterm passthrough).
        "s h" #'lg/consult-history-dwim
        )
  )
