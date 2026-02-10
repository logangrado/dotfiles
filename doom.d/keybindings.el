;;; ../.dotfiles/doom.d/keybindings.el -*- lexical-binding: t; -*-

;; Evil State Maps and Their Abbreviations
;; ---------------------------------------
;; | State Name       | Keymap                     | Abbreviation | Description                               |
;; |------------------|---------------------------|-------------|-------------------------------------------|
;; | Normal          | evil-normal-state-map      | :n          | Standard mode for navigation and editing |
;; | Insert          | evil-insert-state-map      | :i          | Typing mode, like Vim insert mode        |
;; | Visual          | evil-visual-state-map      | :v          | Selection mode for editing text          |
;; | Visual Block    | evil-visual-state-map      | :x          | Blockwise selection (`Ctrl-v` in Vim)    |
;; | Visual Line     | evil-visual-line-state-map | :l          | Linewise selection (`V` in Vim)          |
;; | Operator        | evil-operator-state-map    | :o          | Operator-pending (e.g., `d`, `y`, `c`)   |
;; | Motion          | evil-motion-state-map      | :m          | Read-only navigation (like `view` mode)  |
;; | Replace         | evil-replace-state-map     | :r          | Overwrite mode (`R` in Vim)              |
;; | Emacs           | evil-emacs-state-map       | :e          | Disables Evil, uses native Emacs keys    |

;; Abbreviations can be combined =>
;;     :map :ni => :map (evil-normal-state-map evil-insert-state-map)

;; Special maps:
;; `leader` -> (map! :leader
;;     Equivalent to (map! :map (doom-leader-map)
;; `leader` -> (map! :localleader
;;     Equivalent to (map! :map (doom-localleader-map)

;; COPY/PASTE EDITING:
;; NORMAL/VISUAL MODE:
;; y : yank (copy)
;; x : cut
;; p/P : paste/paste after

;; COPY/PASTE
;; -----------
;; Copy (visual and vterm)
(map! :map (evil-visual-state-map vterm-copy-mode-map)
      "y" #'evil-yank ;; Better than kill-ring-save, works in various visual states
      )
;; Cut (visual only)
(map! :map (evil-visual-state-map)
      "x" #'evil-delete ;; Better than kill-region, works in various visual states
      )
;; Paste
(map! :map (evil-normal-state-map)
      "p" #'evil-paste-before
      "P" #'evil-paste-after
      "zp" #'yank-from-kill-ring
      "zP" #'lg/yank-from-kill-ring-after
      )
;; Paste insert mode
(map! :map (evil-insert-state-map)
      "C-p" #'yank
      "C-S-p" #'yank-from-kill-ring
      )
;; Paste in vterm. Must use vterm-yank commands
(map! :map vterm-mode-map
      ;; "p" #'vterm-yank
      ;; "P" #'lg/vterm-yank-from-kill-ring
      :n "p" #'vterm-yank
      :n "P" #'lg/vterm-yank-from-kill-ring
      :i "C-p" #'vterm-yank
      :i "C-S-P" #'lg/vterm-yank-from-kill-ring
      )



;; LEGACY - DELETE ATER 25-01
;; ---------------------------
(map! :map (evil-normal-state-map evil-insert-state-map evil-visual-state-map vterm-mode-map vterm-copy-mode-map)
      "M-w" #'kill-ring-save
      )
(map! :map (evil-normal-state-map evil-insert-state-map vterm-mode-map evil-vi)
      "C-w" #'kill-region
      "C-y" #'yank
      "M-y" #'yank-from-kill-ring
      "C-k" #'kill-line
      )
;; ---------------------------

(map! :map (evil-normal-state-map)
      "ys" #'evil-surround-edit
      "yS" #'evil-Surround-edit
      "yc" #'evil-surround-change
      "yd" #'evil-surround-delete
      "u" #'evil-undo
      "U" #'evil-redo
      )

;; These keybindings maintain compatability
;; (define-key evil-visual-state-map "3" 'comment-region)
(map! :map (evil-visual-state-map)
      "3" #'comment-or-uncomment-region
      "4" #'uncomment-region
      "M-h" #'lg/indent-rigidly-left-visual
      "M-l" #'lg/indent-rigidly-right-visual
      )
(map! :map (evil-visual-state-map evil-normal-state-map)
      "C--" #'comment-underline
      )

(map! :leader
      (:prefix ("f" . "file")
       :desc "Save & format buffer"          "s" #'+format/save-buffer
       :desc "Save w/o formatting (one-off)" "S" #'+format/save-buffer-no-reformat))

(map! :map (evil-normal-state-map evil-visual-state-map)
      "s" #'evil-snipe-s
      "S" #'evil-snipe-S
      "f" #'evil-snipe-f
      "F" #'evil-snipe-F
      ;; "x" #'evil-snipe-x
      ;; "X" #'evil-snipe-X
      )

(map! :map (dired-mode-map)
      "RET" #'find-alternate-file
      )

;; Windo resizing
;; ---------------
(map! :leader
      "=" #'acg/zoom-frame
      "-" #'acg/zoom-frame-out
      )

(map! :leader
      :desc "Start :%s substitution"
      "r" (lambda ()
            (interactive)
            (evil-ex "%s/\\v")))

;; Move to new window on creation
(map! :leader
      (:prefix "TAB"
               "c" #'cd
               "{" #'+workspace/swap-left
               "}" #'+workspace/swap-right
               )
      (:prefix "b"
               (:desc "Kill current buffer" "d" #'lg/kill-current-buffer)
               (:desc "Force kill current buffer" "D" #'lg/kill-current-buffer-force)
               "s" #'scratch-buffer
               )
      (:prefix "c"
               (:desc "Toggle ipdb" "b" #'lg/ipdb-toggle)
               )
      (:prefix "h"
               (:prefix "r"
                        (:desc "Reset (top-level)" "s" #'top-level)
                        (:desc "Full reset (reload doom + revert buffers" "R" #'lg/reset-emacs)
                        )
               )
      (:prefix "w"
               (:desc "H-Split and follow" "s" #'+evil/window-split-and-follow)
               (:desc "V-Split and follow" "v" #'+evil/window-vsplit-and-follow)
               "1" #'resize-window-to-9/10
               "2" #'resize-window-to-1/2
               "3" #'resize-window-to-1/3
               "#" #'resize-window-to-2/3
               )
      )

;; TO MAP
;; company-complete -> complete thing at point. NEED AVAILABLE IN INSERT MODE
