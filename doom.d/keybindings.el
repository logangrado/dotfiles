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

(map! :map (evil-normal-state-map evil-insert-state-map evil-visual-state-map vterm-mode-map vterm-copy-mode-map)
      "M-w" #'kill-ring-save
      )

(map! :map (evil-normal-state-map evil-insert-state-map vterm-mode-map evil-vi)
      "C-w" #'kill-region
      "C-y" #'yank
      "M-y" #'yank-from-kill-ring
      "C-k" #'kill-line
      )

(map! :map (evil-normal-state-map)
      "X" #'kill-region
      "p" #'evil-paste-before
      "P" #'evil-paste-after
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
      )


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
