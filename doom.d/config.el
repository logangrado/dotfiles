;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Logan Grado"
      user-mail-address "grado.logan@gmail.com")

;; VISUAL PROPERTIES
;;============================================================================

;;FONT
;;----
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept.
(if (string-equal system-type "darwin")
    (setq doom-font (font-spec :family "Menlo" :size 12))
    (setq doom-font (font-spec :size 12)))
(setq doom-theme 'doom-one)
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; GENERAL KEYBINDINGS
;; ================================================================
;; These keybindings maintain compatability
(define-key evil-visual-state-map "3" 'comment-region)
(define-key evil-visual-state-map "4" 'uncomment-region)
(define-key evil-normal-state-map "\C-w" 'evil-delete)
(define-key evil-normal-state-map "U" 'evil-redo)
(define-key evil-insert-state-map "\C-w" 'evil-delete)
(define-key evil-visual-state-map "\C-w" 'evil-delete)
(define-key evil-normal-state-map "\C-y" 'yank)
(define-key evil-insert-state-map "\C-y" 'yank)
(define-key evil-visual-state-map "\C-y" 'yank)
(define-key evil-normal-state-map "\C-k" 'kill-line)
(define-key evil-insert-state-map "\C-k" 'kill-line)
(define-key evil-visual-state-map "\C-k" 'kill-line)
(define-key evil-normal-state-map "Q" 'call-last-kbd-macro)
(define-key evil-visual-state-map "Q" 'call-last-kbd-macro)
;; Not sure what this one does, but let's us toggle beginnign/end
;; of line with TAB
(define-key evil-normal-state-map (kbd "TAB") 'evil-undefine)
(defun evil-undefine ()
 (interactive)
 (let (evil-mode-map-alist)
   (call-interactively (key-binding (this-command-keys)))))

;; TODO Figure out how to move screen if point is within ~10/20 lines of top/bottom on scrolling
;; Them remove the (evil-scroll-line-[up|down]])
;; This seems controllable with `scroll-margin' and `maximum-scroll-margin'
;; (setq scroll-margin 10) -> leaves a margin of 10. Works smoothly scrolling down, but jumps on the way up.
(setq scroll-margin 10) ;; Leave a margin of 10 at top/bottom.
                        ;; FIXME Works smoothly scrolling down, but not up.
(define-key evil-normal-state-map (kbd "K") (lambda ()
                    (interactive)
                    (previous-line 10)
                    (evil-scroll-line-up 10)))
(define-key evil-normal-state-map (kbd "J") (lambda () ;; Overrides evil-join, may want to re-bind
                      (interactive)
                      (next-line 10)
                      (evil-scroll-line-down 10)))

;; Move to new window on creation
(map! :leader
      ;; Move to new window on creation
      "w s" #'+evil/window-split-and-follow
      "w v" #'+evil/window-vsplit-and-follow
      ;; Bind keys to swap workspaces
      "TAB {" #'+workspace/swap-left
      "TAB }" #'+workspace/swap-right
      )

;; SAVE BUFFER ON INSERT MODE EXIT
(defun my-save-if-bufferfilename ()
  (if (buffer-file-name)
      (progn
        (save-buffer))))
(add-hook 'evil-insert-state-exit-hook 'my-save-if-bufferfilename)

;; Always switch to normal mode when switching windows or buffers
(defun my-set-mode ()
  (evil-normal-state)
  )
(add-hook 'doom-switch-window-hook 'my-set-mode)

;; Esc quits most things
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

;; Exclude these modes from evil escape (remove vterm mode from default)
(setq evil-escape-excluded-major-modes nil)

;; DONT FORGET
;; Use '/' to search, n/N to move foward/backward
;; You can then use :%s/<A>/<B> to REPLACE A with B (just like SED!) I'm sure it can handle regex too
;; You can use :%s on it's own (cool!). Don't yet know how to navigate between matches, and accept/reject, OR
;; how to limit

;; EXTERNAL PACAKGE CONFIG
;;=================================================================
;; Load all files in packages/
(mapc 'load (file-expand-wildcards "~/.doom.d/packages/*.el"))
