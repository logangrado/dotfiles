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

;; MAYBE DEFINE UP/DOWN to K/J (capital)?
;; However, J (bound to evil-join) seems pretty useful
(define-key evil-normal-state-map (kbd "K") (lambda ()
                    (interactive)
                    (previous-line 20)
                    ;; (evil-scroll-line-up 10)
                    ))
(define-key evil-normal-state-map (kbd "J") (lambda ()
                      (interactive)
                      (next-line 20)
                      ;; (evil-scroll-line-down 10)
                      ))

;; SAVE BUFFER ON INSERT MODE EXIT
(add-hook 'evil-insert-state-exit-hook
          (lambda ()
            (call-interactively #'save-buffer)))

;; DONT FORGET
;; Use '/' to search, n/N to move foward/backward
;; You can then use :%s/<A>/<B> to REPLACE A with B (just like SED!) I'm sure it can handle regex too
;; You can use :%s on it's own (cool!). Don't yet know how to navigate between matches, and accept/reject, OR
;; how to limit

;; EXTERNAL PACAKGE CONFIG
;;=================================================================
;; Load all files in packages/
(mapc 'load (file-expand-wildcards "~/.doom.d/packages/*.el"))
