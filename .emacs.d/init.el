;; Backup and autosave lications
;;==============================================================================
;;backup

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq backup-by-copying t                ; don't clobber symlinks
      backup-directory-alist
      '(("." . "~/.emacs.d/backups/"))    ; don't litter my fs tree
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)                 ; use versioned backups

;;autosave
(setq auto-save-file-name-transforms
 `((".*" ,temporary-file-directory t)))


;; Interface options
;;-------------------------------------------------------------------
;;paren mode
(show-paren-mode 1)
(set-face-background 'show-paren-match "brightcyan")

;; smooth scrolling
(setq scroll-step            1
      scroll-conservatively  10000)

;; Hideshow
;;-------------------------------------------------------------------
(defvar code-editing-mode-hooks '(c-mode-common-hook
				  clojure-mode-hook
				  emacs-lisp-mode-hook
				  java-mode-hook
				  js-mode-hook
				  web-mode-hook
				  html-mode-hook
				  lisp-mode-hook
				  perl-mode-hook
				  python-mode-hook
				  sh-mode-hook))
;; add a hs-minor-mode hook to code editing major modes
(load-library "hideshow")
(dolist (mode code-editing-mode-hooks)
  (add-hook mode 'hs-minor-mode))

(add-to-list 'load-path "~/.emacs.d/hideshow-orgmode")
(require 'hideshow-orgmode)
(add-hook 'hs-minor-mode-hook 'hs-fold-all)

;; line numbers
;;-------------------------------------------------------------------
(global-linum-mode t)
(setq linum-format "%4d\u2502")
(set-face-background 'linum "brightblack")
(set-face-underline-p 'linum nil) ;;Dont underline linenumbers
(set-face-attribute 'linum nil :inverse-video nil)

;; theme
;;-------------------------------------------------------------------
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/solarized")
(load-theme 'solarized t)
;;(set-frame-parameter nil 'background-mode 'light)    ;;GUI
(set-terminal-parameter nil 'background-mode 'dark) ;;Terminal
;;set faces for git smerge
(defun set-smerge-faces ()
  ;(set-face-attribute 'smerge-mine            nil :background "cyan")
  ;(set-face-attribute 'smerge-other           nil :background "cyan")
  (set-face-attribute 'smerge-refined-removed nil :foreground "black" );:background "green");"#335533")
  (set-face-attribute 'smerge-refined-added   nil :foreground "black" );:background "green");"#553333")
  ;(set-face-attribute 'smerge-refined-changed nil :background "cyan")
  )
(add-hook 'smerge-mode-hook 'set-smerge-faces)

(set-face-attribute 'font-lock-comment-face nil           :foreground "brightred")
(set-face-attribute 'font-lock-comment-delimiter-face nil :foreground "brightred")
(set-face-attribute 'font-lock-doc-face nil               :foreground "cyan")

;; Keyboard Shortcuts
;;==============================================================================
(global-set-key (kbd "C-c c") 'hs-cycle)
(global-set-key (kbd "C-c C") 'hs-cycle-all)
(global-set-key (kbd "C-c h") 'hs-fold-block)
(global-set-key (kbd "C-c H") 'hs-fold-all)
(global-set-key (kbd "C-c s") 'hs-show-block)
(global-set-key (kbd "C-c S") 'hs-show-all)

(global-set-key (kbd "C-c a") 'align)
(global-set-key (kbd "C-c A") 'align-regexp)

;; Styles and Syntax Highlighting
;;==============================================================================
;;C++
(setq c-default-style "linux"
      c-basic-offset 4)
(setq-default indent-tabs-mode nil)
;;(add-to-list 'auto-mode-alist '("\\.scad\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.*rc$" . conf-unix-mode))

;; Define json minor mode
(defun json-indent-level ()
  (when (and (stringp buffer-file-name)
             (string-match "\\.json\\'" buffer-file-name))
    (setq js-indent-level 2)))
(add-hook 'find-file-hook 'json-indent-level)

;; Misc
;;==============================================================================
;;iBuffer
(global-set-key (kbd "C-b") 'ibuffer)
(autoload 'ibuffer "ibuffer" "List buffers." t)
(delete-selection-mode 1) ;; Delete selected text when you type

;; Include elpa directory and subdirectories
;;==============================================================================
;;(let ((default-directory  "~/.emacs.d/elpa/"))
;;  (normal-top-level-add-subdirs-to-load-path))

;; MELPA
;;==========================================================
;; MELPA initialization
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (solarized)))
 '(custom-safe-themes
   (quote
    ("8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(doc-view-continuous t)
 '(neo-hidden-regexp-list
   (quote
    ("\\.pyc\\'" ".*.pyc" "^\\." "\\.pyc$" "~$" "^#.*#$" "\\.elc$")))
 '(org-agenda-custom-commands
   (quote
    (("d" todo "DELEGATED" nil)
     ("c" todo "DONE|DEFERRED|CANCELLED" nil)
     ("w" todo "WAITING" nil)
     ("W" agenda ""
      ((org-agenda-ndays 21)))
     ("A" agenda ""
      ((org-agenda-skip-function
        (lambda nil
          (org-agenda-skip-entry-if
           (quote notregexp)
           "\\=.*\\[#A\\]")))
       (org-agenda-ndays 1)
       (org-agenda-overriding-header "Today's Priority #A tasks: ")))
     ("u" alltodo ""
      ((org-agenda-skip-function
        (lambda nil
          (org-agenda-skip-entry-if
           (quote scheduled)
           (quote deadline)
           (quote regexp)
           "
]+>")))
       (org-agenda-overriding-header "Unscheduled TODO entries: "))))))
 '(org-agenda-files
   (quote
    ("~/org/notes.org" "~/org/todo.org ~/org/notes.org")))
 '(org-agenda-ndays 7 t)
 '(org-agenda-show-all-dates t t)
 '(org-agenda-skip-deadline-if-done t t)
 '(org-agenda-skip-scheduled-if-done t t)
 '(org-agenda-start-on-weekday nil t)
 '(org-deadline-warning-days 14)
 '(org-default-notes-file "~/notes.org")
 '(org-fast-tag-selection-single-key (quote expert))
 '(org-remember-store-without-prompt t)
 '(org-remember-templates
   (quote
    ((116 "* TODO %?
  %u" "~/todo.org" "Tasks")
     (110 "* %u %?" "~/notes.org" "Notes"))))
 '(org-reverse-note-order t)
 '(package-selected-packages
   (quote
    (exec-path-from-shell latex-preview-pane list-packages-ext org-bullets synonymous magit doom-themes-org doom-themes outline-magic scad-mode auto-complete outshine pretty-mode which-key prettify-greek org org-mode matlab-mode web-mode use-package pbcopy nlinum neotree markdown-mode flymd auctex adaptive-wrap 0blayout)))
 '(remember-annotation-functions (quote (org-remember-annotation)))
 '(remember-handler-functions (quote (org-remember-handler))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-keyword-face ((t (:foreground "green"))))
 '(markdown-code-face ((t (:inherit fixed-pitch)))))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;Load use-package.el
(load "~/.emacs.d/use-package")
