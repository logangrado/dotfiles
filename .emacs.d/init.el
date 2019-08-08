;; Backup and autosave lications
;;==============================================================================
;;backup
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

;; Line Numbers
;;-------------------------------------------------------------------
(global-linum-mode t)
(setq linum-format "%4d\u2502")
(set-face-background 'linum "brightblack")
(set-face-underline-p 'linum nil) ;;Dont underline linenumbers
(set-face-attribute 'linum nil :inverse-video nil)

;; Colors and Themes
;;-------------------------------------------------------------------
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/solarized")
(load-theme 'solarized t)
(set-frame-parameter nil 'background-mode 'light)    ;;GUI
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

(global-set-key (kbd "C-b") 'ibuffer)

;; Navigation
(global-set-key (kbd "M-k") 'next-line)
(global-set-key (kbd "M-i") 'previous-line)
(global-set-key (kbd "M-j") 'backward-char)
(global-set-key (kbd "M-l") 'forward-char)
(global-set-key (kbd "M-u") 'beginning-of-line)
(global-set-key (kbd "M-o") 'end-of-line)

(global-set-key (kbd "C-M-i") 'backward-sexp)
(global-set-key (kbd "C-M-k") 'forward-sexp)
(global-set-key (kbd "C-M-j") 'backward-sexp)
(global-set-key (kbd "C-M-l") 'forward-sexp)

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
(autoload 'ibuffer "ibuffer" "List buffers." t)
(delete-selection-mode 1) ;; Delete selected text when you type

;; Turn off god forsaken change-log-mode
(setq auto-mode-alist
      (append '(("[cC]hange\\.?[lL]og?\\'" . markdown-mode)
                ("[cC]hange[lL]og[-.][0-9]+\\'" . markdown-mode)
                ("\\$CHANGE_LOG\\$\\.TXT" . markdown-mode)
                ("[cC]hange[lL]og[-.][-0-9a-z]+\\'" . markdown-mode))
              auto-mode-alist))

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

;; OS SPECIFIC CONFIG
;;===============================================================================
(when (string-equal system-type "darwin")
  ;; Colors and Themes
  ;;------------------------------------------------------------------
  (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/solarized")
  (load-theme 'solarized t)
  (set-terminal-parameter nil 'background-mode 'dark) ;;Terminal
  (set-frame-parameter nil 'background-mode 'light)    ;;GUI
  )

(when (string-equal system-type "windows-nt")
  )

(when (string-equal system-type "gnu/linux")
  (set-face-background 'linum 'unspecified)  ;; Unset linenum face background
  )

;; Custom
;;==============================================================================
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)


;; Use Package
;;==============================================================================
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;Load use-package.el
(load "~/.emacs.d/use-package")
