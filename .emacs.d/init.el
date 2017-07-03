;; Backup and autosave lications
;;==============================================================================
;;backup
(setq
 backup-by-copying t                ; don't clobber symlinks
 backup-directory-alist
 '(("." . "~/.emacs.d/backups"))    ; don't litter my fs tree
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)                 ; use versioned backups

;;autosave
(setq auto-save-file-name-transforms
 `((".*" ,temporary-file-directory t)))


;; Interface options
;;==============================================================================
;;paren mode
(show-paren-mode 1)

;;hide/show
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
(dolist (mode code-editing-mode-hooks)
  (add-hook mode 'hs-minor-mode))

;;line numbers
(global-linum-mode t)
(setq linum-format "%4d\u2502 ")


;; Keyboard Shortcuts
;;==============================================================================
(global-set-key (kbd "C-h") 'hs-toggle-hiding)
(global-set-key (kbd "M-h") 'hs-hide-all)
(global-set-key (kbd "M-s") 'hs-show-all)

(global-set-key (kbd "C-x a RET") 'align)

;; Styles
;;==============================================================================
;;C++
(setq c-default-style "linux"
      c-basic-offset 4)


;; Misc
;;==============================================================================
;;iBuffer
(global-set-key (kbd "C-b") 'ibuffer)
(autoload 'ibuffer "ibuffer" "List buffers." t)


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
 '(package-selected-packages
   (quote
    (neotree markdown-mode flymd web-mode multi-web-mode pbcopy simpleclip scala-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;Load use-package.el
(load "~/.emacs.d/use-package")
