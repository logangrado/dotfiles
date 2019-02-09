(use-package magit
  :defer t
  ; Dont really know how to use this yet...
  )

(use-package auto-complete
  :defer t
  :config
  (ac-config-default)
  )

(use-package outline-magic
  :defer t
  )

(use-package scad-mode
  :defer t
  :init
  :config
  )

;;pretty-mode
(use-package pretty-mode
  :defer t
  :init
  (add-hook 'python-mode-hook 'turn-on-pretty-mode)
  :config
  (pretty-activate-groups '(:greek)) ;;:sub-and-superscripts))
  (pretty-deactivate-patterns '(:==)) ;;disable eqauls
  )

;;which-key
(use-package which-key
  :defer t
  :config
  (which-key-mode)
  )

;;org-mode
(use-package org
  :ensure t
  :init
  (setq org-latex-create-formula-image-program 'dvisvgm)
  
  (setq org-agenda-files (list "~/org/todo.org"
                               "~/org/notes.org"
                               "~/org/refile.org"))
  (setq org-default-notes-file "~/org/refile.org")

  (setq org-agenda-ndays 7)
  (setq org-deadline-warning-days 14)
  (setq org-agenda-show-all-dates t)
  (setq org-agenda-skip-deadline-if-done t)
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-start-on-weekday nil)
  (setq org-reverse-note-order t)

  
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline "~/org/refile.org" "Tasks")
           "* TODO %?\n  %u\n  %a")))

  (setq org-todo-keywords
        '((sequence "TODO" "IN PROG" "|" "DONE")))
  
  (global-set-key (kbd "C-c a") 'org-agenda)
  (global-set-key (kbd "C-c r") 'org-capture)
  
  :config
  (add-hook 'org-mode-hook 'flyspell-mode)
  (set-face-attribute 'org-todo nil :background "Bright Cyan") ;:background "yellow")
  (setq org-startup-indented t)
  (setq org-hierarchical-todo-statics nil)
  ;;(setq org-fontify-done-headline t)
  (setq org-checkbox-hierarchical-statistics nil)
  )

;;doom-themes-org
(use-package doom-themes
  :defer t
  :config
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t)   ; if nil, italics is universally disabled
  (doom-themes-org-config)
  (doom-org-custom-fontification)
  )


;;outshine
(use-package outshine
  :defer t
  )

;;auctex
(use-package auctex
  :defer t
  :mode ("\\.tex\\'" . latex-mode)
  :commands
  (latex-mode LaTeX-mode plain-tex-mode)
  :init
  (add-hook 'LaTeX-mode-hook 'LaTeX-preview-setup)
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)

  ;; Colorize diff
  (defface DIFadd
  '((t :foreground "#268BD2"
       :weight bold
       ))
  "Face for DIFadd"
  :group 'my-lang-mode )
  (defface DIFdel
    '((t :foreground "#DC322F"
         :weight bold
         ))
  "Face for DIFdel"
  :group 'my-lang-mode )
  (setq font-latex-user-keyword-classes
        '(("DIFadd" (("DIFadd" "{")) DIFadd)
          ("DIFdel" (("DIFdel" "{")) DIFdel)
          ("DIFaddFL" (("DIFaddFL" "{")) DIFadd)
          ("DIFdelFL" (("DIFdelFL" "{")) DIFdel)))
    
  (setq TeX-auto-save t
	TeX-parse-self t
	TeX-save-query nil
	TeX-PDF-mode t)
  (setq-default TeX-master nil)
  (add-hook 'TeX-mode-hook (lambda ()
			     (TeX-fold-mode t)
			     (delete '("[l]" ("label")) TeX-fold-macro-spec-list)
			     (TeX-fold-mode t)
			     (add-hook 'find-file-hook 'TeX-fold-buffer t t) ;;Autohide all when opening buffer
			     (add-hook 'after-change-functions               ;;Autohide after typing '}' or '$'
				       (lambda (start end oldlen) 
					 (when (= (- end start) 1)
					   (let ((char-point 
						  (buffer-substring-no-properties 
						   start end)))
					     (when (or (string= char-point "}")
						       (string= char-point "$"))
					       (TeX-fold-paragraph)))))
				       t t)))
  (add-hook 'TeX-mode-hook 'outline-minor-mode)
  (add-hook 'LaTeX-mode-hook (lambda() (bind-key (kbd "C-c c") 'outline-cycle)))
  :config
  )

;;flyspell
(use-package flyspell
  :defer t
  :init
  (flyspell-mode 1)
  :config
  (setq ispell-program-name "ispell")
  (setq ispell-dictionary "english")
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  ;;(add-hook 'LaTeX-mode-hook 'flyspell-buffer)
  )

;;pbcopy
;;========================================================================
(use-package pbcopy
  :defer t
  :init
  :config
  (turn-on-pbcopy)
  )
  
;;web-mode
;;========================================================================
(use-package web-mode
  :defer t
  :init
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  ;;(set-face-attribute 'web-mode-html-tag-face nil :foreground "#990000") ;;set tags to dark red
  (set-face-attribute 'web-mode-html-tag-face nil :foreground "#0000B2") ;;set tags to dark blue
  (set-face-background 'web-mode-current-element-highlight-face "#999999") ;;set highlight color to white

  ;;set indent to 2
  (defun my-web-mode-hook ()
    "Hooks for Web mode."
    (setq web-mode-markup-indent-offset 2)
    )
  (add-hook 'web-mode-hook  'my-web-mode-hook)
  
  (setq web-mode-enable-current-element-highlight t) ;;highlight current element
  ;;(setq web-mode-enable-current-column-highlight t)  ;;highlight current column
  (setq web-mode-enable-auto-closing t) 
  (setq web-mode-enable-auto-pairing t)  
  )

;;flymd
;;==========================================================================
(use-package flymd
  :defer t
  :config
  (add-to-list 'flymd-markdown-file-type '"\\.mdown\\'")
  (defun my-flymd-browser-function (url)
    (let ((process-environment (browse-url-process-environment)))
    (apply 'start-process
           (concat "firefox " url)
           nil
           "/usr/bin/open"
           (list "-a" "firefox" url))))
  (setq flymd-browser-open-function 'my-flymd-browser-function)
  )

;;(use-package markdown-mode
  ;;:ensure t
;;)

;;neotree
;;==========================================================================
(use-package neotree
  :ensure t
  :init
  :config
  (global-set-key (kbd "C-t") 'neotree-toggle)
  (set-face-attribute 'neo-file-link-face nil :foreground "#999999") ;;change file face color to white
  (setq-default neo-show-hidden-files t) ;;show hidden files
  (setq-default neo-window-width 50) ;;set width (default 25)
  (setq-default neo-window-position 'right) ;;put window on right
  (setq-default neo-autorefresh t) ;;automatically refresh tree2

  ;; hide neotree on file select
  (defun neo-open-file-hide (full-path &optional arg) 
    "Open a file node and hides tree."
    (neo-global--select-mru-window arg)
    (find-file full-path)
    (neotree-hide))
  (defun neotree-enter-hide (&optional arg)
    "Enters file and hides neotree directly"
    (interactive "P")
    (neo-buffer--execute arg 'neo-open-file-hide 'neo-open-dir))
  (add-hook
   'neotree-mode-hook
   (lambda ()
     (define-key neotree-mode-map (kbd "RET") 'neotree-enter-hide)))

  ;; Hides certain file types/extensions. Can be customized in list neo-hidden-regexp-list
  (setq neo-show-hidden-files nil)
  ;;(add-to-list 'neo-hidden-regexp-list)
  )
  
;;adaptive wrap
;;========================================================================
(use-package adaptive-wrap
  :ensure t
  :config
  (define-globalized-minor-mode global-visual-line-mode visual-line-mode
    (lambda () (visual-line-mode 1)))
  (global-visual-line-mode t)
  (add-hook 'visual-line-mode-hook 'adaptive-wrap-prefix-mode)
)
