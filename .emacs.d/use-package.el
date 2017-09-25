;;auctex
(use-package auctex
  :ensure t
  :mode ("\\.tex\\'" . latex-mode)
  :commands (latex-mode LaTeX-mode plain-tex-mode)
  :init
  (add-hook 'LaTeX-mode-hook #'LaTeX-preview-setup)
  (add-hook 'LaTeX-mode-hook #'flyspell-mode)
  (add-hook 'LaTeX-mode-hook #'turn-on-reftex)

  (add-hook 'LaTeX-mode-hook (lambda ()
                             (TeX-fold-mode 1)))
  ;;(add-hook 'find-file-hook 'TeX-fold-buffer t) ;;automatically hide all foldable items
  
  (setq TeX-auto-save t
	TeX-parse-self t
	TeX-save-query nil
	TeX-PDF-mode t)
  (setq-default TeX-master nil)

  :config
  
  )


;;flyspell
(use-package flyspell
  :ensure t
  :init
  (flyspell-mode 1)
  :config
  (setq ispell-program-name "ispell")
  (setq ispell-dictionary "english")
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'LaTeX-mode-hook 'flyspell-buffer)
  )

(use-package matlab-mode
  :ensure t
  )

;;pbcopy
;;========================================================================
(use-package pbcopy
  :ensure t
  :init
  :config
  (turn-on-pbcopy)
  )
  
;;web-mode
;;========================================================================
(use-package web-mode
  :ensure t
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
  :ensure t
  :config
  (add-to-list 'flymd-markdown-file-type '"\\.mdown\\'")
  )

(use-package markdown-mode
  :ensure t
  )

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

;;nlinum
;;(use-package nlinum
;;  :ensure t
;;  :init
;;  (global-nlinum-mode t)
;;  )
