(use-package python-black
  :ensure t
  :after python
  :hook (python-mode . python-black-on-save-mode)
  :config
  (setq python-black-extra-args (list "-l 120"))
  )

(use-package protobuf-mode
  :ensure t
  :init
  (defconst my-protobuf-style
    '((c-basic-offset . 2)
      (indent-tabs-mode . nil)))
  
  (add-hook 'protobuf-mode-hook
            (lambda () (c-add-style "my-style" my-protobuf-style t)))
  )
  
(use-package jsonnet-mode
  :ensure t
  )

(use-package log4j-mode
  :ensure t
  )

;; (use-package logview
;;   :ensure t
;;   :init
;;   (setenv "TZ" "US/Pacific")
;;   ;;(setq datetime-timezone "US/Pacific")
;;   )

(use-package origami
  :ensure t
  :init
  (add-hook 'yaml-mode-hook 'origami-mode)
  ;(dolist (mode code-editing-mode-hooks)
  ;  (add-hook mode 'origami-mode))
  :config
  ;; (add-hook 'origami-mode-hook
  ;;           (lambda ()
  ;;             (origami-close-all-nodes (current-buffer))))
  (setq origami-mode-map (make-sparse-keymap))
  (use-local-map origami-mode-map)
  :bind (:map origami-mode-map
              ("M-c"   . origami-recursively-toggle-node)
              ("C-c c" . origami-recursively-toggle-node)
              ("C-c C" . origami-toggle-all-nodes)
              ("C-c h" . origami-close-node)
              ("C-c H" . origami-close-all-nodes)
              ("C-c s" . origami-open-node)
              ("C-c S" . origami-open-all-nodes))
  )

(use-package dockerfile-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))
  )
  
(use-package yaml-mode
  :ensure t
  )
  
(use-package csv-mode
  :ensure t
  :config
  ;;(csv-align-fields-mode)
  )

(use-package tabbar
  :ensure t
  :init
  (tabbar-mode)
  :config
 
  (set-face-attribute 'tabbar-default nil :background "brightcyan" :foreground "brightcyan")
  (set-face-attribute 'tabbar-unselected nil
                      :background "brightblack"
                      :foreground "brightcyan")

  (set-face-attribute 'tabbar-selected nil
                      :background "brightcyan"
                      :foreground "brightblack")

  ;; Add space to make less crowded
  (defadvice tabbar-buffer-tab-label (after fixup_tab_label_space_and_flag activate)
    (setq ad-return-value
          (if (and (buffer-modified-p (tabbar-tab-value tab))
                   (buffer-file-name (tabbar-tab-value tab)))
              (concat " + " (concat ad-return-value " "))
            (concat " " (concat ad-return-value " ")))))

  (defun tabbar-buffer-groups ()
    "Return the list of group names the current buffer belongs to.
    This function is a custom function for tabbar-mode's tabbar-buffer-groups.
    This function group all buffers into 3 groups:
    Those Dired, those user buffer, and those emacs buffer.
    Emacs buffer are those starting with “*”."
    (list
     (cond
      ((string-equal "*" (substring (buffer-name) 0 1))
       "Emacs Buffer"
       )
      ((eq major-mode 'org-mode)
       "Org"
       )
      ((eq major-mode 'dired-mode)
       "Dired"
       )
      (t
       "User Buffer"
       )
      ))) 
  
  (defun tabbar-move-current-tab-one-place-left ()
    "Move current tab one place left, unless it's already the leftmost."
    (interactive)
    (let* ((bufset (tabbar-current-tabset t))
           (old-bufs (tabbar-tabs bufset))
           (first-buf (car old-bufs))
           (new-bufs (list)))
      (if (string= (buffer-name) (format "%s" (car first-buf)))
          old-bufs ; the current tab is the leftmost
        (setq not-yet-this-buf first-buf)
        (setq old-bufs (cdr old-bufs))
        (while (and
                old-bufs
                (not (string= (buffer-name) (format "%s" (car (car old-bufs))))))
          (push not-yet-this-buf new-bufs)
          (setq not-yet-this-buf (car old-bufs))
          (setq old-bufs (cdr old-bufs)))
        (if old-bufs ; if this is false, then the current tab's buffer name is mysteriously missing
            (progn
              (push (car old-bufs) new-bufs) ; this is the tab that was to be moved
              (push not-yet-this-buf new-bufs)
              (setq new-bufs (reverse new-bufs))
              (setq new-bufs (append new-bufs (cdr old-bufs))))
          (error "Error: current buffer's name was not found in Tabbar's buffer list."))
        (set bufset new-bufs)
        (tabbar-set-template bufset nil)
        (tabbar-display-update))))
  
  (defun tabbar-move-current-tab-one-place-right ()
    "Move current tab one place right, unless it's already the rightmost."
    (interactive)
    (let* ((bufset (tabbar-current-tabset t))
           (old-bufs (tabbar-tabs bufset))
           (first-buf (car old-bufs))
           (new-bufs (list)))
      (while (and
              old-bufs
              (not (string= (buffer-name) (format "%s" (car (car old-bufs))))))
        (push (car old-bufs) new-bufs)
        (setq old-bufs (cdr old-bufs)))
      (if old-bufs ; if this is false, then the current tab's buffer name is mysteriously missing
          (progn
            (setq the-buffer (car old-bufs))
            (setq old-bufs (cdr old-bufs))
            (if old-bufs ; if this is false, then the current tab is the rightmost
                (push (car old-bufs) new-bufs))
            (push the-buffer new-bufs)) ; this is the tab that was to be moved
        (error "Error: current buffer's name was not found in Tabbar's buffer list."))
      (setq new-bufs (reverse new-bufs))
      (setq new-bufs (append new-bufs (cdr old-bufs)))
      (set bufset new-bufs)
      (tabbar-set-template bufset nil)
      (tabbar-display-update)))
  
  (setq tabbar-buffer-groups-function 'tabbar-buffer-groups)

  (global-set-key (kbd "ESC <left>") 'tabbar-backward-tab)
  (global-set-key (kbd "ESC <right>") 'tabbar-forward-tab)
  (global-set-key (kbd "M-<up>") 'tabbar-backward-group)
  (global-set-key (kbd "M-<down>") 'tabbar-forward-group)

  (global-set-key (kbd "C-c t <left>") 'tabbar-move-current-tab-one-place-left)
  (global-set-key (kbd "C-c t <right>") 'tabbar-move-current-tab-one-place-right)
  
  )

(use-package iedit
  :ensure t
  :init
  (global-set-key (kbd "C-c i") 'iedit-mode)
  (custom-set-faces
   '(iedit-occurrence ((t (:background "yellow" :foreground "black")))))
  )

(use-package undo-tree
  :ensure t
  :init
  (global-undo-tree-mode)
  ;; Keybindings:
  ;;   C-_   : Undo,
  ;;   M-_   : Redo,
  ;;   C-x u : Show tree
  )

(use-package exec-path-from-shell   ;; For GUI emacs
   :ensure t
   :config
   (setq exec-path-from-shell-check-startup-files nil)  ; suppress warning message
   (exec-path-from-shell-initialize)                    ; use shell path
   )

(use-package magit
  :ensure t
  ; Dont really know how to use this yet...
  )

(use-package auto-complete
  :ensure t
  :init
  (ac-config-default)
  )

(use-package outline-magic
  :ensure t
  :init
  :config
  )

(use-package scad-mode
  :ensure t
  :init
  :config
  )

;;pretty-mode
(use-package pretty-mode
  :ensure t
  :init
  (add-hook 'python-mode-hook 'turn-on-pretty-mode)
  :config
  (pretty-activate-groups '(:greek)) ;;:sub-and-superscripts))
  (pretty-deactivate-patterns '(:== :return :def)) ;;disable eqauls
  )

;;which-key
(use-package which-key
  :ensure t
  :init
  (which-key-mode)
  )

;;real-auto-save
(use-package real-auto-save
  :ensure t
  :init
  (setq real-auto-save-interval 10) ;; in seconds
  :config
  )

;;org-mode
(use-package org
  :ensure t
  :init
  (setq org-latex-create-formula-image-program 'dvisvgm)

  (setq org-agenda-files (list "~/org/todo"))
  (setq org-default-notes-file "~/org/todo/inbox.org")

  (setq org-agenda-ndays 7)
  (setq org-deadline-warning-days 14)
  (setq org-agenda-show-all-dates t)
  (setq org-agenda-skip-deadline-if-done t)
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-start-on-weekday nil)
  (setq org-reverse-note-order t)

  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline "~/org/todo/inbox.org")
           "* TODO %?\n  %u\n  %a")))

  (setq org-todo-keywords
        '((sequence "TODO(t)" "IN PROG(p)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c)")
          (sequence "TODO(t)" "IN PROG(p)" "PENDING REVIEW(p)" "IN REVIEW(r)" "|" "DONE(d)" "CANCELED(c)")
          (sequence "DELEGATED" "|" "DONE")))
  
  (global-set-key (kbd "C-c a") 'org-agenda)
  (global-set-key (kbd "C-c r") 'org-capture)

  ;; Org Archive
  (setq org-archive-location ".%s_archive::") ;; Hide org archive files
  
  ;; Function to archive all DONE items in file
  (defun org-archive-done-tasks ()
    (interactive)
    (org-map-entries 'org-archive-subtree "/DONE" 'file))
  (global-set-key (kbd "C-c C-x C-A") 'org-archive-done-tasks)

  ;; Org-mode-hooks
  ;; auto-save and auto-revert
  (add-hook 'org-mode-hook 'real-auto-save-mode)
  (add-hook 'org-mode-hook 'auto-revert-mode)  
  
  :config
  ;;=============================================================
  (add-hook 'org-mode-hook 'flyspell-mode)
  (set-face-attribute 'org-todo nil :background "Red")
  (setq org-startup-indented t)
  (setq org-hierarchical-todo-statistics nil)         ;; Nil means children count, not just top leve
  (setq org-checkbox-hierarchical-statistics nil)

  ;; Org-column settings
  (setq org-agenda-overriding-columns-format "%TODO %ALLTAGS %ITEM")
  (set-face-attribute 'org-column nil :inverse-video nil)
  (setq org-agenda-view-columns-initially t)

  ;; org-mode only keybindings
  (global-set-key (kbd "M-t") 'org-todo)
  
  ;; Org colors
  (setq org-todo-keyword-faces
      '(("TODO" .    (:foreground "red" :weight bold :inverse-video t))
        ("IN PROG" . (:foreground "yellow" :weight bold :inverse-video t))
        ("WAITING" . (:foreground "cyan" :weight bold :inverse-video t))
        ("PENDING REVIEW" . (:foreground "cyan" :weight bold :inverse-video t))
        ("IN REVIEW" . (:foreground "yellow" :weight bold :inverse-video t))
        ("DONE" .    (:foreground "green" :weight bold))
        ("CANCELED" .    (:foreground "red" :weight bold))))

  
  ;; Checkbox colors
  ;; Changes the color of the text behind a checkbox
  ;;==================================================
  ;; Define the checkbox todo text
  (defface org-checkbox-todo-text
    ;;'((t (:inherit org-todo)))
    '((t (
          :inherit default
          :foreground "red"
          :weight normal)))
    "Face for the text part of an unchecked org-mode checkbox.")
  ;; Add keyword for it
  (font-lock-add-keywords
   'org-mode
   `(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?: \\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)" 1 'org-checkbox-todo-text prepend))
   'append)

  ;; Define done
  (defface org-checkbox-done-text
    '((t (
          :foreground "bright green"
          )))
    ;;'((t (:inherit org-done)))
    "Face for the text part of a checked org-mode checkbox.")

  ;; Add keyworkd for it
  (font-lock-add-keywords
   'org-mode
   `(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:X\\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)" 1 'org-checkbox-done-text prepend))
   'append)  
  )

;;doom-themes-org
;; (use-package doom-themes
;;   :ensure t
;;   :config
;;   (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
;;         doom-themes-enable-italic t)   ; if nil, italics is universally disabled
;;   (doom-themes-org-config)
;;   (doom-org-custom-fontification)
;;   )

;;outshine
(use-package outshine
  :ensure t
  )

;;auctex
(use-package auctex
  :ensure t
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
  :ensure t
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
  :ensure t
  :init
  :config
  (turn-on-pbcopy)
  )
  
;;web-mode
;;========================================================================
(use-package web-mode
  :ensure t
  :bind
  ("C-c s" . hs-show-block)
  ("C-c S" . hs-show-all)
  ("C-c h" . hs-hide-block)
  ("C-c H" . hs-hide-all)
  
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.js[x]?\\'" . web-mode)) ;; Use web-mode for .js(x) files
  
  (setq web-mode-content-types-alist                        
        '(("jsx" . "\\.js[x]?\\'")))                          ;; Tell web-mode that .js(x) files are jsx

  ;; Set indent level to 2
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  
  ;;(set-face-attribute 'web-mode-html-tag-face nil :foreground "#990000") ;;set tags to dark red
  (set-face-attribute 'web-mode-html-tag-face nil :foreground "cyan") ;;set tags to dark blue
  (set-face-background 'web-mode-current-element-highlight-face "#999999") ;;set highlight color to white

  
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
  (defun my-flymd-browser-function (url)
    (let ((process-environment (browse-url-process-environment)))
    (apply 'start-process
           (concat "firefox " url)
           nil
           "/usr/bin/open"
           (list "-a" "firefox" url))))
  (setq flymd-browser-open-function 'my-flymd-browser-function)
  (setq flymd-close-buffer-delete-temp-files t)
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
  (setq neo-smart-open t)  ;; Always open at the current file
  
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
