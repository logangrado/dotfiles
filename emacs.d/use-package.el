;; Inbox

;; Python Related
;;================================================================
(use-package epc
  :ensure t
  )

(use-package company
  ;; Complete Anything (company). Autocomplete popup for code.
  ;; Distinct from ivy, which auto-completes buffer commands
  :ensure t
  :bind
  (:map company-active-map ("RET" . newline))  ;; Make RET enter a newline
  (:map company-active-map ("TAB" . company-complete-selection)) ;; Use TAB for completion (instead of complete common)  
  )


(use-package elpy
  :ensure t
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable)
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "-i --simple-prompt")
  ;; Use the currently-active python for elpy. As long as poetry is active when launching emacs, this works perfectly
  (setq elpy-rpc-virtualenv-path 'current)
  ;; fix "elpy-rpc--default-error-callback: peculiar error: "exited abnormally with code 1"" issue
  (add-to-list 'process-coding-system-alist '("python" . (utf-8 . utf-8)))
  )

(use-package ein
  :ensure t
  :bind
  ;; ECS <arrow> is really M-<arrow>
  ("ESC <up>" . ein:worksheet-goto-prev-input-km)
  ("ESC <down>" . ein:worksheet-goto-next-input-km)

  )

;; DELETE AFTER: 2021-01
;; Elpy doesn't seem to need this
;; (use-package poetry
;;   ;; ANOTHER POSSABILITY: with-evenv
;;   ;; This looks like it might be able to deted poetry env without haveing to do it for each buffer...
;;   ;; However, with the 'projectile mode, it seems to work well
  
;;   ;; Allows automatically finding/using poetry envs
;;   :ensure t
;;   :hook (python-mode . poetry-tracking-mode)
;;   :config
;;   ;;(setq poetry-tracking-strategy `projectile)
;;   (setq poetry-tracking-strategy `switch-buffer)
;;   ;; Other options: projectile (but only works if you switch buffers using projectile-switch command
;;   )

;; DELETE AFTER: 2021-01
;; (use-package jedi
;;   ;; Python auto complete package
;;   ;; relies on auto-compoete and epc
;;   ;; also uses poetry mode to find/use the correct poetry environment
;;   :ensure t
;;   :hook (python-mode . jedi:setup)
;;   ;;:after (poetry auto-complete epc)
;;   :init
;;   :config
;;   (add-to-list 'ac-sources 'ac-source-jedi-direct)
;;   (setq jedi:complete-on-dot t)
;;   (setq jedi:get-in-function-call-delay 500)
;;   )

(use-package python-black
  :ensure t
  :after python
  :hook (python-mode . python-black-on-save-mode)
  :config
  (setq python-black-extra-args (list "-l 120"))
  )

;; General
;;================================================================

(use-package ivy
  ;; General smart auto-complete, all over emacs
  :ensure t
  :init
  (ivy-mode)
  ;; this sets the ivy autocomplete to only use two faces for matches
  ;; face-1 is for space between matching words
  ;; face-2 is for the actual matches
  ;; ALSO, it seems that ivy-current-match somehow inherits from face-2 in some weird way...
  (setq ivy-minibuffer-faces '(ivy-minibuffer-match-face-1
                               ivy-minibuffer-match-face-2))
  
  (set-face-attribute 'ivy-current-match nil :foreground "black" :background "#65a7e2")
  (set-face-attribute 'ivy-minibuffer-match-face-1 nil :foreground "brightblue" :background "brightblack")
  (set-face-attribute 'ivy-minibuffer-match-face-2 nil :foreground "#6387ac" :background "brightblack" :inverse-video t :inherit nil)
  )

;; DELETE AFTER: 2021-01
;; I believe this was completely supplanted in favor of ivy
;; (use-package auto-complete
;;   :ensure t
;;   :init
;;   (ac-config-default)
;;   )

(use-package projectile
  :ensure t
  ;;:pin melpa-stable
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))

;; DELETE AFTER: 2021-01
;; I THINK I've removed this in favor of ibuffer-prjectile, but who knows. delete later?
;; (use-package ibuffer-vc
;;   ;; Group ibuffers by VC project
;;   :ensure t
;;   :init
;;   :config
;;    ;; (add-hook 'ibuffer-hook
;;    ;;           (lambda ()
;;    ;;             (ibuffer-vc-set-filter-groups-by-vc-root)
;;    ;;             (unless (eq ibuffer-sorting-mode 'alphabetic)
;;    ;;               (ibuffer-do-sort-by-alphabetic))))
;;    (setq vc-status ibuffer-formats)
;;    )

(use-package ibuffer-projectile
  :ensure t
  :init
  (add-hook 'ibuffer-mode-hook
            (lambda ()
              (ibuffer-projectile-set-filter-groups)
              (ibuffer-do-sort-by-filename/process)))
      ;; (unless (eq ibuffer-sorting-mode 'alphabetic)
      ;;   (ibuffer-do-sort-by-alphabetic))))
  :config
  ;; define size-h column (human readable)
  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (cond
     ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
     ((> (buffer-size) 100000) (format "%7.0fk" (/ (buffer-size) 1000.0)))
     ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
     (t (format "%8dB" (buffer-size)))))
  
  (setq ibuffer-formats
      '((mark modified read-only " "
              (name 25 25 :left :elide)
              " "
              (size-h 9 -1 :right)       ;; use human readable size
              " "
              (mode 16 16 :left :elide)
              " "
              project-relative-file)))   ;; Display filenames relative to project root
  )

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

  (set-face-attribute 'neo-root-dir-face nil :foreground "lightblue" :bold t)
  (set-face-attribute 'neo-dir-link-face nil :foreground "blue" :bold nil)
  )

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

(use-package centaur-tabs
  :ensure t
  :init
  (centaur-tabs-mode t)
  (centaur-tabs-local-mode)
  
  :config
  (setq centaur-tabs-style "bar"
        centaur-tabs-height 32
        ;; centaur-tabs-set-icons t
        centaur-tabs-set-modified-marker t
        centaur-tabs-show-navigation-buttons nil ; No navigation buttons
        centaur-tabs-set-close-button nil        ; No close button
        centaur-tabs-set-bar 'under
        x-underline-at-descent-line t)
  (centaur-tabs-headline-match)
  (centaur-tabs-group-by-projectile-project) ;; Group tabs by projectile
  (setq centaur-tabs-cycle-scope 'tabs) ;; cycle tabs only through current group
  (set-face-attribute 'centaur-tabs-unselected nil :foreground "brightblue")

  ;; Fixed tab width
  ;'(setq centaur-tabs-label-fixed-length 20)

  ;; AUTOMATIC BUFFER REORDERING
  ;;============================
  ;; Enable buffer reordering when selecting tabs. Automatically moves the newly selected
  ;; next to the previous tab. Only work with ivy-switch-buffer (not ibuffer)
  (centaur-tabs-enable-buffer-reordering)
  (setq centaur-tabs-adjust-buffer-order t)  ;; After switch, move previous buffer next to current buffer (but on the same side)
  ;; Custom buffer switchign funtion. Only trigger for ivy/projectile switch
  ;; Otherwise, if we switch buffers by focusing on a different emacs window in tmux, buffers switch on us
  ;; NOTE: Note sure how to enable for ibuffer switches too
  (defun custom-adjust-buffer-function ()
    (if (or (string-prefix-p "ivy" (format "%s" this-command))
	    (string-prefix-p "projectile" (format "%s" this-command))
            (centaur-tabs-adjust-buffer-order)
            )
        )
    )
  (setq centaur-tabs-adjust-buffer-order-function 'custom-adjust-buffer-function)
  
  :bind
  ("C-c C-<left>" . centaur-tabs-backward)
  ("C-c C-<right>" . centaur-tabs-forward)
  ("C-c C-<up>" . centaur-tabs-backward-group)
  ("C-c C-<down>" . centaur-tabs-forward-group)

  ("C-c ESC <right>" . centaur-tabs-move-current-tab-to-right)
  ("C-c ESC <left>" . centaur-tabs-move-current-tab-to-left)
  ("C-c k" . centaur-tabs--kill-this-buffer-dont-ask)
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

;; This appears to be responsible for confusing poetry.el/jedi. Disabling to see if the warnings
;; finally go away...
;; (use-package exec-path-from-shell   ;; For GUI emacs
;;    :ensure t
;;    :config
;;    (setq exec-path-from-shell-check-startup-files nil)  ; suppress warning message
;;    (exec-path-from-shell-initialize)                    ; use shell path
;;    )

(use-package magit
  :ensure t
  :init
  (bind-key "C-x g" 'magit-status)
  :config
  (setq magit-diff-refine-hunk t)

  ;; Colors!
  ;; Branches and tags
  (set-face-attribute 'magit-branch-local nil :foreground "blue" :bold t)
  (set-face-attribute 'magit-branch-remote nil :foreground "red" :bold t)
  (set-face-attribute 'magit-branch-current nil :foreground "cyan" :bold t)
  (set-face-attribute 'magit-tag nil :foreground "cyan")

  ;; Section headers
  (set-face-attribute 'magit-section-heading nil :foreground "yellow")
  (set-face-attribute 'magit-section-highlight nil :background "black")

  ;; Diff-ing
  (set-face-attribute 'magit-diff-hunk-heading-highlight nil :foreground "grey70" :background "grey35")
  (set-face-attribute 'magit-diff-hunk-heading nil :foreground "grey70" :background "grey25")

  (set-face-attribute 'magit-diff-context nil :foreground "brightblue" :background "brightblack")
  (set-face-attribute 'magit-diff-context-highlight nil :foreground "grey70" :background "grey20")

  (set-face-attribute 'magit-diff-removed t :foreground "red" :background "brightblack" :inherit 'magit-diff-context)
  (set-face-attribute 'magit-diff-removed-highlight t :foreground "red" :background "grey20" :inherit 'magit-diff-context-highlight)
  
  (set-face-attribute 'magit-diff-added nil :foreground "green" :background "brightblack" :inherit 'magit-diff-context)
  (set-face-attribute 'magit-diff-added-highlight nil :foreground "green" :background "grey20" :inherit 'magit-diff-context-highlight)


  ;; (set-face-attribute 'diff-refine-added nil :foreground "grey70" :background "color-29" :inverse-video nil)
  ;; (set-face-attribute 'diff-refine-removed nil :foreground "grey70" :background "color-88" :inverse-video nil)

  ;; (set-face-attribute 'diff-refine-added nil :foreground "grey10" :background "color-23" :inverse-video nil)
  ;; (set-face-attribute 'diff-refine-removed nil :foreground "grey5" :background "color-52" :inverse-video nil)

  
  ;; Display the status buffer in full-frame mode
  (setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)
  )

(use-package outline-magic
  :ensure t
  :init
  :config
  )
  
(use-package adaptive-wrap
  :ensure t
  :config
  (define-globalized-minor-mode global-visual-line-mode visual-line-mode
    (lambda () (visual-line-mode 1)))
  (global-visual-line-mode t)
  (add-hook 'visual-line-mode-hook 'adaptive-wrap-prefix-mode)
)

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

(use-package pretty-mode
  :ensure t
  :init
  (add-hook 'python-mode-hook 'turn-on-pretty-mode)
  :config
  (pretty-activate-groups '(:greek)) ;;:sub-and-superscripts))
  (pretty-deactivate-patterns '(:== :return :def)) ;;disable eqauls
  )

(use-package which-key
  :ensure t
  :init
  (which-key-mode)
  )

(use-package real-auto-save
  :ensure t
  :init
  (setq real-auto-save-interval 10) ;; in seconds
  :config
  )

(use-package outshine
  :ensure t
  )

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

(use-package pbcopy
  :ensure t
  :init
  :config
  (turn-on-pbcopy)
  )
  
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

;; Major Modes
;;================================================================

(use-package org
  :ensure t
  :init
  (setq org-latex-create-formula-image-program 'dvisvgm)

  (setq org-agenda-files (list "~/org/todo/"))
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

  :bind
  ("S-RET" . org-insert-heading-respect-content)
  
  :config
  ;;=============================================================
  (add-hook 'org-mode-hook 'flyspell-mode)
  (set-face-attribute 'org-todo nil :background "Red")
  (setq org-startup-indented t)
  (setq org-hierarchical-todo-statistics nil)         ;; Nil means children count, not just top leve
  (setq org-checkbox-hierarchical-statistics nil)

  (setq org-M-RET-may-split-line nil)                 ;; Don't split lines when using M-RET
  
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

(use-package scad-mode
  :ensure t
  :init
  :config
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

(use-package format-all
  ;; Generalized formatting mode.
  ;; Attempts to call the correct code formatter for just about any language (language-specific
  ;; formatter must be installed). Also provides format-all-mode to automatically call
  :ensure t
  ;;:hook
  ;;(jsonnet-mode . format-all-mode)
  ;;(protobuf-mode . format-all-mode)
  :bind
  ("C-c C-f" . format-all-buffer)
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

(use-package dockerfile-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("Dockerfile.*\\'" . dockerfile-mode))
  )
  
(use-package yaml-mode
  :ensure t
  )
  
(use-package csv-mode
  :ensure t
  :config
  ;;(csv-align-fields-mode)
  )


;;==============================
;; SWTICHED TO Centaur Tabs
;; Centuar tabs has native support for projectile grouping
;; Leaving this config here because it was a lot of work
;;==============================
;; (use-package tabbar
;;   :ensure t
;;   :after projectile
;;   :init
;;   (tabbar-mode)
;;   :config
 
;;   (set-face-attribute 'tabbar-default nil :background "brightcyan" :foreground "brightcyan")
;;   (set-face-attribute 'tabbar-unselected nil
;;                       :background "brightblack"
;;                       :foreground "brightcyan")

;;   (set-face-attribute 'tabbar-selected nil
;;                       :background "brightcyan"
;;                       :foreground "brightblack")

;;   ;; Add space to make less crowded
;;   (defadvice tabbar-buffer-tab-label (after fixup_tab_label_space_and_flag activate)
;;     (setq ad-return-value
;;           (if (and (buffer-modified-p (tabbar-tab-value tab))
;;                    (buffer-file-name (tabbar-tab-value tab)))
;;               (concat " + " (concat ad-return-value " "))
;;             (concat " " (concat ad-return-value " ")))))

;; ;;   (defun tabbar-buffer-groups ()
;; ;;     "Return the list of group names the current buffer belongs to.
;; ;; Return a list of one element based on major mode."
;; ;;     (list
;; ;;      (cond
;; ;;       ((or (get-buffer-process (current-buffer))
;; ;;            ;; Check if the major mode derives from `comint-mode' or
;; ;;            ;; `compilation-mode'.
;; ;;            (tabbar-buffer-mode-derived-p
;; ;;             major-mode '(comint-mode compilation-mode)))
;; ;;        "Process"
;; ;;        )
;; ;;       ((member (buffer-name)
;; ;;                '("*scratch*" "*Messages*" "*dashboard*" "TAGS"))
;; ;;        "Common"
;; ;;        )
;; ;;       ((eq major-mode 'dired-mode)
;; ;;        "Dired"
;; ;;        )
;; ;;       ((memq major-mode
;; ;;              '(help-mode apropos-mode Info-mode Man-mode))
;; ;;        "Help"
;; ;;        )
;; ;;       ((memq major-mode
;; ;;              '(rmail-mode
;; ;;                rmail-edit-mode vm-summary-mode vm-mode mail-mode
;; ;;                mh-letter-mode mh-show-mode mh-folder-mode
;; ;;                gnus-summary-mode message-mode gnus-group-mode
;; ;;                gnus-article-mode score-mode gnus-browse-killed-mode))
;; ;;        "Mail"
;; ;;        )
;; ;;     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;     ;;; Group tabs by projectile projects
;; ;;       ((memq (current-buffer)
;; ;;              (condition-case nil
;; ;;                  (projectile-buffers-with-file-or-process (projectile-project-buffers))
;; ;;                (error nil)))
;; ;;        (projectile-project-name)
;; ;;        )
;; ;;     ;;; end of hacking
;; ;;     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
;; ;;       (t
;; ;;        ;; Return `mode-name' if not blank, `major-mode' otherwise.
;; ;;        (if (and (stringp mode-name)
;; ;;                 ;; Take care of preserving the match-data because this
;; ;;                 ;; function is called when updating the header line.
;; ;;                 (save-match-data (string-match "[^ ]" mode-name)))
;; ;;            mode-name
;; ;;          (symbol-name major-mode))
;; ;;        ))))

;;   ;; (defun tabbar-buffer-groups ()
;;   ;;   "Return the list of group names the current buffer belongs to.
;;   ;;   This function is a custom function for tabbar-mode's tabbar-buffer-groups.
;;   ;;   This function group all buffers into 3 groups:
;;   ;;   Those Dired, those user buffer, and those emacs buffer.
;;   ;;   Emacs buffer are those starting with “*”."
;;   ;;   (list
;;   ;;    (cond
;;   ;;     ((string-equal "*" (substring (buffer-name) 0 1))
;;   ;;      "Emacs Buffer"
;;   ;;      )
;;   ;;     ((eq major-mode 'org-mode)
;;   ;;      "Org"
;;   ;;      )
;;   ;;     ((eq major-mode 'dired-mode)
;;   ;;      "Dired"
;;   ;;      )
;;   ;;     (t
;;   ;;      "User Buffer"
;;   ;;      )
;;   ;;     ))) 
  
;;   (defun tabbar-move-current-tab-one-place-left ()
;;     "Move current tab one place left, unless it's already the leftmost."
;;     (interactive)
;;     (let* ((bufset (tabbar-current-tabset t))
;;            (old-bufs (tabbar-tabs bufset))
;;            (first-buf (car old-bufs))
;;            (new-bufs (list)))
;;       (if (string= (buffer-name) (format "%s" (car first-buf)))
;;           old-bufs ; the current tab is the leftmost
;;         (setq not-yet-this-buf first-buf)
;;         (setq old-bufs (cdr old-bufs))
;;         (while (and
;;                 old-bufs
;;                 (not (string= (buffer-name) (format "%s" (car (car old-bufs))))))
;;           (push not-yet-this-buf new-bufs)
;;           (setq not-yet-this-buf (car old-bufs))
;;           (setq old-bufs (cdr old-bufs)))
;;         (if old-bufs ; if this is false, then the current tab's buffer name is mysteriously missing
;;             (progn
;;               (push (car old-bufs) new-bufs) ; this is the tab that was to be moved
;;               (push not-yet-this-buf new-bufs)
;;               (setq new-bufs (reverse new-bufs))
;;               (setq new-bufs (append new-bufs (cdr old-bufs))))
;;           (error "Error: current buffer's name was not found in Tabbar's buffer list."))
;;         (set bufset new-bufs)
;;         (tabbar-set-template bufset nil)
;;         (tabbar-display-update))))
  
;;   (defun tabbar-move-current-tab-one-place-right ()
;;     "Move current tab one place right, unless it's already the rightmost."
;;     (interactive)
;;     (let* ((bufset (tabbar-current-tabset t))
;;            (old-bufs (tabbar-tabs bufset))
;;            (first-buf (car old-bufs))
;;            (new-bufs (list)))
;;       (while (and
;;               old-bufs
;;               (not (string= (buffer-name) (format "%s" (car (car old-bufs))))))
;;         (push (car old-bufs) new-bufs)
;;         (setq old-bufs (cdr old-bufs)))
;;       (if old-bufs ; if this is false, then the current tab's buffer name is mysteriously missing
;;           (progn
;;             (setq the-buffer (car old-bufs))
;;             (setq old-bufs (cdr old-bufs))
;;             (if old-bufs ; if this is false, then the current tab is the rightmost
;;                 (push (car old-bufs) new-bufs))
;;             (push the-buffer new-bufs)) ; this is the tab that was to be moved
;;         (error "Error: current buffer's name was not found in Tabbar's buffer list."))
;;       (setq new-bufs (reverse new-bufs))
;;       (setq new-bufs (append new-bufs (cdr old-bufs)))
;;       (set bufset new-bufs)
;;       (tabbar-set-template bufset nil)
;;       (tabbar-display-update)))
  
;;   (setq tabbar-buffer-groups-function 'tabbar-buffer-groups)

;;   (global-set-key (kbd "ESC <left>") 'tabbar-backward-tab)
;;   (global-set-key (kbd "ESC <right>") 'tabbar-forward-tab)
;;   (global-set-key (kbd "M-<up>") 'tabbar-backward-group)
;;   (global-set-key (kbd "M-<down>") 'tabbar-forward-group)

;;   (global-set-key (kbd "C-c t <left>") 'tabbar-move-current-tab-one-place-left)
;;   (global-set-key (kbd "C-c t <right>") 'tabbar-move-current-tab-one-place-right)
  
;;   )
