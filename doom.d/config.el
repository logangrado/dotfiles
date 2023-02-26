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
    (setq doom-font (:size 12)))
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

(after! ibuffer
  ;; Don't show magit buffers in ibuffer
  (add-to-list 'ibuffer-never-show-predicates "magit*")
  )

(use-package!  ibuffer-projectile
  :init
  ;; Order buffers alphabetically within groups
  (add-hook 'ibuffer-hook
    (lambda ()
      (ibuffer-projectile-set-filter-groups)
      (unless (eq ibuffer-sorting-mode 'alphabetic)
        (ibuffer-do-sort-by-alphabetic))))
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

;; EXTERNAL PACAKGE CONFIG
;;=================================================================
;; Always display workspace tab bar on bottom
(after! persp-mode
  (defun display-workspaces-in-minibuffer ()
    (with-current-buffer " *Minibuf-0*"
      (erase-buffer)
      (insert (+workspace--tabline))))
  (run-with-idle-timer 1 t #'display-workspaces-in-minibuffer)
  (+workspace/display))

;; Auto-dim other buffers
(use-package! auto-dim-other-buffers
  :init
  (auto-dim-other-buffers-mode))

(use-package! centaur-tabs
  ;;:defer 5

  :bind (:map evil-normal-state-map
         ("M-]" . centaur-tabs-forward)
         ("M-[" . centaur-tabs-backward))

  :hook
  ;; Disalbe tabs in vterm
  ((vterm-mode vterm-toggle--mode) . centaur-tabs-local-mode)

  :config
  (centaur-tabs-mode t)
  (setq centaur-tabs-height 32
        centaur-tabs-style "bar"
        centaur-tabs-set-icons nil
        centaur-tabs-set-modified-marker t
        centaur-tabs-set-bar 'ler  ;;'under
        centaur-tabs-set-close-button nil
        )
  (centaur-tabs-group-by-projectile-project)

  ;; ;; These seem to help keep vterm tabs out of tabbar group?
  ;; (defun centaur-tabs-buffer-groups ()
  ;;   (list
  ;;    (cond
  ;;     ((string-match "vterm" (format "%s" (buffer-name))) "Emacs")
  ;;     (t (centaur-tabs-get-group-name (current-buffer))))))
  ;;

  ;; (defun my-show-only-vterm ()
  ;;   (when (bound-and-true-p centaur-tabs-mode)
  ;;         (if (string-match "vterm" (format "%s" (buffer-name)))
  ;;             (centaur-tabs-local-mode)
  ;;           (centaur-tabs-local-mode 0))))

  ;; ;; (add-hook! 'window-configuration-change-hook 'my-show-only-vterm)
  ;; (add-hook! 'buffer-list-update-hook 'my-show-only-vterm)
  )

(use-package!  origami
  :hook python-mode
  :init
  (origami-mode)
  :config
  (map! :map evil-normal-state-map
        :prefix ("z" . "origami")
        "j" #'origami-forward-fold  ;;origami-next-fold
        "k" #'origami-previous-fold  ;;origami-previous-fold
        "o" #'origami-open-node-recursively
        "O" #'origami-open-all-nodeso
        "c" #'origami-close-node-recursively
        "C" #'origami-close-all-nodes
        "t" #'origami-recursively-toggle-node
        "T" #'origami-toggle-all-nodes)
 )

(use-package dockerfile-mode
  :init
  (add-to-list 'auto-mode-alist '("Dockerfile.*\\'" . dockerfile-mode))
 )
