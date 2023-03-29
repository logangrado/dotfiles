;;; ../.dotfiles/doom.d/packages/ibuffer-projectile.el -*- lexical-binding: t; -*-

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
