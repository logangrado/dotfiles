;;; ../.dotfiles/doom.d/packages/ibuffer-projectile.el -*- lexical-binding: t; -*-

(use-package!  ibuffer-projectile
  :init
  ;; Group by open persp/worktree (see persp-worktree.el) instead of
  ;; ibuffer-projectile's own per-buffer projectile-root detection: each
  ;; persp is now scoped to one repo+worktree, so grouping by persp
  ;; membership already gives worktree-aware groups for free.
  (defun lg/ibuffer-set-persp-filter-groups ()
    "Set `ibuffer-filter-groups' to one group per open persp/worktree.
Buffers not owned by any real persp fall into a catch-all \"Other\" group."
    (setq ibuffer-filter-groups
          (append
           (cl-loop for name in (+workspace-list-names)
                    unless (string= name persp-nil-name)
                    collect (let ((bufs (persp-buffers (persp-get-by-name name))))
                              (cons name `((predicate . (memq buf ',bufs))))))
           (list (cons "Other" '((predicate . t)))))))
  (add-hook 'ibuffer-hook
            (lambda ()
              (unless ibuffer-filter-groups
                (lg/ibuffer-set-persp-filter-groups))))
  :config
  (setq ibuffer-default-sorting-mode 'alphabetic)

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
