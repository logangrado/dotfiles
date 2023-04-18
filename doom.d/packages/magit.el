;;; ../.dotfiles/doom.d/packages/magit.el -*- lexical-binding: t; -*-


(use-package magit
  :config
  (setq magit-diff-refine-hunk t)

  (custom-set-faces!
    `(magit-branch-local :foreground ,(nth 2 (doom-themes--colors-p 'blue)) :bold t)
    `(magit-branch-current :inherit magit-branch-local :underline t)
    `(magit-branch-remote :foreground ,(nth 2 (doom-themes--colors-p 'green)) :bold t)
    `(magit-branch-remote-head :inherit magit-branch-remote :box nil :underline t)
   )

   (map! :map magit-mode-map
         "K" #'(lambda () (interactive) (previous-line 10) (evil-scroll-line-up 10))
         "J" #'(lambda () (interactive) (next-line 10) (evil-scroll-line-down 10))
         )

  ;; Colors!
  ;; Branches and tags
  ;; (set-face-attribute 'magit-branch-local nil :inherit 'ansi-color-green :bold t)
  ;; (set-face-attribute 'magit-branch-remote nil :foreground "red" :bold t)
  ;; (set-face-attribute 'magit-branch-current nil :foreground "cyan" :bold t)
  ;; (set-face-attribute 'magit-tag nil :foreground "cyan")

  ;; ;; Section headers
  ;; (set-face-attribute 'magit-section-heading nil :foreground "yellow")
  ;; (set-face-attribute 'magit-section-highlight nil :background "black")

  ;; ;; Diff-ing
  ;; (set-face-attribute 'magit-diff-hunk-heading-highlight nil :foreground "grey70" :background "grey35")
  ;; (set-face-attribute 'magit-diff-hunk-heading nil :foreground "grey70" :background "grey25")

  ;; (set-face-attribute 'magit-diff-context nil :foreground "brightblue" :background "brightblack")
  ;; (set-face-attribute 'magit-diff-context-highlight nil :foreground "grey70" :background "grey20")

  ;; (set-face-attribute 'magit-diff-removed t :foreground "red" :background "brightblack" :inherit 'magit-diff-context)
  ;; (set-face-attribute 'magit-diff-removed-highlight t :foreground "red" :background "grey20" :inherit 'magit-diff-context-highlight)

  ;; (set-face-attribute 'magit-diff-added nil :foreground "green" :background "brightblack" :inherit 'magit-diff-context)
  ;; (set-face-attribute 'magit-diff-added-highlight nil :foreground "green" :background "grey20" :inherit 'magit-diff-context-highlight)


  ;; ;; (set-face-attribute 'diff-refine-added nil :foreground "grey70" :background "color-29" :inverse-video nil)
  ;; ;; (set-face-attribute 'diff-refine-removed nil :foreground "grey70" :background "color-88" :inverse-video nil)

  ;; ;; (set-face-attribute 'diff-refine-added nil :foreground "grey10" :background "color-23" :inverse-video nil)
  ;; ;; (set-face-attribute 'diff-refine-removed nil :foreground "grey5" :background "color-52" :inverse-video nil)


  ;; ;; Display the status buffer in full-frame mode
  ;; (setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)
  )
