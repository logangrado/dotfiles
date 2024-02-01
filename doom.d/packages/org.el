;;; ../.Dotfiles/doom.d/packages/org.el -*- lexical-binding: t; -*-

(use-package! org
  :bind (:map org-mode-map
              ("S-<return>" . org-insert-item)
              ("M-S-<return>" . org-insert-heading)
              ("C-M-<return>" . org-insert-subheading)

              )

  :config
  ;; (global-unset-key (kbd "M-s-<return>"))
  ;; (global-set-key (kbd "M-s-<return>") 'org-insert-subheading)
  (custom-set-faces!
    `(org-todo :foreground ,(nth 2 (doom-themes--colors-p 'red)) :box t)
    )

  (with-no-warnings
    (custom-declare-face '+org-todo-active  '((t (:inherit (bold font-lock-constant-face org-todo)))) "")
    (custom-declare-face '+org-todo-project '((t (:inherit (bold font-lock-doc-face org-todo)))) "")
    (custom-declare-face '+org-todo-onhold  '((t (:inherit (bold warning org-todo)))) "")
    (custom-declare-face '+org-todo-cancel  '((t (:inherit (bold error org-todo)))) ""))

  (custom-set-faces!
    `(+org-todo-active :foreground ,(nth 2 (doom-themes--colors-p 'yellow)))
    `(+org-todo-project :foreground ,(nth 2 (doom-themes--colors-p 'blue)))
    `(+org-todo-onhold :foreground ,(nth 2 (doom-themes--colors-p 'magenta)))
    `(+org-todo-cancel :foreground ,(nth 2 (doom-themes--colors-p 'red)))
    )

  (setq org-todo-keywords
        '((sequence
           "TODO(t)"  ; A task that needs doing & is ready to do
           "PROJ(p)"  ; A project, which usually contains other tasks
           "LOOP(r)"  ; A recurring task
           "STRT(s)"  ; A task that is in progress
           "WAIT(w)"  ; Something external is holding up this task
           "HOLD(h)"  ; This task is paused/on hold because of me
           "IDEA(i)"  ; An unconfirmed and unapproved task or notion
           "|"
           "DONE(d)"  ; Task successfully completed
           "KILL(k)") ; Task was cancelled, aborted or is no longer applicable
          (sequence
           "[ ](T)"   ; A task that needs doing
           "[-](S)"   ; Task is in progress
           "[?](W)"   ; Task is being held up or paused
           "|"
           "[X](D)")  ; Task was completed
          (sequence
           "|"
           "OKAY(o)"
           "YES(y)"
           "NO(n)"))
        org-todo-keyword-faces
        '(("[-]"  . +org-todo-active)
          ("STRT" . +org-todo-active)
          ("[?]"  . +org-todo-onhold)
          ("WAIT" . +org-todo-onhold)
          ("HOLD" . +org-todo-onhold)
          ("PROJ" . +org-todo-project)
          ("NO"   . +org-todo-cancel)
          ("KILL" . +org-todo-cancel)))

  (setq org-tags-column -80)

  ;; Make ORG look better
  (setq org-hide-emphasis-markers t) ;;Hide emphasis markers (bold, italic, etc))
  )
;; (use-package! org
;;   ;; :bind
;;   ;; ("s-ret" . org-insert-heading-respect-content)

;;   :init
;;   (setq org-preview-latex-default-process 'dvisvgm)

;;   (load-library "find-lisp")
;;   (setq org-agenda-files
;;    (find-lisp-find-files "~/org/" "\.org$"))

;;   (setq org-default-notes-file "~/org/todo/inbox.org")

;;   (setq org-agenda-ndays 7)
;;   (setq org-deadline-warning-days 14)
;;   (setq org-agenda-show-all-dates t)
;;   (setq org-agenda-skip-deadline-if-done t)
;;   (setq org-agenda-skip-scheduled-if-done t)
;;   (setq org-agenda-start-on-weekday nil)
;;   (setq org-reverse-note-order t)

;;   (setq org-capture-templates
;;         '(("t" "Todo" entry (file+headline "~/org/todo/inbox.org")
;;            "* TODO %?\n  %u\n  %a")))

;;   (setq org-todo-keywords
;;         '((sequence "TODO(t)" "IN PROG(p)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c)")
;;           (sequence "TODO(t)" "IN PROG(p)" "PENDING REVIEW(p)" "IN REVIEW(r)" "|" "DONE(d)" "CANCELED(c)")
;;           (sequence "DELEGATED" "|" "DONE")))

;;   ;;(global-set-key (kbd "C-c a") 'org-agenda)
;;   ;;(global-set-key (kbd "C-c r") 'org-capture)
;;   ;;(global-set-key (kbd "M-t") 'org-todo)


;;   ;; Org Archive
;;   (setq org-archive-location ".%s_archive::") ;; Hide org archive files

;;   ;; Function to archive all DONE items in file
;;   (defun org-archive-done-tasks ()
;;     (interactive)
;;     (org-map-entries 'org-archive-subtree "/DONE" 'file))
;;   (global-set-key (kbd "C-c C-x C-A") 'org-archive-done-tasks)

;;   ;; Org-mode-hooks
;;   (add-hook 'org-mode-hook 'real-auto-save-mode)
;;   (add-hook 'org-mode-hook 'auto-revert-mode)

;;   ;; Ensure org-capture pop up in full screen
;;   (add-hook 'org-capture-mode-hook 'delete-other-windows)

;;   :config
;;   ;;=============================================================
;;   (add-hook 'org-mode-hook 'flyspell-mode)
;;   ;;(set-face-attribute 'org-todo nil :background "Red")
;;   (setq org-startup-indented t)
;;   (setq org-hierarchical-todo-statistics nil)         ;; Nil means children count, not just top leve
;;   (setq org-checkbox-hierarchical-statistics nil)

;;   (setq org-M-RET-may-split-line nil)                 ;; Don't split lines when using M-RET

;;   ;; Org-column settings
;;   (setq org-agenda-overriding-columns-format "%TODO %3PRIORITY %ALLTAGS %ITEM")
;;   ;;(set-face-attribute 'org-column nil :inverse-video nil)
;;   (setq org-agenda-view-columns-initially t)

;;   ;; Org priority values
;;   (setq org-highest-priority 1)
;;   (setq org-default-priority 5)
;;   (setq org-lowest-priority 9)



;;   ;; I don't tink we need this anymore, as doom in GUI does this for us
;;   ;; ;; Checkbox colors
;;   ;; ;; Changes the color of the text behind a checkbox
;;   ;; ;;==================================================
;;   ;; ;; Define the checkbox todo text
;;   ;; (defface org-checkbox-todo-text
;;   ;;   ;;'((t (:inherit org-todo)))
;;   ;;   '((t (
;;   ;;         :inherit default
;;   ;;         :foreground "red"
;;   ;;         :weight normal)))
;;   ;;   "Face for the text part of an unchecked org-mode checkbox.")
;;   ;; ;; Add keyword for it
;;   ;; (font-lock-add-keywords
;;   ;;  'org-mode
;;   ;;  `(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?: \\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)" 1 'org-checkbox-todo-text prepend))
;;   ;;  'append)

;;   ;; ;; Define done
;;   ;; (defface org-checkbox-done-text
;;   ;;   '((t (
;;   ;;         :foreground "bright green"
;;   ;;         )))
;;   ;;   ;;'((t (:inherit org-done)))
;;   ;;   "Face for the text part of a checked org-mode checkbox.")

;;   ;; ;; Add keyworkd for it
;;   ;; (font-lock-add-keywords
;;   ;;  'org-mode
;;   ;;  `(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:X\\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)" 1 'org-checkbox-done-text prepend))
;;   ;;  'append)
;;   )
