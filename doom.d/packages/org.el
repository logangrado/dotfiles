;;; ../.Dotfiles/doom.d/packages/org.el -*- lexical-binding: t; -*-

(after! org
  :hook
  (org-mode . (lambda () (setq line-spacing 0.25)))
  (org-mode . auto-save-mode)
  ;; (org-mode . mixed-pitch-mode)
  :init
  (setq org-directory "~/org/")
  (add-hook 'auto-save-hook 'org-save-all-org-buffers)`

  :config
  (setq-default org-startup-indented t
                org-pretty-entities t ;; Ensure we render equations and such
                org-use-sub-superscripts "{}" ;; Force using {} for sup/super scripts
                org-hide-emphasis-markers t ;; Auto-hide emphasis markers
                org-startup-with-inline-images t
                org-image-actual-width '(300)
                )

  ;; Use bullets for list markers (auto-sub for '-' character)
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Ensure these faces are fixed-pitch in variable-potch mode
  (custom-set-faces
   '(org-block ((t (:inherit fixed-pitch))))
   '(org-table ((t (:inherit fixed-pitch))))
   '(org-code ((t (:inherit fixed-pitch))))
   '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
   '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-verbatim ((t (:inherit (shadow fixed-pitch)))))
   '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-property-value ((t (:inherit fixed-pitch))) t)
   '(org-document-info ((t (:inherit fixed-pitch)))))

  ;; TODO:
  ;; Look into better agenda formating
  (setq org-agenda-view-columns-initially t)
  (setq org-overriding-columns-format "%TODO %3PRIORITY %ALLTAGS %ITEM")

  ;; Investigate setting org-agenda-prefix-format
  ;; Look into org-super-agenda
  ;; Promising example: https://github.com/alphapapa/org-super-agenda/blob/master/examples.org#sebastian-schulze

  ;; ORG AGENDA PREFIX FORMAT

  ;; Declare new faces for todo states
  ;; (with-no-warnings
  ;;   (custom-declare-face '+org-todo-active  '((t (:inherit (bold font-lock-constant-face org-todo)))) "")
  ;;   (custom-declare-face '+org-todo-project '((t (:inherit (bold font-lock-doc-face org-todo)))) "")
  ;;   (custom-declare-face '+org-todo-onhold  '((t (:inherit (bold warning org-todo)))) "")
  ;;   (custom-declare-face '+org-todo-cancel  '((t (:inherit (bold error org-todo)))) ""))



  ;; Declare faces
  (custom-set-faces!
    `(org-level-1 :foreground ,(nth 2 (doom-themes--colors-p 'fg)) :weight bold :height 1.3)
    `(org-level-2 :foreground ,(nth 2 (doom-themes--colors-p 'fg)) :weight bold :height 1.2)
    `(org-level-3 :foreground ,(nth 2 (doom-themes--colors-p 'fg)) :weight bold :height 1.1)
    `(org-level-4 :foreground ,(nth 2 (doom-themes--colors-p 'fg)) :weight bold :height 1.05)
    `(org-level-5 :foreground ,(nth 2 (doom-themes--colors-p 'fg)) :weight bold :height 1.05)
    `(org-level-6 :foreground ,(nth 2 (doom-themes--colors-p 'fg)) :weight bold :height 1.05)
    `(org-level-7 :foreground ,(nth 2 (doom-themes--colors-p 'fg)) :weight bold :height 1.05)
    `(org-level-8 :foreground ,(nth 2 (doom-themes--colors-p 'fg)) :weight bold :height 1.05)
    ;;     `(org-tag ((t (:foreground "red" :weight normal :height 1.0))))
    ;;     `(org-agenda-date-today ((t (:inherit org-agenda-date :weight bold :height 1.0))))
    ;;     `(org-agenda-date ((t (:inherit org-agenda-date :weight normal :height 1.0))))
    ;;     `(org-agenda-date-weekend ((t (:inherit org-agenda-date :weight normal :height 1.0))))j
    ;;     `(org-agenda-structure ((t (:inherit default :weight normal :height 1.0))))
    ;;     `(org-agenda-tags ((t (:inherit org-tag :weight normal :height 1.0))))
    )

  ;; Declare todo faces
  (custom-set-faces!
    `(org-todo :foreground ,(nth 2 (doom-themes--colors-p 'red)) :weight bold)
    `(+org-todo-active :foreground ,(nth 2 (doom-themes--colors-p 'cyan)) :weight bold)
    `(+org-todo-project :foreground ,(nth 2 (doom-themes--colors-p 'blue)) :weight bold)
    `(+org-todo-onhold :foreground ,(nth 2 (doom-themes--colors-p 'magenta)) :weight bold)
    `(+org-todo-cancel :foreground ,(nth 2 (doom-themes--colors-p 'red)) :weight bold)
    )

  ;; Fix face for org todo column view
  (custom-set-faces! '(org-column :background nil))

  (setq! org-todo-keywords
         '((sequence
            "TODO(t)"  ; A task that needs doing & is ready to do
            "PROG(p)"  ; A task that is in progress
            "DELG(e)"  ; This task has been delegated, may require followup
            "HOLD(h)"  ; This task is paused/on hold because of me
            ;; "PROJ(p)"  ; A project, which usually contains other tasks
            ;; "LOOP(r)"  ; A recurring task
            ;; "STRT"     ; Legacy in progress, delete in future
            "WAIT(w)"  ; Something external is holding up this task
            "FLUP(f)"
            "REVW(r)"  ; In review
            ;; "IDEA(i)"  ; An unconfirmed and unapproved task or notjion
            "|"
            "DUPE"     ; Duplicate
            "DONE(d)"  ; Task successfully completed
            "KILL(k)") ; Task was cancelled, aborted or is no longer applicable
           (sequence
            "|"
            "OKAY(o)"
            "YES(y)"
            "NO(n)"))
         )

  (setq! org-todo-keyword-faces
         '(("FLUP"  . +org-todo-onhold)
           ("PROG" . +org-todo-active)
           ("DELG" . +org-todo-active)
           ("[?]"  . +org-todo-onhold)
           ("WAIT" . +org-todo-onhold)
           ("REVW" . +org-todo-onhold)
           ("HOLD" . +org-todo-onhold)
           ("REVW" . +org-todo-onhold)
           ;; Cancel
           ("NO"   . +org-todo-cancel)
           ("KILL" . +org-todo-cancel)

           ))

  ;; ;; Adjust the display of priorities if desired
  ;; (setq! org-priority-faces '((?0 . error)
  ;;                             (?1 . warning)
  ;;                             (?2 . success)
  ;;                             (?3 . default)
  ;;                             (?4 . default)
  ;;                             (?5 . default)
  ;;                             (?6 . default)
  ;;                             (?7 . default)
  ;;                             (?8 . default)
  ;;                             (?9 . default)))

  (setq! org-priority-highest 0
         org-priority-default 2 ;; This displays as "^E" in agenda view, but I can't figure out how to fix it
         ;; We can set it to ?2, and it will display as "2", but it will be at the bottom of the list (not good!)
         org-priority-lowest 4)


  ;; When updating a todo state in agenda, ensure we retain the complete original state of the todo
  ;;=====================
  (defun my/org-agenda-todo-around (orig-fun &rest args)
    "Around advice to preserve agenda view state while running `org-agenda-todo`."
    (message "BEFORE AROUND")
    (let ((buffer (current-buffer))
          (point (point))
          (window-start (window-start)))
      (apply orig-fun args)
      (with-current-buffer buffer
        (goto-char point)
        (set-window-start (selected-window) window-start))
      (org-agenda-redo)
      )
    (message "AFTER AROUND")
    )

  (advice-add 'org-agenda-todo :around #'my/org-agenda-todo-around)
  (advice-add 'org-agenda-priority :around #'my/org-agenda-todo-around)
  ;;=====================

  (defun org-columns-priority (&optional _arg)
    "Change the PRIORITY state during column view."
    (interactive "P")
    (org-columns-edit-value "PRIORITY"))

  (defun org-todo-list-priority ()
    "Return the custom agenda command configuration for priority-based view."
    (append
     (cl-loop for priority in '(0 1 2 3 4)
              collect
              `(tags-todo ,(format "+PRIORITY=\"%d\"" priority)
                ((org-agenda-overriding-header ,(format "Priority %d" priority))
                 (org-agenda-sorting-strategy '(priority-down))
                 (org-agenda-view-columns-initially t))))
     ;; Add section for items with priority unset
     '((tags-todo "-PRIORITY=\"0\"-PRIORITY=\"1\"-PRIORITY=\"2\"-PRIORITY=\"3\"-PRIORITY=\"4\""
        ((org-agenda-overriding-header "Priority Unset")
         (org-agenda-sorting-strategy '(priority-down))
         (org-agenda-view-columns-initially t))))))

  (setq org-agenda-custom-commands
        `(("c" "Agenda by priority" ,(org-todo-list-priority))))
  )

(use-package! org-fancy-priorities
  :hook (org-mode . org-fancy-priorities-mode)
  :config
  (setq! org-priority-highest 0
         org-priority-default 2 ;; This displays as "^E" in agenda view, but I can't figure out how to fix it
         ;; We can set it to ?2, and it will display as "2", but it will be at the bottom of the list (not good!)
         org-priority-lowest 4)
  (setq! org-fancy-priorities-list '(
                                     (?0 . "P0")
                                     (?1 . "P1")
                                     (?2 . "P2")
                                     (?3 . "P3")
                                     (?4 . "P4"))

         org-priority-faces '((?0 :foreground "DarkRed" :background "LightPink")
                              (?1 :foreground "DarkOrange4" :background "LightGoldenrod")
                              (?2 :foreground "gray20" :background "gray")
                              (?3 :foreground "gray20" :background "gray")
                              (?4 :foreground "gray20" :background "gray")))
  )

;; When editing sections with emphasis, show the hideen emphasis!
(use-package! org-appear
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autolinks t
        org-appear-autosubmarkers t
        org-appear-autokeywords t
        org-appear-inside-latex t))

;; Use pretty org-bullets
;; (use-package! org-bullets
;;   :config
;;   (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; Mixed pitch mode!
(use-package! mixed-pitch
  )

;;OLD CODE
;;================================================================================================

;; (use-package! org
;;   :bind (:map org-mode-map
;;               ("S-<return>" . org-insert-item)
;;               ("M-S-<return>" . org-insert-heading)
;;               ("C-M-<return>" . org-insert-subheading)

;;               )

;;   :config
;;   ;; (global-unset-key (kbd "M-s-<return>"))
;;   ;; (global-set-key (kbd "M-s-<return>") 'org-insert-subheading)
;;   (custom-set-faces!
;;     `(org-todo :foreground ,(nth 2 (doom-themes--colors-p 'red)) :box t)
;;     )


;;   (with-no-warnings
;;     (custom-declare-face '+org-todo-active  '((t (:inherit (bold font-lock-constant-face org-todo)))) "")
;;     (custom-declare-face '+org-todo-project '((t (:inherit (bold font-lock-doc-face org-todo)))) "")
;;     (custom-declare-face '+org-todo-onhold  '((t (:inherit (bold warning org-todo)))) "")
;;     (custom-declare-face '+org-todo-cancel  '((t (:inherit (bold error org-todo)))) ""))

;;   (custom-set-faces!
;;     `(+org-todo-active :foreground ,(nth 2 (doom-themes--colors-p 'yellow)))
;;     `(+org-todo-project :foreground ,(nth 2 (doom-themes--colors-p 'blue)))
;;     `(+org-todo-onhold :foreground ,(nth 2 (doom-themes--colors-p 'magenta)))
;;     `(+org-todo-cancel :foreground ,(nth 2 (doom-themes--colors-p 'red)))
;;     )

;;   (setq org-todo-keywords
;;         '((sequence
;;            "TODO(t)"  ; A task that needs doing & is ready to do
;;            "PROJ(p)"  ; A project, which usually contains other tasks
;;            "LOOP(r)"  ; A recurring task
;;            "STRT(s)"  ; A task that is in progress
;;            "WAIT(w)"  ; Something external is holding up this task
;;            "HOLD(h)"  ; This task is paused/on hold because of me
;;            "IDEA(i)"  ; An unconfirmed and unapproved task or notion
;;            "|"
;;            "DONE(d)"  ; Task successfully completed
;;            "KILL(k)") ; Task was cancelled, aborted or is no longer applicable
;;           (sequence
;;            "[ ](T)"   ; A task that needs doing
;;            "[-](S)"   ; Task is in progress
;;            "[?](W)"   ; Task is being held up or paused
;;            "|"
;;            "[X](D)")  ; Task was completed
;;           (sequence
;;            "|"
;;            "OKAY(o)"
;;            "YES(y)"
;;            "NO(n)"))
;;         org-todo-keyword-faces
;;         '(("[-]"  . +org-todo-active)
;;           ("STRT" . +org-todo-active)
;;           ("[?]"  . +org-todo-onhold)
;;           ("WAIT" . +org-todo-onhold)
;;           ("HOLD" . +org-todo-onhold)
;;           ("PROJ" . +org-todo-project)
;;           ("NO"   . +org-todo-cancel)
;;           ("KILL" . +org-todo-cancel)))

;;   (setq org-tags-column -80)

;;   ;; Make ORG look better
;;   (setq org-hide-emphasis-markers t) ;;Hide emphasis markers (bold, italic, etc))
;;   )

;; VERY OLD CONFIG!!
;; ==================================================================================
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
