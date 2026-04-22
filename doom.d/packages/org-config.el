;;; ../.Dotfiles/doom.d/packages/org.el -*- lexical-binding: t; -*-

(use-package! org
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

  ;; ORG AGENDA

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

  (setq! org-log-into-drawer t)  ; put state-change timestamps in :LOGBOOK: drawer

  (setq! org-priority-highest ?A
         org-priority-default  ?C
         org-priority-lowest   ?D)


  ;; When updating a todo state in agenda, ensure we retain the complete original state of the todo
  ;;=====================
  (defun lg/org-agenda-todo-around (orig-fun &rest args)
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

  (advice-add 'org-agenda-todo :around #'lg/org-agenda-todo-around)
  (advice-add 'org-agenda-priority :around #'lg/org-agenda-todo-around)
  ;;=====================

  ;; Suppress normal todo-keyword column (we render state in prefix instead)
  (setq org-agenda-todo-keyword-format "")
  ;; Remove right-margin tags (we render tags in prefix instead)
  (setq org-agenda-remove-tags t)

  ;; Sort by :ORDER: property (lower number = higher in list)
  (defun lg/org-cmp-order (a b)
    "Compare agenda entries A and B by their :ORDER: property (lower = first)."
    (let ((oa (or (org-entry-get (get-text-property 0 'org-marker a) "ORDER") "999"))
          (ob (or (org-entry-get (get-text-property 0 'org-marker b) "ORDER") "999")))
      (cond ((< (string-to-number oa) (string-to-number ob)) -1)
            ((> (string-to-number oa) (string-to-number ob)) +1))))
  (setq org-agenda-cmp-user-defined #'lg/org-cmp-order)

  ;; Column prefix helpers — called in org buffer context during agenda formatting
  (defun lg/agenda-order-str ()
    "Return :ORDER: as a right-justified 3-char string."
    (format "%3s" (or (org-entry-get (point) "ORDER") "-")))

  (defun lg/agenda-file-str ()
    "Return filename without roam timestamp prefix, truncated to 15 chars."
    (let* ((base (file-name-base (or (buffer-file-name) "")))
           (name (if (string-match "^[0-9]\\{14\\}-\\(.*\\)$" base)
                     (match-string 1 base)
                   base)))
      (format "%-15s" (truncate-string-to-width name 15 nil nil t))))

  (defun lg/agenda-state-str ()
    "Return TODO state padded to 4 chars (colors applied in finalize hook)."
    (format "%-4s" (or (org-entry-get (point) "TODO") "")))

  (defvar lg/agenda-tag-color-palette
    '("#e06c75" "#98c379" "#e5c07b" "#61afef" "#c678dd" "#56b6c2" "#d19a66")
    "Color palette for per-tag coloring in the agenda view.
Seven colors (prime) ensures better distribution when taking mod.")

  (defun lg/agenda-tag-color (tag)
    "Return a deterministic color from the palette for TAG.
Uses sum of character codes mod palette length — simple, session-stable,
and verified collision-free for the current tag set with a 7-color palette."
    (nth (mod (apply #'+ (string-to-list tag))
              (length lg/agenda-tag-color-palette))
         lg/agenda-tag-color-palette))

  (defun lg/agenda-tags-str ()
    "Return 20-char fixed-width tags string, excluding :project: (colors applied in finalize hook).
Uses non-breaking spaces (U+00A0) for padding — org-agenda strips trailing
ASCII spaces from %(function) results; NBSP survives.  The nobreak-space
display highlight is suppressed in lg/agenda-colorize."
    (let* ((tags (seq-filter
                  (lambda (tag) (not (string= tag "project")))
                  (org-get-tags nil t)))
           (plain (if tags (concat ":" (string-join tags ":") ":") ""))
           (padding (make-string (max 0 (- 20 (length plain))) ?\xA0)))
      (concat plain padding)))

  ;; Ordered TODO agenda grouped by priority using org-super-agenda
  (setq org-agenda-custom-commands
        '(("o" "Ordered TODOs"
           ((todo ""
                  ((org-super-agenda-groups
                    '((:name "Unprioritized" :not (:priority ("A" "B" "C" "D")))
                      (:name "High"          :priority "A")
                      (:name "Medium"        :priority "B")
                      (:name "Low"           :priority "C")
                      (:name "Backlog"       :priority "D")))
                   (org-agenda-sorting-strategy '(user-defined-up priority-down))
                   (org-agenda-prefix-format " %(lg/agenda-order-str)  %(lg/agenda-state-str)  %(lg/agenda-tags-str)  ")))))))
  )

;; ORDER normalization: runs on every agenda open, per priority group
(defvar lg/org-agenda-normalize-pending nil
  "Set to t after normalization; cleared on the subsequent finalize (post-redo).")

(defun lg/org-agenda-normalize-orders ()
  "Normalize :ORDER: values per priority group.
ORDER=0 items (newly captured) sort to the top of their group.
All items are then re-indexed 1, 2, 3... within each group."
  (let ((groups (make-hash-table :test 'equal)))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when-let ((m (org-get-at-bol 'org-marker)))
          (let* ((priority (or (org-entry-get m "PRIORITY") "none"))
                 (order (string-to-number (or (org-entry-get m "ORDER") "0"))))
            (puthash priority
                     (cons (cons m order) (gethash priority groups '()))
                     groups)))
        (forward-line 1)))
    (maphash
     (lambda (_priority items)
       (setq items (sort items
                         (lambda (a b)
                           (let ((oa (cdr a)) (ob (cdr b)))
                             (cond ((= oa 0) t)
                                   ((= ob 0) nil)
                                   (t (< oa ob)))))))
       (let ((n 1))
         (dolist (item items)
           (org-entry-put (car item) "ORDER" (number-to-string n))
           (setq n (1+ n)))))
     groups))
  (org-save-all-org-buffers))

(defun lg/org-agenda-maybe-normalize ()
  "Normalize ORDER on agenda finalize, then redo once to reflect new values.
Uses a persistent flag so the redo's own finalize call does not re-trigger."
  (if lg/org-agenda-normalize-pending
      (setq lg/org-agenda-normalize-pending nil)
    (lg/org-agenda-normalize-orders)
    (setq lg/org-agenda-normalize-pending t)
    (run-at-time 0 nil #'org-agenda-redo)))

(add-hook 'org-agenda-finalize-hook #'lg/org-agenda-maybe-normalize)

(defun lg/agenda-strip-priority-cookies ()
  "Remove [#A]-[#D] priority cookies from the agenda display."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward " *\\[#[A-D]\\] *" nil t)
        (replace-match " ")))))

(add-hook 'org-agenda-finalize-hook #'lg/agenda-strip-priority-cookies)

;; Prefix column offsets (0-indexed from line start):
;;   " ORDER(3)  STATE(4)  TAGS(20)  "
;;     ^6        ^6..9    ^12..31
(defun lg/agenda-colorize ()
  "Apply face colors to state and tags columns in agenda item lines."
  ;; Suppress Emacs's built-in NBSP highlight (nobreak-space face / cyan underline).
  ;; Our tags padding uses U+00A0 to survive org-agenda's trailing-space stripping;
  ;; this prevents it from being rendered visibly.
  (setq-local nobreak-char-display nil)
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when (org-get-at-bol 'org-marker)
          (let* ((bol (line-beginning-position))
                 ;; State: columns 6-9 (4 chars)
                 (state-beg (+ bol 6))
                 (state-end (min (+ bol 10) (line-end-position)))
                 (state (string-trim
                         (buffer-substring-no-properties state-beg state-end)))
                 (state-face (cond
                              ((member state '("NEXT" "PROG")) '+org-todo-active)
                              ((string= state "WAIT") '+org-todo-onhold)
                              ((string= state "KILL") '+org-todo-cancel)
                              ((string= state "DONE") 'org-done)
                              (t 'org-todo))))
            (put-text-property state-beg state-end 'face state-face)
            ;; Tags: columns 12-31 (20 chars)
            ;; Reset the whole field to default first, then color individual tag names.
            (put-text-property (+ bol 12) (min (+ bol 32) (line-end-position))
                               'face 'default)
            (save-excursion
              (goto-char (+ bol 12))
              (while (re-search-forward ":\\([a-zA-Z0-9_@#%]+\\)"
                                        (min (+ bol 32) (line-end-position))
                                        t)
                (put-text-property (match-beginning 1) (match-end 1)
                                   'face `(:foreground
                                           ,(lg/agenda-tag-color
                                             (match-string-no-properties 1))))))))
        (forward-line 1)))))

(add-hook 'org-agenda-finalize-hook #'lg/agenda-colorize)

;; J/K in agenda: swap :ORDER: property with adjacent visible item
(defun lg/org-agenda-item-order (&optional marker)
  (let ((m (or marker (org-get-at-bol 'org-marker))))
    (when m
      (string-to-number (or (org-entry-get m "ORDER") "0")))))

(defun lg/org-agenda-swap-order (direction)
  "Swap :ORDER: of current item with adjacent item in the same priority group.
DIRECTION is 1 (down) or -1 (up). Does nothing if the adjacent item is in a
different priority group."
  (let* ((marker (or (org-get-at-bol 'org-marker) (user-error "No item here")))
         (cur-priority (org-entry-get marker "PRIORITY"))
         (cur-order (lg/org-agenda-item-order marker))
         (swapped nil))
    (save-excursion
      (if (= direction 1)
          (org-agenda-next-item 1)
        (org-agenda-previous-item 1))
      (let ((adj-marker (org-get-at-bol 'org-marker)))
        (when (and adj-marker
                   (equal cur-priority (org-entry-get adj-marker "PRIORITY")))
          (let ((adj-order (lg/org-agenda-item-order adj-marker)))
            (org-entry-put marker     "ORDER" (number-to-string adj-order))
            (org-entry-put adj-marker "ORDER" (number-to-string cur-order)))
          (setq swapped t))))
    (when swapped
      (setq lg/org-agenda-normalize-pending t)
      (org-agenda-redo)
      (cond ((= direction  1) (org-agenda-next-item 1))
            ((= direction -1) (org-agenda-previous-item 1))))))


(defun lg/org-agenda-move-down () (interactive) (lg/org-agenda-swap-order  1))
(defun lg/org-agenda-move-up   () (interactive) (lg/org-agenda-swap-order -1))

(defun lg/org-agenda-ensure-evil-normal ()
  "Keep evil in normal state in org-agenda; super-agenda headers reset it."
  (when (and (derived-mode-p 'org-agenda-mode)
             (not (evil-normal-state-p)))
    (evil-normal-state)))

;; Set todo keywords after all package init, so Doom's +org module can't overwrite us.
;; setq! (customize-set-variable) on org-log-into-drawer triggers org re-init which
;; resets org-todo-keywords to Doom defaults — placing these here guarantees they win.
(after! org
  (setq org-todo-keywords
        '((sequence
           "TODO(t!)"  ; Captured, not yet triaged
           "NEXT(n!)"  ; Triaged — do this next
           "PROG(p!)"  ; Actively in progress
           "WAIT(w!)"  ; Blocked/waiting on someone
           "|"
           "DONE(d!)"  ; Complete
           "KILL(k!)") ; Cancelled
          ))
  (setq org-todo-keyword-faces
        '(("NEXT" . +org-todo-active)
          ("PROG" . +org-todo-active)
          ("WAIT" . +org-todo-onhold)
          ("KILL" . +org-todo-cancel))))

(after! org-agenda
  (evil-set-initial-state 'org-agenda-mode 'normal)
  (add-hook 'post-command-hook #'lg/org-agenda-ensure-evil-normal)
  (map! :map org-agenda-mode-map
        :n "j" #'org-agenda-next-item
        :n "k" #'org-agenda-previous-item
        :n "J" #'lg/org-agenda-move-down
        :n "K" #'lg/org-agenda-move-up
        :n "+" (cmd! (org-capture nil "t"))))

(use-package! org-fancy-priorities
  :hook (org-mode . org-fancy-priorities-mode)
  :config
  (setq! org-priority-highest ?A
         org-priority-default  ?C
         org-priority-lowest   ?D)
  (setq! org-fancy-priorities-list '((?A . "HIGH")
                                     (?B . "MED")
                                     (?C . "LOW")
                                     (?D . "BKG"))
         org-priority-faces '((?A :foreground "DarkRed"     :weight bold)
                              (?B :foreground "DarkOrange4"  :weight bold)
                              (?C :foreground "goldenrod"    :weight bold)
                              (?D :foreground "gray50"       :weight bold))))

(use-package! org-super-agenda
  :after org-agenda
  :config
  (org-super-agenda-mode))

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
