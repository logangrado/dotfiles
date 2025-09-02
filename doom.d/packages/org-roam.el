;;; ../.dotfiles/doom.d/packages/org-roam.el -*- lexical-binding: t; -*-

(use-package! org-roam
  ;;:ensure t
  :after org
  :init
  (require 'vulpea)

  (setq org-roam-v2-ack t) ;; Acknowledge V2 upgrade
  ;; Old default template, allows searching by tags or title
  (setq org-roam-node-display-template
        (concat "${title:40} "
                (propertize "${tags:50}" 'face 'org-tag)))

  (setq org-roam-directory (concat org-directory "roam/"))

  ;; BEGIN ORG ROAM AGENDA
  ;; =====================
  ;; This series of functions helps for org agenda
  ;; They will dynamically add/remove the project tag from org roam files
  ;; Based on this blog: https://d12frosted.io/posts/2021-01-16-task-management-with-roam-vol5.html
  (defun vulpea-project-p ()
    "Return non-nil if current buffer has any todo entry.
        TODO entries marked as done are ignored, meaning the this
        function returns nil if current buffer contains only completed
        tasks."
    (org-element-map                          ; (2)
        (org-element-parse-buffer 'headline) ; (1)
        'headline
      (lambda (h)
        (eq (org-element-property :todo-type h)
            'todo))
      nil 'first-match))                     ; (3)

  (defun vulpea-project-update-tag ()
    (interactive)
    (message "UPDATING VULPEA PROJECT TAG")
    "Update PROJECT tag in the current buffer."
    (when (and (not (active-minibuffer-window))
               (vulpea-buffer-p))
      (save-excursion
        (goto-char (point-min))
        (let* ((tags (vulpea-buffer-tags-get))
               (original-tags tags))
          (if (vulpea-project-p)
              (setq tags (cons "project" tags))
            (setq tags (remove "project" tags)))

          ;; cleanup duplicates
          (setq tags (seq-uniq tags))

          ;; update tags if changed
          (when (or (seq-difference tags original-tags)
                    (seq-difference original-tags tags))
            (apply #'vulpea-buffer-tags-set tags))))))

  (defun vulpea-buffer-p ()
    "Return non-nil if the currently visited buffer is a note."
    (and buffer-file-name
         (string-prefix-p
          (expand-file-name (file-name-as-directory org-roam-directory))
          (file-name-directory buffer-file-name))))

  (add-hook 'find-file-hook #'vulpea-project-update-tag)
  (add-hook 'before-save-hook #'vulpea-project-update-tag)

  ;; Functions for BUILDING THE AGENDA
  (defun vulpea-project-files ()
    "Return a list of note files containing 'project' tag." ;
    (seq-uniq
     (seq-map
      #'car
      (org-roam-db-query
       [:select [nodes:file]
        :from tags
        :left-join nodes
        :on (= tags:node-id nodes:id)
        :where (like tag (quote "%\"project\"%"))]))))

  (defun vulpea-agenda-files-update (&rest _)
    "Update the value of `org-agenda-files'."
    (interactive)
    (setq org-agenda-files (vulpea-project-files)))

  (advice-add 'org-agenda :before #'vulpea-agenda-files-update)
  (advice-add 'org-todo-list :before #'vulpea-agenda-files-update)

  (defun vulpea-update-all-project-states ()
    (interactive)
    (dolist (file (org-roam-list-files))
      (message "processing %s" file)
      (with-current-buffer (or (find-buffer-visiting file)
                               (find-file-noselect file))
        (vulpea-project-update-tag)
        (save-buffer))))
  ;; END ORG ROAM AGENDA
  ;; ===================

  (defun org-roam-search ()
    "Search org-roam directory using consult-ripgrep. With live-preview."
    (interactive)
    (let ((consult-ripgrep-command "rg --multiline --null --ignore-case --type org --line-buffered --color=always --max-columns=500 --no-heading --line-number . -e ARG OPTS"))
      (consult-ripgrep org-roam-directory "")))

  ;; Org roam weeklies
  ;;==========================================================================================
  (defun org-roam-weeklies--file-path (time)
    "Return the file path for the weekly note corresponding to TIME."
    (let ((filename (format-time-string "weekly/%Y-W%V.org" time)))
      (expand-file-name filename org-roam-directory)))

  (defun org-roam-weeklies-goto-today ()
    "Find or create the weekly note for the current week."
    (interactive)
    (let ((file-path (org-roam-weeklies--file-path (current-time))))
      (find-file file-path)
      (unless (file-exists-p file-path)
        (insert (format "#+title: Week %s, %s\n\n"
                        (format-time-string "%V")
                        (format-time-string "%Y")))
        (save-buffer))))

  (defun org-roam-weeklies--get-week-from-file ()
    "Extract the year and week number from current weekly note filename.
Returns a cons cell (YEAR . WEEK) or nil if not in a weekly note."
    (when buffer-file-name
      (when (string-match "/weekly/\\([0-9]\\{4\\}\\)-W\\([0-9]\\{2\\}\\)\\.org$"
                          buffer-file-name)
        (cons (string-to-number (match-string 1 buffer-file-name))
              (string-to-number (match-string 2 buffer-file-name))))))

  (defun org-roam-weeklies--time-for-week (year week offset)
    "Create a time value for YEAR and WEEK with OFFSET weeks added."
    ;; Convert to time using first day of the week (Monday)
    (let* ((time (encode-time 0 0 0 1 1 year))
           ;; Add weeks to get to target week
           (target-week (+ week offset))
           (seconds-per-week (* 60 60 24 7))
           (offset-seconds (* seconds-per-week (1- target-week))))
      (time-add time offset-seconds)))

  (defun org-roam-weeklies-goto-next ()
    "Go to the next weekly note.
If not in a weekly note, go to next week from current week."
    (interactive)
    (let* ((week-info (or (org-roam-weeklies--get-week-from-file)
                          (org-roam-weeklies--get-current-week)))
           (year (car week-info))
           (week (cdr week-info))
           (next-time (org-roam-weeklies--time-for-week year week 1))
           (file-path (org-roam-weeklies--file-path next-time)))
      (find-file file-path)
      (unless (file-exists-p file-path)
        (insert (format "#+title: Week %s, %s\n\n"
                        (format-time-string "%V" next-time)
                        (format-time-string "%Y" next-time)))
        (save-buffer))))

  (defun org-roam-weeklies-goto-previous ()
    "Go to the previous weekly note.
If not in a weekly note, go to previous week from current week."
    (interactive)
    (let* ((week-info (or (org-roam-weeklies--get-week-from-file)
                          (org-roam-weeklies--get-current-week)))
           (year (car week-info))
           (week (cdr week-info))
           (prev-time (org-roam-weeklies--time-for-week year week -1))
           (file-path (org-roam-weeklies--file-path prev-time)))
      (find-file file-path)
      (unless (file-exists-p file-path)
        (insert (format "#+title: Week %s, %s\n\n"
                        (format-time-string "%V" prev-time)
                        (format-time-string "%Y" prev-time)))
        (save-buffer))))
  ;; END Org roam weeklies
  ;;==========================================================================================

  :custom
  (org-roam-completion-everywhere t)
  ;; More detail in default catpure template
  (org-roam-dailies-capture-templates
   '(("d" "default" entry "* %<%I:%M %p>: %?"
      :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))

  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  ("C-c n w" . org-roam-weeklies-map)

  :bind
  (("C-c n f" . org-roam-node-find)
   ("C-c n r" . org-roam-db-sync)
   ("C-c n s" . org-roam-search)
   :map org-mode-map
   ("C-c n i" . org-roam-node-insert)
   ("C-c n o" . org-id-get-create)
   ("C-c n t" . org-roam-tag-add)
   ("C-c n a" . org-roam-alias-add)
   ("C-c n l" . org-roam-buffer-toggle)
   :map org-roam-dailies-map
   ("Y" . org-roam-dailies-capture-yesterday)
   ("T" . org-roam-dailies-capture-tomorrow)
   :map org-roam-weeklies-map
   ("w" . org-roam-weeklies-goto-today)
   ("n" . org-roam-weeklies-goto-next)
   ("p" . org-roam-weeklies-goto-previous)
   )



  :config
  (org-roam-setup)
  (require 'org-roam-dailies) ;; Ensure the keymap is available
  (org-roam-db-autosync-mode)
  (defvar org-roam-weeklies-map (make-sparse-keymap)
    "Keymap for weekly note commands.")

  )

(use-package! vulpea
  :hook ((org-roam-db-autosync-mode . vulpea-db-autosync-enable))

  )
