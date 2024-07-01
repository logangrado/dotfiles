;;; ../.dotfiles/doom.d/packages/org-roam.el -*- lexical-binding: t; -*-

(use-package! org-roam
  ;;:ensure t
  :after org
  :init
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

  :custom
  (org-roam-completion-everywhere t)
  ;; More detail in default catpure template
  (org-roam-dailies-capture-templates
   '(("d" "default" entry "* %<%I:%M %p>: %?"
      :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))


  :bind
  (("C-c n f" . org-roam-node-find)
   ("C-c n r" . org-roam-node-random)
   :map org-mode-map
   ("C-c n i" . org-roam-node-insert)
   ("C-c n o" . org-id-get-create)
   ("C-c n t" . org-roam-tag-add)
   ("C-c n a" . org-roam-alias-add)
   ("C-c n l" . org-roam-buffer-toggle)
   :map org-roam-dailies-map
   ("Y" . org-roam-dailies-capture-yesterday)
   ("T" . org-roam-dailies-capture-tomorrow))


  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)


  :config
  (org-roam-setup)
  (require 'org-roam-dailies) ;; Ensure the keymap is available
  (org-roam-db-autosync-mode)
  )
