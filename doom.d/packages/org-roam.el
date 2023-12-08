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
  :custom
  (org-roam-directory "~/org/roam")
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
