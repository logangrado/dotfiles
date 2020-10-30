(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(doc-view-continuous t)
 '(neo-hidden-regexp-list
   (quote
    ("\\.pyc\\'" ".*.pyc" "^\\." "\\.pyc$" "~$" "^#.*#$" "\\.elc$")))
 '(org-agenda-custom-commands
   (quote
    (("d" todo "DELEGATED" nil)
     ("c" todo "DONE|DEFERRED|CANCELLED" nil)
     ("w" todo "WAITING" nil)
     ("W" agenda ""
      ((org-agenda-ndays 21)))
     ("A" agenda ""
      ((org-agenda-skip-function
        (lambda nil
          (org-agenda-skip-entry-if
           (quote notregexp)
           "\\=.*\\[#A\\]")))
       (org-agenda-ndays 1)
       (org-agenda-overriding-header "Today's Priority #A tasks: ")))
     ("u" alltodo ""
      ((org-agenda-skip-function
        (lambda nil
          (org-agenda-skip-entry-if
           (quote scheduled)
           (quote deadline)
           (quote regexp)
           "
]+>")))
       (org-agenda-overriding-header "Unscheduled TODO entries: "))))))
 '(org-agenda-files
   (quote
    ("~/org/notes.org" "~/org/todo.org ~/org/notes.org")))
 '(org-agenda-ndays 7 t)
 '(org-agenda-show-all-dates t t)
 '(org-agenda-skip-deadline-if-done t t)
 '(org-agenda-skip-scheduled-if-done t t)
 '(org-agenda-start-on-weekday nil t)
 '(org-deadline-warning-days 14)
 '(org-default-notes-file "~/notes.org")
 '(org-fast-tag-selection-single-key (quote expert))
 '(org-remember-store-without-prompt t)
 '(org-remember-templates
   (quote
    ((116 "* TODO %?
  %u" "~/todo.org" "Tasks")
     (110 "* %u %?" "~/notes.org" "Notes"))))
 '(org-reverse-note-order t)
 '(package-selected-packages
   (quote
    (log4j-mode Log4j jsonnet-mode yaml-mode csv-mode flymd tabbar js2-mode iedit i-edit undo-tree real-auto-save exec-path-from-shell latex-preview-pane list-packages-ext org-bullets synonymous magit doom-themes-org doom-themes outline-magic scad-mode auto-complete outshine pretty-mode which-key prettify-greek org org-mode matlab-mode web-mode use-package pbcopy nlinum neotree markdown-mode auctex adaptive-wrap 0blayout)))
 '(remember-annotation-functions (quote (org-remember-annotation)))
 '(remember-handler-functions (quote (org-remember-handler))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-keyword-face ((t (:foreground "green"))))
 '(iedit-occurrence ((t (:background "yellow" :foreground "black"))))
 '(markdown-code-face ((t (:inherit fixed-pitch))))
 '(org-column ((t (:foreground "brightblue" :inverse-video nil)))))
