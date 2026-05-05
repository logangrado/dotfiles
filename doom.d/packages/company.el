;;; ../.dotfiles/doom.d/packages/company.el -*- lexical-binding: t; -*-

;; In org-mode, <s TAB (and similar) expands org-tempo templates.
;; company-active-map normally grabs TAB first, preventing the expansion.
;; This function detects the org-tempo prefix pattern and yields to org-cycle.
(defun lg/company-tab-or-org-tempo ()
  "Complete with company, unless an org-tempo expansion should take priority.
If in org-mode and the current line looks like an org-tempo trigger (e.g. `<s'),
abort company and call `org-cycle' to perform the expansion instead."
  (interactive)
  (if (and (derived-mode-p 'org-mode)
           (string-match-p "^[ \t]*<[a-zA-Z]*$"
                           (buffer-substring-no-properties
                            (line-beginning-position) (point))))
      (progn
        (company-abort)
        (org-cycle))
    (company-complete-selection)))

(use-package! company
  :bind
  (:map company-active-map
        ("<return>" . newline)  ;; Make RET enter a newline
        ("<tab>" . lg/company-tab-or-org-tempo)
        ) ;; Use TAB for completion (instead of complete common)
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0)
  (company-tooltip-idle-delay 0)

  :config
  (after! lsp-mode
    (map! :map lsp-mode-map
          "<tab>" #'company-indent-or-complete-common))
  )
