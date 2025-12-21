;;; ../.dotfiles/doom.d/packages/apheleia.el -*- lexical-binding: t; -*-

(use-package apheleia
  :config
  ;; ;; Add/configure formatters
  ;; ;; Use setf to modify the existing element
  ;; ;; (setq python-black-extra-args (list "-l 120"))
  ;; (setf (alist-get 'black apheleia-formatters)
  ;;       '("black" "-l" "120" "-"))
  ;; (setf (alist-get 'isort apheleia-formatters)
  ;;       '("isort" "--stdout" "--profile" "black" "-"))
  (setf (alist-get 'jsonnetfmt apheleia-formatters)
        '("jsonnetfmt" "-"))



  ;; ;; Associate formatters
  ;; ;; replace first matching cell for each mode
  (setf
   ;; (alist-get 'python-mode     apheleia-mode-alist nil nil #'eq) '(isort black)
   ;; (alist-get 'python-ts-mode  apheleia-mode-alist nil nil #'eq) '(isort black)
   (alist-get 'jsonnet-mode    apheleia-mode-alist nil nil #'eq) 'jsonnetfmt
   )

  ;; ---------- Define formatter commands ----------
  ;; Use uvx so tools don’t have to be in the project venv.
  (setf (alist-get 'ruff-fix apheleia-formatters)
        '("uvx" "ruff" "check" "--fix-only" "--line-length" "120" "--extend-ignore" "F401" "--stdin-filename" filepath "-")
        )
  (setf (alist-get 'ruff-format apheleia-formatters)
        '("uvx" "ruff" "format" "--line-length" "120" "--stdin-filename" filepath "-"))
  (setf (alist-get 'ruff-remove-unused-imports apheleia-formatters)
        '("uvx" "ruff" "check" "--fix-only" "--line-length" "120" "--stdin-filename" filepath "-"))
  
  ;; Legacy formatters (Black/isort) kept for projects not yet on Ruff.
  (setf (alist-get 'black apheleia-formatters)
        '("uvx" "black" "-l" "120" "-"))
  (setf (alist-get 'isort apheleia-formatters)
        '("uvx" "isort" "--stdout" "--profile" "black" "-"))

  ;; Default associations (will be overridden per-buffer by our detector)
  (setf (alist-get 'python-mode    apheleia-mode-alist nil nil #'eq) '(ruff-fix ruff-format)
        (alist-get 'python-ts-mode apheleia-mode-alist nil nil #'eq) '(ruff-fix ruff-format))

  (defun lg/apheleia-ruff-remove-unused-imports ()
    "Run Ruff to remove unused imports (F401) in the current buffer."
    (interactive)
    (apheleia-format-buffer 'ruff-remove-unused-imports))

  (map! :leader
        :prefix ("c")
        :desc "Remove unused imports" "F" #'lg/apheleia-ruff-remove-unused-imports
        )
  
  
  ;; ---------- Project-aware selector ----------
  (defun lg/file-contains-p (file regex)
    "Return non-nil if FILE exists and contains REGEX."
    (when (and file (file-readable-p file))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (re-search-forward regex nil t))))

  (defun lg/python-detect-project-formatter ()
    "Pick Ruff or Black/isort for this buffer based on project config."
    (when (derived-mode-p 'python-mode 'python-ts-mode)
      (let* ((root (or (and (fboundp 'projectile-project-root)
                            (ignore-errors (projectile-project-root)))
                       (and (fboundp 'project-current)
                            (when-let ((pr (project-current)))
                              (car (project-roots pr))))
                       default-directory))
             (pyproject (and root (expand-file-name "pyproject.toml" root)))
             (ruff-toml (and root (locate-dominating-file root ".ruff.toml")))
             (use-ruff
              (or ruff-toml
                  (lg/file-contains-p pyproject "\\[tool\\.ruff\\]")))
             (use-black-isort
              (or (lg/file-contains-p pyproject "\\[tool\\.black\\]")
                  (lg/file-contains-p pyproject "\\[tool\\.isort\\]"))))
        (cond
         ;; Prefer explicit Ruff config if present
         (use-ruff
          (setq-local apheleia-formatter '(ruff-fix ruff-format)))
         ;; Else, if explicit Black/isort config is present, use that
         (use-black-isort
          (setq-local apheleia-formatter '(isort black)))
         ;; Fallback: Ruff (modern default)
         (t
          (setq-local apheleia-formatter '(ruff-fix ruff-format)))))))

  (defun lg/python-reselect-formatter ()
    "Manually re-run formatter detection for current buffer."
    (interactive)
    (lg/python-detect-project-formatter)
    (message "Apheleia formatter: %S" apheleia-formatter))

  ;; Run detection on open and when switching projects.
  (add-hook 'python-mode-hook #'lg/python-detect-project-formatter)
  (add-hook 'python-ts-mode-hook #'lg/python-detect-project-formatter)
  (add-hook 'find-file-hook
            (defun lg/python-detect-on-open ()
              (when (derived-mode-p 'python-mode 'python-ts-mode)
                (lg/python-detect-project-formatter))))

  ;; Optional: keybinding for quick toggle / re-detect
  (map! :after apheleia
        :map python-mode-map
        "C-c C-f r" #'lg/python-reselect-formatter)
  )
