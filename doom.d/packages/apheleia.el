;;; ../.dotfiles/doom.d/packages/apheleia.el -*- lexical-binding: t; -*-

(use-package apheleia
  :config
  ;; Add/configure formatters
  ;; Use setf to modify the existing element
  ;; (setq python-black-extra-args (list "-l 120"))
  (setf (alist-get 'black apheleia-formatters)
        '("black" "-l" "120" "-"))
  (setf (alist-get 'isort apheleia-formatters)
        '("isort" "--stdout" "--profile" "black" "-"))
  (setf (alist-get 'jsonnetfmt apheleia-formatters)
        '("jsonnetfmt" "-"))

  ;; Associate formatters
  ;; replace first matching cell for each mode
  (setf (alist-get 'python-mode     apheleia-mode-alist nil nil #'eq) '(isort black)
        (alist-get 'python-ts-mode  apheleia-mode-alist nil nil #'eq) '(isort black)
        (alist-get 'jsonnet-mode    apheleia-mode-alist nil nil #'eq) 'jsonnetfmt)

  )
