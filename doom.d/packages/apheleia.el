;;; ../.dotfiles/doom.d/packages/apheleia.el -*- lexical-binding: t; -*-

(use-package apheleia
  :config
  ;; Add/configure formatters
  ;; Use setf to modify the existing element
  ;; (setq python-black-extra-args (list "-l 120"))
  (setf (alist-get 'black apheleia-formatters)
        '("black" "-l" "120" "-"))
  (setf (alist-get 'isort apheleia-formatters)
        '("isort" "--stdout" "--profile" "--black" "-"))
  (setf (alist-get 'jsonnetfmt apheleia-formatters)
        '("jsonnetfmt" "-"))

  ;; Associate formatters
  (add-to-list 'apheleia-mode-alist
               '(jsonnet-mode . jsonnetfmt)
               '(python-mode . (isort black))
               )
  )
