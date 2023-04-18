;;; ../.dotfiles/doom.d/packages/ein.el -*- lexical-binding: t; -*-

(use-package! ein
  :init
  (setq ein:jupyter-server-args (list "--no-browser" "--allow-root"))
  :config
  ;; Localleader bindingsk
  (setq ein:output-area-inlined-images t)
  (map! :after ein
        :map ein:notebook-mode-map
        :localleader
        "j" #'ein:worksheet-goto-next-input-km
        "k" #'ein:worksheet-goto-prev-input-km
        "J" #'ein:worksheet-move-cell-down-km
        "K" #'ein:worksheet-move-cell-up-km
        "a" #'ein:worksheet-insert-cell-above
        "b" #'ein:worksheet-insert-cell-below
        "x" #'ein:worksheet-delete-cell
        "RET" #'ein:worksheet-execute-all-cells
        )

  ;; General keybindings
  (map! :map ein:notebook-mode-map
        "<normal-state> J" #'ein:worksheet-goto-next-input-km
        "<normal-state> K" #'ein:worksheet-goto-prev-input-km
        "<normal-state> RET" #'ein:worksheet-execute-cell-and-goto-next-km
   )

  ;; (setq ein:notebook-modes '(ein:notebook-multilang-mode
  ;;                            ein:notebook-python-mode
  ;;                            ein:notebook-plain-mode))

  )
