;;; ../.dotfiles/doom.d/packages/format-all.el -*- lexical-binding: t; -*-


;; No longer need, using apheleia. Need to test out jsonnet formatter before deleting config
;; DELETE AFTER CONFIRMING FORMATTING WORKS FOR JSONNET!

;; (use-package format-all
;;   :commands format-all-mode
;;   ;; :hook (prog-mode . format-all-mode)
;;   :init
;;   ;; For some reason, we need this in order for format-all to run on save for jsonnet-mode.
;;   ;; https://github.com/doomemacs/doomemacs/issues/6936#issuecomment-1366030502
;;   (advice-remove 'format-all-buffer--from-hook '+format--all-buffer-from-hook-a)
;;   ;; Set formatter for jsonnet mode
;;   ;; (set-formatter! 'jsonnetfmt "jsonnetfmt" :modes '(jsonnet-mode))
;;   :config
;;   (setq-default format-all-formatters
;;                 '("Jsonnet" (jsonnetfmt))
;;                 )
;;   )
