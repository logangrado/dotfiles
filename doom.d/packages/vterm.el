;;; ../.dotfiles/doom.d/packages/vterm.el -*- lexical-binding: t; -*-


(use-package! vterm
  :config
  (setq vterm-shell "zsh")
  ;;(set-popup-rule! "^vterm" :size 0.25 :vslot -4 :select t :quit nil :ttl 0)
  ;;(set-popup-rule! "*doom:vterm-popup:main" :size 0.25 :vslot -4 :select t :quit nil :ttl 0 :side 'bottom)
  (defun my-vterm/split-bottom ()
    "Create a vterm window below the current one"
    ;; Now we just got to figure out how to make this a "toggle" popup
    ;; I think we'd have to create a "named" vterm window
    (interactive)
    (let* ((ignore-window-parameters t)
           (dedicated-p (window-dedicated-p)))
      (split-window-vertically (floor (* 0.70 (window-height))))
      (other-window 1)
      (+vterm/here default-directory)))

  ;; MIGHT BE WORTH COPY/PASTING +vterm/toggle's SOURCE CODE AND WORKING FROM THERE?
  ;; it seems like vterm/toggle creates a _named_ vterm buffer, which is what we need to do
 )
