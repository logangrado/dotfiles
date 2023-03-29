;;; ../.dotfiles/doom.d/packages/ibuffer.el -*- lexical-binding: t; -*-

(after! ibuffer
  ;; Don't show magit buffers in ibuffer
  (add-to-list 'ibuffer-never-show-predicates "magit*")
  ;; Don't ask for confirmation when killing buffers
  (setq ibuffer-expert t)
  )
