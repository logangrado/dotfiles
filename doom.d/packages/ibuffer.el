;;; ../.dotfiles/doom.d/packages/ibuffer.el -*- lexical-binding: t; -*-

(after! ibuffer
  ;; Don't show magit buffers in ibuffer
  (add-to-list 'ibuffer-never-show-predicates "magit*")
  )
