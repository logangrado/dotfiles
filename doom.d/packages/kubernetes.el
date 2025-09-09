;;; ../.dotfiles/doom.d/packages/kubernetes.el -*- lexical-binding: t; -*-

(use-package! kubernetes

  :config
  (map! :leader
        (:prefix ("k" . "kubectl commands")
                 "k" #'kubernetes-overview
                 "x" #'kubernetes-contexts-use-context
                 "n" #'kubernetes-set-namespace
                 "f" #'kubernetes-file
                 "l" #'kubernetes-logs
                 ))


  )

(use-package! kubernetes-evil
  :ensure t
  :after kubernetes)
