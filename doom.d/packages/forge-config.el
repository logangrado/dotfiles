;;; ../.dotfiles/doom.d/packages/forge.el -*- lexical-binding: t; -*-

(use-package! forge
  :after magit

  :config
  (setq forge-add-pullreq-refspec nil)

  ;; Example adding Forge source (do this for private hosts)
  ;; (push '("gitlab.com"               ; GITHOST
  ;;         "gitlab.com/api/v4"        ; APIHOST
  ;;         "gitlab.com"               ; WEBHOST and INSTANCE-ID
  ;;         forge-gitlab-repository)    ; CLASS
  ;;       forge-alist)

  )
