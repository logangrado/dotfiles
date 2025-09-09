;;; ../.dotfiles/doom.d/packages/forge.el -*- lexical-binding: t; -*-

;; (use-package! forge
;;   :after magit                      ; load after Magit, but...
;;   :init
;;   ;; ...make sure we restrict what Forge knows *before* it loads.
;;   (setq forge-alist
;;         '(("github.com" "api.github.com" "github.com" forge-github-repository)
;;           ("gitlab.com" "gitlab.com/api/v4" "gitlab.com" forge-gitlab-repository)))

;;   ;; Optional: list your enterprise/self-hosted hosts here and they’ll be added.
;;   ;; Leave them empty if you don’t have any.
;;   (defvar my/forge-github-enterprise-hosts '()     ; e.g. '("git.myco.com")
;;     "List of GitHub Enterprise hosts.")
;;   (defvar my/forge-gitlab-enterprise-hosts '()     ; e.g. '("gitlab.myco.local")
;;     "List of self-hosted GitLab hosts.")

;;   (dolist (host my/forge-github-enterprise-hosts)
;;     ;; GHE API is usually /api/v3 (adjust if your instance differs)
;;     (add-to-list 'forge-alist
;;                  (list host (format "%s/api/v3" host) host 'forge-github-repository)
;;                  t #'equal))
;;   (dolist (host my/forge-gitlab-enterprise-hosts)
;;     ;; GitLab API is /api/v4
;;     (add-to-list 'forge-alist
;;                  (list host (format "%s/api/v4" host) host 'forge-gitlab-repository)
;;                  t #'equal))

;;   ;; Auth: use whatever you actually keep up to date
;;                                         ; (setq auth-sources '("~/.authinfo.gpg" "~/.authinfo"))
;;   (setq auth-sources '("~/.authinfo"))

;;   :config
;;   ;; (Optional) If sqlite warnings crop up on Emacs 29+, consider:
;;   ;;   (package! emacsql-sqlite-builtin)  ;; then doom sync -u
;;   ;; Forge will auto-detect it.
;;   )
