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

  ;; -----------------------------------------------------------
  ;; Push & open PR: callback half (paired with magit.el's command).
  ;; -----------------------------------------------------------
  ;; forge-post-submit-callback-hook fires for ALL forge submissions
  ;; (comments, issues, PRs), so a flag scopes browsing to PR-creation
  ;; specifically. Flag is also cleared when the post buffer is killed
  ;; without submitting, so an aborted PR doesn't leak into the next
  ;; unrelated submission.
  (defvar lg/forge--pending-pr-browse nil
    "When non-nil, the next forge post submission's URL is browsed.")

  (defun lg/forge--maybe-browse-new-pr (value &rest _)
    "On `forge-post-submit-callback-hook': browse new PR if flag is set.
GitHub returns the PR URL as `html_url'; GitLab as `web_url'."
    (when lg/forge--pending-pr-browse
      (setq lg/forge--pending-pr-browse nil)
      (let ((url (or (alist-get 'html_url value)
                     (alist-get 'web_url value))))
        (if url
            (condition-case err
                (browse-url url)
              (error (message "PR created at %s (browse failed: %S)" url err)))
          (message "PR submit hook fired without html_url/web_url")))))

  (add-hook 'forge-post-submit-callback-hook
            #'lg/forge--maybe-browse-new-pr 90)

  (defun lg/forge--clear-pending-on-cancel ()
    "On post-mode setup: arrange to clear the flag if the buffer is killed.
On successful submit forge clears the flag first then kills the buffer,
so this only takes effect on abort."
    (when lg/forge--pending-pr-browse
      (add-hook 'kill-buffer-hook
                (lambda () (setq lg/forge--pending-pr-browse nil))
                nil t)))
  (add-hook 'forge-post-mode-hook #'lg/forge--clear-pending-on-cancel))
