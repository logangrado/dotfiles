;;; jira.el -*- lexical-binding: t; -*-

(defmacro lg/define-transient-map (map transient &rest groups)
  "Define TRANSIENT and Evil bindings from grouped actions."
  (declare (indent 2))
  (let ((actions (apply #'append (mapcar #'cdr groups))))
    `(progn
       (transient-define-prefix ,transient ()
         "Show Jira commands."
         ,@(mapcar
            (lambda (group)
              (apply #'vector
                     (cons (car group)
                           (mapcar
                            (lambda (action)
                              (let ((command (nth 2 action)))
                                (list (car action) (nth 1 action)
                                      (if (eq (car-safe command) 'function)
                                          (cadr command)
                                        command))))
                            (cdr group)))))
            groups))
       (evil-define-key '(normal visual) ,map
         (kbd "h") ,(list 'function transient)
         (kbd "?") ,(list 'function transient)
         ,@(apply #'append
                  (mapcar (lambda (action)
                            `(,(kbd (car action)) ,(nth 2 action)))
                          actions)))
       (evil-make-intercept-map ,map 'normal t)
       (evil-make-intercept-map ,map 'visual t))))

(use-package! jira
  :commands jira-issues
  :init
  (setq jira-api-version 3)
  ;; Set `jira-base-url' locally in computer-locals.el.  jira.el obtains the
  ;; username and API token from auth-source for that host.
  (map! :leader
        (:prefix ("j" . "jira")
         :desc "List issues" "j" #'jira-issues)))

(after! jira-issues
  (defun lg/jira-show-selected-issue ()
    "Show the selected Jira issue in a detail buffer."
    (interactive)
    (let ((issue-key (jira-utils-marked-item)))
      (unless issue-key
        (user-error "No Jira issue selected."))
      (jira-detail-show-issue issue-key)))
  (defun lg/jira-find-issue ()
    "Find and show a Jira issue by key or URL."
    (interactive)
    (require 'jira-detail)
    (jira-detail-find-issue-by-key))
  (defun lg/jira-update-selected-issue ()
    "Update a field on the selected Jira issue."
    (interactive)
    (let ((issue-key (jira-utils-marked-item)))
      (unless issue-key
        (user-error "No Jira issue selected."))
      (jira-api-call
       "GET" (concat "issue/" issue-key)
       :callback
       (lambda (issue _response)
         (jira-detail--issue issue-key issue)
         (with-current-buffer (jira-detail--get-issue-buffer issue-key)
           (jira-detail--update-field))))))
  (defun lg/jira-create-issue ()
    "Create a Jira issue in a selected project."
    (interactive)
    (let* ((project (completing-read "Project: " (mapcar #'car jira-projects)
                                     nil t nil nil "FLWT"))
           (metadata (jira-api-get-project-issue-types project :sync t))
           (issue-types
            (cl-remove-if (lambda (type) (eq (alist-get 'subtask type) t))
                          (append (alist-get 'issueTypes metadata) nil)))
           (choices (mapcar (lambda (type)
                              (cons (alist-get 'name type) (alist-get 'id type)))
                            issue-types))
           (type (completing-read "Issue type: " (mapcar #'car choices) nil t)))
      (jira-complete-ask-issue-fields
       project (cdr (assoc type choices))
       :callback #'jira-detail--create-issue-from-fields)))
  (lg/define-transient-map jira-issues-mode-map lg/jira-issues-menu
    ("Filters"
     ("l" "Filter issues" #'jira-issues-menu))
    ("Issue Actions"
     ("RET" "Show issue" #'lg/jira-show-selected-issue)
     ("U" "Update field" #'lg/jira-update-selected-issue)
     ("n" "New issue" #'lg/jira-create-issue)
     ("f" "Find issue" #'lg/jira-find-issue)
     ("C" "Change status" #'jira-actions-change-issue-menu)
     ("W" "Add worklog" #'jira-actions-add-worklog-menu)
     ("e" "Export issues" #'jira-export-menu)
     ("O" "Open in browser"
      (lambda () (interactive)
        (jira-actions-open-issue (jira-utils-marked-item))))
     ("c" "Copy issue key"
      (lambda () (interactive)
        (jira-actions-copy-issues-id-to-clipboard (jira-utils-marked-item))))
     ("H" "Switch host" #'jira-issues--switch-host-and-refresh)
     ("T" "Tempo worklogs" #'jira-tempo)))
  (add-hook 'jira-issues-mode-hook #'evil-normalize-keymaps))

(after! jira-detail
  (defun lg/jira-remove-comment-at-point ()
    "Remove the comment at point."
    (interactive)
    (jira-detail--remove-comment-at-point))
  (defun lg/jira-edit-comment-at-point ()
    "Edit the comment at point."
    (interactive)
    (jira-detail--edit-comment-at-point))
  (lg/define-transient-map jira-detail-mode-map lg/jira-detail-menu
    ("Comments"
     ("+" "Add comment"
      (lambda () (interactive)
        (jira-detail--add-comment jira-detail--current-key)))
     ("-" "Remove comment at point" #'lg/jira-remove-comment-at-point)
     ("e" "Edit comment at point" #'lg/jira-edit-comment-at-point))
    ("Issue Actions"
     ("C" "Change status" #'jira-actions-change-issue-menu)
     ("O" "Open in browser"
      (lambda () (interactive)
        (jira-actions-open-issue jira-detail--current-key)))
     ("P" "Show parent" #'jira-detail--show-parent-issue)
     ("U" "Update field" (lambda () (interactive) (jira-detail--update-field)))
     ("w" "Update watchers" #'jira-detail--watchers-menu)
     ("f" "Find issue" #'lg/jira-find-issue)
     ("c" "Copy issue key"
      (lambda () (interactive)
        (jira-actions-copy-issues-id-to-clipboard jira-detail--current-key)))
     ("g" "Refresh"
      (lambda () (interactive)
        (jira-detail-show-issue jira-detail--current-key)))
     ("S" "Add subtask"
      (lambda () (interactive) (jira-detail--create-subtask)))
     ("n" "New issue" #'lg/jira-create-issue)))
  (add-hook 'jira-detail-mode-hook #'evil-normalize-keymaps))
