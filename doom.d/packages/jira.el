;;; jira.el -*- lexical-binding: t; -*-

(use-package! jira
  :commands jira-issues
  :init
  (setq jira-api-version 3)
  ;; Set `jira-base-url' locally in computer-locals.el.  jira.el obtains the
  ;; username and API token from auth-source for that host.
  (map! :leader
        (:prefix ("j" . "jira")
         :desc "List issues" "j" #'jira-issues)))

(defcustom lg/jira-views nil
  "Personal Jira views.

Each entry is (NAME JQL STATUS-ORDER).  STATUS-ORDER is a list of status
names used for local sorting."
  :type '(repeat (list (string :tag "Name")
                       (string :tag "JQL")
                       (repeat :tag "Status order" string))))

(after! jira-issues
  (defvar-local lg/jira--view-status-order nil)
  (defun lg/jira--status-order-index (issue)
    "Return ISSUE's local status sort position."
    (let* ((status (jira-table-extract-field jira-issues-fields :status-name issue))
           (name (alist-get 'name status)))
      (or (cl-position name lg/jira--view-status-order :test #'string=)
          (length lg/jira--view-status-order))))
  (defun lg/jira-sort-cached-view ()
    "Sort the current page by the active view's status order."
    (interactive)
    (when (and lg/jira--view-status-order jira-issues--raw-issues)
      (setq jira-issues--raw-issues
            (vconcat
             (cl-stable-sort
              (append jira-issues--raw-issues nil)
              (lambda (left right)
                (< (lg/jira--status-order-index left)
                   (lg/jira--status-order-index right))))))
      (setq tabulated-list-entries
            (mapcar #'jira-issues--data-format-issue jira-issues--raw-issues))
      (tabulated-list-print t)))
  (defun lg/jira--refresh-table-with-view-sort (function data response)
    "Sort fetched issues using the active local view before displaying them."
    (funcall function data response)
    (lg/jira-sort-cached-view))
  (advice-add 'jira-issues--refresh-table :around
              #'lg/jira--refresh-table-with-view-sort)
  (defun lg/jira-read-status-order ()
    "Read an optional comma-separated status order."
    (mapcar #'string-trim
            (split-string (read-string "Status order (comma-separated, blank for none): ")
                          "," t)))
  (defun lg/jira-save-view ()
    "Save a personal Jira view in `custom-file'."
    (interactive)
    (let* ((name (read-string "View name: "))
           (jql (read-string "JQL: " jira-issues--current-jql))
           (status-order (lg/jira-read-status-order)))
      (when (string-empty-p name)
        (user-error "View name cannot be empty"))
      (setq lg/jira-views
            (cons (list name jql status-order)
                  (cl-remove name lg/jira-views :key #'car :test #'string=)))
      (customize-save-variable 'lg/jira-views lg/jira-views)
      (message "Saved Jira view: %s" name)))
  (defun lg/jira-open-view ()
    "Fetch and display a saved personal Jira view."
    (interactive)
    (unless lg/jira-views
      (user-error "No saved Jira views"))
    (let* ((name (completing-read "Jira view: " (mapcar #'car lg/jira-views) nil t))
           (view (assoc-string name lg/jira-views)))
      (setq-local lg/jira--view-status-order (nth 2 view))
      (setq jira-issues--current-jql (nth 1 view))
      (jira-issues--reset-pagination)
      (jira-issues--fetch-and-display nil)))
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
    ("Views"
     ("g v" "Open saved view" #'lg/jira-open-view :states normal)
     ("g V" "Save current view" #'lg/jira-save-view :states normal)
     ("g s" "Resort cached view" #'lg/jira-sort-cached-view :states normal))
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
