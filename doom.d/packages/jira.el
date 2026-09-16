;;; jira.el -*- lexical-binding: t; -*-

(use-package! jira
  :commands jira-issues
  :init
  (setq jira-api-version 3)
  ;; Set `jira-base-url' locally in computer-locals.el.  jira.el obtains the
  ;; username and API token from auth-source for that host.
  (map! :leader
        (:prefix ("j" . "jira")
         :desc "List issues" "j" #'lg/jira-list-issues)))

(defcustom lg/jira-filters nil
  "Personal Jira filters.

Each entry is (NAME JQL DEFAULT-VIEW)."
  :type '(repeat (list (string :tag "Name")
                       (string :tag "JQL")
                       (choice (const :tag "No default view" nil) string))))

(defcustom lg/jira-views nil
  "Personal Jira views.

Each entry is (NAME :sort FIELDS :status-order STATUS-ORDER :columns FIELDS)."
  :type '(repeat sexp))

(defcustom lg/jira-default-filter nil
  "Name of the filter opened by `lg/jira-list-issues'."
  :type '(choice (const :tag "Jira default" nil) string))

(defun lg/jira-list-issues ()
  "List issues using the saved default filter when one is configured."
  (interactive)
  (require 'jira-issues)
  (let ((lg/jira--opening-default-filter t))
    (jira-issues)))

(after! jira-issues
  (defvar-local lg/jira--current-filter nil)
  (defvar-local lg/jira--current-view nil)
  (defvar-local lg/jira--loaded-columns nil)
  (defvar lg/jira--opening-default-filter nil)
  (defvar lg/jira--default-columns (copy-sequence jira-issues-table-fields))

  (defun lg/jira--view-value (view property)
    "Return PROPERTY from saved VIEW."
    (plist-get (cdr view) property))
  (defun lg/jira--view-columns (view)
    "Return VIEW's columns, or Jira's standard columns."
    (or (lg/jira--view-value view :columns) jira-issues-table-fields))
  (defun lg/jira--view-fields (view)
    "Return all fields VIEW needs to display and sort issues."
    (delete-dups
     (append (copy-sequence (lg/jira--view-columns view))
             (copy-sequence (lg/jira--view-value view :sort)))))
  (defun lg/jira--set-columns (columns)
    "Use COLUMNS in the current issue list buffer."
    (setq-local jira-issues-table-fields columns)
    (setq tabulated-list-format
          (vconcat
           (mapcar (lambda (field)
                     (list (jira-table-field-name jira-issues-fields field)
                           (jira-table-field-columns jira-issues-fields field) t))
                   columns)))
    (tabulated-list-init-header))
  (defun lg/jira--sort-value (issue field)
    "Return ISSUE's sortable display value for FIELD."
    (let ((value (jira-table-extract-field jira-issues-fields field issue)))
      (cond ((and (listp value) (alist-get 'name value)) (alist-get 'name value))
            ((null value) "")
            (t (format "%s" value)))))
  (defun lg/jira--status-order-index (issue)
    "Return ISSUE's local status sort position."
    (let* ((status (jira-table-extract-field jira-issues-fields :status-name issue))
           (name (downcase (or (alist-get 'name status) ""))))
      (or (cl-position name (lg/jira--view-value lg/jira--current-view :status-order)
                       :test (lambda (left right) (string= left (downcase right))))
          most-positive-fixnum)))
  (defun lg/jira-sort-cached-view ()
    "Sort the current page by the active view's fields."
    (interactive)
    (when (and lg/jira--current-view jira-issues--raw-issues)
      (setq jira-issues--raw-issues
            (vconcat
             (cl-stable-sort
              (append jira-issues--raw-issues nil)
              (lambda (left right)
                (catch 'different
                  (dolist (field (lg/jira--view-value lg/jira--current-view :sort))
                    (let ((left-value (if (eq field :status-name)
                                          (lg/jira--status-order-index left)
                                        (lg/jira--sort-value left field)))
                          (right-value (if (eq field :status-name)
                                           (lg/jira--status-order-index right)
                                         (lg/jira--sort-value right field))))
                      (unless (equal left-value right-value)
                        (throw 'different
                               (if (numberp left-value)
                                   (< left-value right-value)
                                 (string-lessp left-value right-value))))))
                  nil)))))
      (setq tabulated-list-entries
            (mapcar #'jira-issues--data-format-issue jira-issues--raw-issues))
      (tabulated-list-print t)))
  (defun lg/jira--refresh-table-with-view-sort (function data response)
    "Sort fetched issues using the active local view before displaying them."
    (funcall function data response)
    (setq-local lg/jira--loaded-columns
                (if lg/jira--current-view
                    (lg/jira--view-fields lg/jira--current-view)
                  jira-issues-table-fields))
    (lg/jira-sort-cached-view))
  (advice-add 'jira-issues--refresh-table :around
              #'lg/jira--refresh-table-with-view-sort)
  (defun lg/jira--fetch-view-sort-fields (function jql callback &optional page-token)
    "Request non-visible fields required by the current view's sorting."
    (let ((jira-issues-table-fields
           (if lg/jira--current-view
               (lg/jira--view-fields lg/jira--current-view)
             jira-issues-table-fields)))
      (funcall function jql callback page-token)))
  (advice-add 'jira-issues--api-get-issues :around
              #'lg/jira--fetch-view-sort-fields)
  (defun lg/jira-read-status-order ()
    "Read an optional comma-separated status order."
    (mapcar #'string-trim
            (split-string (read-string "Status order (comma-separated, blank for none): ")
                          "," t)))
  (defun lg/jira--read-fields (prompt default)
    "Read Jira fields using PROMPT, with DEFAULT selected."
    (let* ((choices (mapcar (lambda (field)
                              (cons (jira-table-field-name jira-issues-fields field) field))
                            (mapcar #'car jira-issues-fields)))
           (names (mapcar #'car choices))
           (selected (completing-read-multiple prompt names nil t
                                                (mapconcat #'identity
                                                           (mapcar (lambda (field) (cdr (rassoc field choices))) default)
                                                           ","))))
      (mapcar (lambda (name) (cdr (assoc-string name choices))) selected)))
  (defun lg/jira-configure-sort ()
    "Choose the current view's sort fields and optional status order."
    (interactive)
    (let* ((view (or lg/jira--current-view (list "Unsaved view")))
           (sort (lg/jira--read-fields "Sort by (in priority order): "
                                       (or (lg/jira--view-value view :sort) '(:status-name))))
           (status-order (if (memq :status-name sort)
                             (lg/jira-read-status-order)
                           nil)))
      (setq-local lg/jira--current-view
                  (list (car view) :sort sort :status-order status-order
                        :columns (lg/jira--view-columns view)))
      (lg/jira-sort-cached-view)))
  (defun lg/jira-configure-columns ()
    "Choose the columns shown by the current view."
    (interactive)
    (let* ((view (or lg/jira--current-view (list "Unsaved view")))
           (columns (lg/jira--read-fields "Columns: " (lg/jira--view-columns view))))
      (unless columns
        (user-error "A view needs at least one column"))
      (setq-local lg/jira--current-view
                  (list (car view) :sort (lg/jira--view-value view :sort)
                        :status-order (lg/jira--view-value view :status-order)
                        :columns columns))
      (lg/jira--apply-view lg/jira--current-view)))
  (defun lg/jira-save-view ()
    "Save the current sorting and columns as a personal Jira view."
    (interactive)
    (let* ((name (read-string "View name: "))
           (sort (or (and lg/jira--current-view
                          (lg/jira--view-value lg/jira--current-view :sort))
                     '(:status-name)))
           (status-order (or (and lg/jira--current-view
                                  (lg/jira--view-value lg/jira--current-view :status-order))
                             (lg/jira-read-status-order))))
      (when (string-empty-p name)
        (user-error "View name cannot be empty"))
      (setq lg/jira-views
            (cons (list name :sort sort :status-order status-order
                        :columns jira-issues-table-fields)
                  (cl-remove name lg/jira-views :key #'car :test #'string=)))
      (customize-save-variable 'lg/jira-views lg/jira-views)
      (message "Saved Jira view: %s" name)))
  (defun lg/jira--apply-view (view &optional fetch)
    "Apply VIEW, fetching only when its columns were not loaded."
    (setq-local lg/jira--current-view view)
    (let ((columns (lg/jira--view-columns view)))
      (lg/jira--set-columns columns)
      (if (or fetch (not (seq-every-p (lambda (field) (memq field lg/jira--loaded-columns))
                                        (lg/jira--view-fields view))))
          (progn
            (jira-issues--reset-pagination)
            (jira-issues--fetch-and-display nil))
        (lg/jira-sort-cached-view))))
  (defun lg/jira-open-view ()
    "Apply a saved personal view to the current filter."
    (interactive)
    (unless lg/jira-views
      (user-error "No saved Jira views"))
    (let* ((name (completing-read "Jira view: " (mapcar #'car lg/jira-views) nil t))
           (view (assoc-string name lg/jira-views)))
      (lg/jira--apply-view view)))
  (defun lg/jira-save-filter ()
    "Save the current JQL as a personal Jira filter."
    (interactive)
    (let* ((name (read-string "Filter name: " (car-safe lg/jira--current-filter)))
           (jql (read-string "JQL: " jira-issues--current-jql))
           (default-view (car-safe lg/jira--current-view)))
      (when (or (string-empty-p name) (string-empty-p jql))
        (user-error "Filter name and JQL cannot be empty"))
      (setq lg/jira-filters
            (cons (list name jql default-view)
                  (cl-remove name lg/jira-filters :key #'car :test #'string=)))
      (setq-local lg/jira--current-filter (assoc-string name lg/jira-filters))
      (customize-save-variable 'lg/jira-filters lg/jira-filters)
      (message "Saved Jira filter: %s" name)))
  (defun lg/jira-open-filter ()
    "Fetch and display a saved personal Jira filter."
    (interactive)
    (unless lg/jira-filters
      (user-error "No saved Jira filters"))
    (let* ((name (completing-read "Jira filter: " (mapcar #'car lg/jira-filters) nil t))
           (filter (assoc-string name lg/jira-filters))
           (view (and (nth 2 filter) (assoc-string (nth 2 filter) lg/jira-views))))
      (setq-local lg/jira--current-filter filter)
      (setq jira-issues--current-jql (nth 1 filter))
      (if view
          (lg/jira--apply-view view t)
        (progn
          (setq-local lg/jira--current-view nil)
          (lg/jira--set-columns lg/jira--default-columns)
          (jira-issues--reset-pagination)
          (jira-issues--fetch-and-display nil)))))
  (defun lg/jira-set-filter-default-view ()
    "Set the current view as the current filter's default view."
    (interactive)
    (unless lg/jira--current-filter
      (user-error "Open or save a filter first"))
    (unless (and lg/jira--current-view
                 (assoc-string (car lg/jira--current-view) lg/jira-views))
      (user-error "Save the current view first"))
    (setf (nth 2 lg/jira--current-filter) (car lg/jira--current-view))
    (customize-save-variable 'lg/jira-filters lg/jira-filters)
    (message "%s now defaults to %s" (car lg/jira--current-filter)
             (car lg/jira--current-view)))
  (defun lg/jira-set-default-filter ()
    "Make the current filter the one opened by `SPC j j'."
    (interactive)
    (unless lg/jira--current-filter
      (user-error "Open or save a filter first"))
    (setq lg/jira-default-filter (car lg/jira--current-filter))
    (customize-save-variable 'lg/jira-default-filter lg/jira-default-filter)
    (message "%s is now the default Jira filter" lg/jira-default-filter))
  (defun lg/jira--refresh-with-default-filter (function)
    "Use the saved default filter during initial list construction."
    (if (and lg/jira--opening-default-filter lg/jira-default-filter)
        (progn
          (let ((filter (assoc-string lg/jira-default-filter lg/jira-filters)))
            (if filter
                (let ((lg/jira--opening-default-filter nil))
                  (setq-local lg/jira--current-filter filter)
                  (setq jira-issues--current-jql (nth 1 filter))
                  (lg/jira--apply-view
                   (or (and (nth 2 filter)
                            (assoc-string (nth 2 filter) lg/jira-views))
                       (list "Default view" :sort nil :columns lg/jira--default-columns))
                   t))
              (funcall function))))
      (funcall function)))
  (advice-add 'jira-issues--refresh :around #'lg/jira--refresh-with-default-filter)
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
  (transient-define-prefix lg/jira-filter-menu ()
    "Manage saved Jira filters."
    [["Filter"
      ("o" "Open saved filter" lg/jira-open-filter)
      ("n" "Save current filter" lg/jira-save-filter)
      ("d" "Set filter default view" lg/jira-set-filter-default-view)
      ("D" "Set startup filter" lg/jira-set-default-filter)]])
  (transient-define-prefix lg/jira-view-menu ()
    "Manage Jira views."
    [["View"
      ("o" "Open saved view" lg/jira-open-view)
      ("n" "Save current view" lg/jira-save-view)
      ("s" "Configure sorting" lg/jira-configure-sort)
      ("c" "Configure columns" lg/jira-configure-columns)]])
  (lg/define-transient-map jira-issues-mode-map lg/jira-issues-menu
    ("Navigation"
     ("f" "Filters" #'lg/jira-filter-menu :states normal)
     ("," "Views" #'lg/jira-view-menu :states normal)
     ("l" "Filter issues" #'jira-issues-menu)
     ("s" "Find issue" #'lg/jira-find-issue :states normal))
    ("Issue Actions"
     ("RET" "Show issue" #'lg/jira-show-selected-issue)
     ("U" "Update field" #'lg/jira-update-selected-issue)
     ("n" "New issue" #'lg/jira-create-issue)
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
