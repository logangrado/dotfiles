;;; ../.dotfiles/doom.d/packages/gptel.el -*- lexical-binding: t; -*-

(use-package! gptel

  :config
  ;; (setq! gptel-api-key "your key")
  ;; BETTER: use ~/.authinfo
  (map! :leader
        (:prefix ("l" . "gptel-commands")
                 "l" #'gptel-here
                 "n" #'gptel-new
                 "m" #'gptel-menu
                 "r" #'gptel-rename-chat
                 ))

  ;; General config
  (add-hook 'gptel-post-stream-hook 'gptel-auto-scroll) ;; Auto-scroll buffer on responses
  (add-hook 'gptel-post-response-functions 'gptel-end-of-response) ;; Move cursor too

  ;; AUTO-SAVING
  (setq gptel-save-logs-to-directory-p t) ;; Enable saving log
  (setq gptel-logs-directory "~/.gptel/") ;; Base directory

  (defun gptel-here ()
    "Open gptel in the current window."
    (interactive)
    (let* ((gptel-buffers (seq-filter
                           (lambda (buf)
                             (with-current-buffer buf
                               (bound-and-true-p gptel-mode)))
                           (buffer-list)))
           (name (completing-read "Buffer Name: "
                                  (mapcar 'buffer-name gptel-buffers)
                                  nil nil nil nil
                                  (generate-new-buffer-name "gptel")))
           (buffer (gptel name)))
      (switch-to-buffer buffer)))


  (defun gptel-new ()
    "Open a new gptel in the current window with buffer name 'gptel-new'."
    (interactive)
    (let ((buffer (gptel "gptel-new"))) ; Use given buffer name 'gptel-new'
      (switch-to-buffer buffer))) ; Switch to the new buffer in the current window

  (defun gptel-rename-chat ()
    (interactive)
    (unless gptel-mode
      (user-error "This command is intended to be used in gptel chat buffers."))
    (let ((gptel-model 'gpt-4o-mini))
      (gptel-request
          (list nil                                    ;user
                "What is the chat content?"            ;llm
                (concat "```" (if (eq major-mode 'org-mode) "org" "markdown") "\n"
                        (buffer-substring-no-properties (point-min) (point-max))
                        "\n```"))                      ;user
        :system
        (list (format                                  ;system message
               "I will provide a transcript of a chat with an LLM.  \
Suggest a short and informative name for a file to store this chat in.  \
Use the following guidelines:
- be very concise, one very short sentence at most
- no spaces, use underscores if required
- return ONLY the title, no explanation or summary
- append the extension .%s"
               (if (eq major-mode 'org-mode) "org" "md")))
        :callback
        (lambda (resp info)                           ;callback called with response and request info
          (if (stringp resp)
              (let ((buf (plist-get info :buffer)))
                (when (and (buffer-live-p buf)
                           ;;(y-or-n-p (format "Rename buffer %s to %s? " (buffer-name buf) resp))
                           )
                  (with-current-buffer buf (rename-buffer resp)))) ;; We could use `rename-visited-file` to also save in the current directory
            (message "Error(%s): did not receive a response from the LLM."
                     (plist-get info :status)))))))

  (defun gptel-save-current-buffer-with-date ()
    (interactive)
    (when (derived-mode-p 'gptel-mode)
      (let* ((current-date (format-time-string "%Y-%m-%d"))
             (buffername (buffer-name))
             (filename (concat gptel-logs-directory current-date buffername)))
        (with-current-buffer (get-buffer buffername)
          (write-file filename)))))


  ;; (defun gptel-rename-buffer-once ()
  ;;   (when (and (eq major-mode 'gptel-mode)
  ;;              (not (bound-and-true-p gptel-renamed)))
  ;;     (setq-local gptel-renamed t)
  ;;     (gptel-rename-chat)
  ;;     ;; Removing this function from the hook so it won't get triggered again
  ;;     (remove-hook 'gptel-after-insert-hook 'gptel-rename-buffer-once)))

  ;; (add-hook 'gptel-after-insert-hook 'gptel-rename-buffer-once)

  )
