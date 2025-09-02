;;; ../.dotfiles/doom.d/custom_funcs.el -*- lexical-binding: t; -*-

(defun uuidgen-4 ()
  "Return a lowercase RFC4122 UUIDv4 string by calling `uuidgen` on macOS.
Retries a few times until the version nibble is '4' (defensive)."
  (let* ((bin (or (executable-find "uuidgen") "/usr/bin/uuidgen"))
         (tries 0)
         uuid hex)
    (unless (and bin (file-executable-p bin))
      (user-error "uuidgen not found"))
    (while (and (< tries 8)
                (progn
                  (setq uuid (downcase (car (process-lines bin))))
                  (setq hex (replace-regexp-in-string "-" "" uuid))
                  (not (and (= (length hex) 32) (eq (aref hex 12) ?4))))) ; v4 nibble
      (setq tries (1+ tries)))
    uuid))

(defun uuid4-insert (&optional n)
  "Insert a UUIDv4 at point using macOS `uuidgen`.
With prefix arg N, insert N UUIDs (one per line). The last UUID is pushed to the kill ring."
  (interactive "P")
  (unless (eq system-type 'darwin)
    (user-error "This command expects macOS (Darwin)"))
  (let* ((count (prefix-numeric-value (or n 1)))
         (col (current-column))
         last)
    (dotimes (i count)
      (setq last (uuidgen-4))
      (insert last)
      (when (< i (1- count))
        (newline)
        (move-to-column col)))
    (kill-new last)
    (message "Inserted UUIDv4: %s" last)))
