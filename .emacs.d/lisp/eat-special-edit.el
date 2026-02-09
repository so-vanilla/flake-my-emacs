;;; eat-special-edit.el --- Special edit buffer for eat terminal -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides a temporary buffer for composing text to send to eat terminal.
;; Supports configurable major mode and template insertion.

;;; Code:

(require 'eat)

(defcustom eat-special-edit-major-mode 'org-mode
  "Major mode to use in the special edit buffer."
  :type 'function
  :group 'eat)

(defcustom eat-special-edit-template ""
  "Template string inserted when the special edit buffer is opened."
  :type 'string
  :group 'eat)

(defvar-local eat-special-edit--source-buffer nil
  "The eat buffer to send text to.")

(defvar-local eat-special-edit--source-terminal nil
  "The eat terminal object.")

(defvar-local eat-special-edit--saved-window-config nil
  "Saved window configuration to restore after closing.")

(defvar eat-special-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c '") #'eat-special-edit-approve)
    (define-key map (kbd "C-c C-'") #'eat-special-edit-approve)
    (define-key map (kbd "C-c C-k") #'eat-special-edit-abort)
    map)
  "Keymap for `eat-special-edit-mode'.")

(define-minor-mode eat-special-edit-mode
  "Minor mode for eat special edit buffer."
  :lighter " EatEdit"
  :keymap eat-special-edit-mode-map)

(defun eat-special-edit-open ()
  "Open a buffer for composing text to send to eat terminal."
  (interactive)
  (unless (derived-mode-p 'eat-mode)
    (user-error "Not in an eat buffer"))
  (let ((source-buf (current-buffer))
        (terminal eat-terminal)
        (win-config (current-window-configuration))
        (edit-buf (generate-new-buffer "*eat-special-edit*")))
    (pop-to-buffer edit-buf)
    (funcall eat-special-edit-major-mode)
    (eat-special-edit-mode 1)
    (setq-local eat-special-edit--source-buffer source-buf)
    (setq-local eat-special-edit--source-terminal terminal)
    (setq-local eat-special-edit--saved-window-config win-config)
    (when eat-special-edit-template
      (insert eat-special-edit-template))
    (goto-char (point-min))
    (message "Edit, then C-c ' to send or C-c C-k to abort")))

(defun eat-special-edit-approve ()
  "Send the buffer content to eat terminal and close."
  (interactive)
  (let ((content (buffer-string))
        (terminal eat-special-edit--source-terminal)
        (win-config eat-special-edit--saved-window-config))
    (kill-buffer)
    (when win-config
      (set-window-configuration win-config))
    (when (and terminal (not (string-empty-p content)))
      (eat-term-send-string-as-yank terminal content))))

(defun eat-special-edit-abort ()
  "Abort editing and close without sending."
  (interactive)
  (let ((win-config eat-special-edit--saved-window-config))
    (kill-buffer)
    (when win-config
      (set-window-configuration win-config))
    (message "Aborted")))

(provide 'eat-special-edit)

;;; eat-special-edit.el ends here
