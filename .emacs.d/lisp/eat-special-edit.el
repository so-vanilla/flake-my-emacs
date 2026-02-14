;;; eat-special-edit.el --- Special edit buffer for eat terminal -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides a temporary buffer for composing text to send to eat terminal.
;; Supports configurable major mode and multiple template insertion.

;;; Code:

(require 'eat)

(defcustom eat-special-edit-major-mode 'org-mode
  "Major mode to use in the special edit buffer."
  :type 'function
  :group 'eat)

(defcustom eat-special-edit-templates nil
  "List of templates. Each element is a plist:
  :name      - Template name (string, required)
  :template  - Template content (string, required)
  :major-mode - Major mode override (symbol, optional)"
  :type 'sexp
  :group 'eat)

(defcustom eat-special-edit-use-default-template nil
  "When non-nil, auto-insert the default template on buffer open."
  :type 'boolean
  :group 'eat)

(defcustom eat-special-edit-default-template nil
  "Name of the default template to auto-insert."
  :type '(choice (const nil) string)
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
    (define-key map (kbd "M-i") #'eat-special-edit-insert-template)
    map)
  "Keymap for `eat-special-edit-mode'.")

(define-minor-mode eat-special-edit-mode
  "Minor mode for eat special edit buffer."
  :lighter " EatEdit"
  :keymap eat-special-edit-mode-map)

(defun eat-special-edit--find-template (name)
  "Find template plist by NAME from `eat-special-edit-templates'."
  (seq-find (lambda (tmpl) (equal (plist-get tmpl :name) name))
            eat-special-edit-templates))

(defun eat-special-edit--apply-template (tmpl)
  "Erase buffer and insert TMPL plist content. Switch major mode if specified."
  (erase-buffer)
  (insert (plist-get tmpl :template))
  (when-let ((mode (plist-get tmpl :major-mode)))
    (funcall mode))
  (goto-char (point-min)))

(defun eat-special-edit-insert-template ()
  "Select and insert a template from `eat-special-edit-templates'."
  (interactive)
  (unless eat-special-edit-templates
    (user-error "No templates configured"))
  (let* ((names (mapcar (lambda (tmpl) (plist-get tmpl :name))
                        eat-special-edit-templates))
         (selected (completing-read "Template: " names nil t))
         (tmpl (eat-special-edit--find-template selected)))
    (when tmpl
      (eat-special-edit--apply-template tmpl))))

(defun eat-special-edit-open ()
  "Open a buffer for composing text to send to eat terminal."
  (interactive)
  (unless (derived-mode-p 'eat-mode)
    (user-error "Not in an eat buffer"))
  (let* ((source-buf (current-buffer))
         (terminal eat-terminal)
         (win-config (current-window-configuration))
         (edit-buf (generate-new-buffer "*eat-special-edit*"))
         (default-tmpl (when (and eat-special-edit-use-default-template
                                  eat-special-edit-default-template)
                         (eat-special-edit--find-template
                          eat-special-edit-default-template))))
    (pop-to-buffer edit-buf)
    (funcall (if-let ((mode (plist-get default-tmpl :major-mode)))
                 mode
               eat-special-edit-major-mode))
    (eat-special-edit-mode 1)
    (setq-local eat-special-edit--source-buffer source-buf)
    (setq-local eat-special-edit--source-terminal terminal)
    (setq-local eat-special-edit--saved-window-config win-config)
    (when default-tmpl
      (insert (plist-get default-tmpl :template))
      (goto-char (point-min)))
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
