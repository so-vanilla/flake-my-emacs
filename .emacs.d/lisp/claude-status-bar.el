;;; claude-status-bar.el --- Claude Code status display bar -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides a status bar showing Claude Code task status (running, ask, ready)
;; with color-coded display and automatic updates via polling.

;;; Code:

(require 'json)

;; Variables
(defvar claude-status-bar-buffer-name "*Claude Status*"
  "Name of the Claude status bar buffer.")

(defvar claude-status-bar-window nil
  "Window displaying the Claude status bar buffer.")

(defvar claude-status-bar-timer nil
  "Timer for updating Claude status.")

(defcustom claude-status-file-path "/tmp/claude-status"
  "Path to the Claude status file."
  :type 'string
  :group 'claude-status-bar)

(defcustom claude-status-update-interval 2.0
  "Interval in seconds for status updates."
  :type 'number
  :group 'claude-status-bar)

(defcustom claude-status-auto-start t
  "Whether to automatically start status monitoring."
  :type 'boolean
  :group 'claude-status-bar)

;; Faces (catppuccin-latte theme integration)
(defface claude-status-running
  '((t (:foreground "#1e66f5" :weight bold)))
  "Face for running status."
  :group 'claude-status-bar)

(defface claude-status-ask
  '((t (:foreground "#df8e1d" :weight bold)))
  "Face for ask status."
  :group 'claude-status-bar)

(defface claude-status-ready
  '((t (:foreground "#40a02b" :weight bold)))
  "Face for ready status."
  :group 'claude-status-bar)

(defface claude-status-error
  '((t (:foreground "#d20f39" :weight bold)))
  "Face for error status."
  :group 'claude-status-bar)

(defface claude-status-inactive
  '((t (:foreground "#6c6f85" :weight normal)))
  "Face for inactive status."
  :group 'claude-status-bar)

;; Core functions
(defun claude-status-bar-show ()
  "Show Claude status bar."
  (interactive)
  (let ((buffer (get-buffer-create claude-status-bar-buffer-name)))
    (with-current-buffer buffer
      (claude-status-bar--render-buffer))
    (setq claude-status-bar-window
          (display-buffer buffer '((display-buffer-in-side-window)
                                   (side . right)
                                   (slot . 0)
                                   (window-width . 25))))
    (claude-status-bar-start-timer)
    (select-window claude-status-bar-window)))

(defun claude-status-bar-hide ()
  "Hide Claude status bar."
  (interactive)
  (let ((buffer (get-buffer claude-status-bar-buffer-name)))
    (when buffer
      (let ((window (get-buffer-window buffer)))
        (when window
          (delete-window window)
          (setq claude-status-bar-window nil))))
    (claude-status-bar-stop-timer)))

(defun claude-status-bar-toggle ()
  "Toggle Claude status bar visibility."
  (interactive)
  (let ((buffer (get-buffer claude-status-bar-buffer-name)))
    (if (and buffer (get-buffer-window buffer))
        (claude-status-bar-hide)
      (claude-status-bar-show))))

;; Status file handling
(defun claude-status-bar--read-status-file ()
  "Read and parse Claude status file. Returns parsed JSON or nil."
  (condition-case nil
      (when (file-exists-p claude-status-file-path)
        (with-temp-buffer
          (insert-file-contents claude-status-file-path)
          (when (> (buffer-size) 0)
            (goto-char (point-min))
            (json-read))))
    (error nil)))

(defun claude-status-bar--get-status-display (status-data)
  "Convert status data to display string with appropriate face."
  (if (not status-data)
      (propertize "● INACTIVE" 'face 'claude-status-inactive)
    (let ((status (cdr (assq 'status status-data))))
      (pcase status
        ("running" (propertize "● RUNNING" 'face 'claude-status-running))
        ("ask" (propertize "● ASKING" 'face 'claude-status-ask))
        ("ready" (propertize "● READY" 'face 'claude-status-ready))
        ("error" (propertize "● ERROR" 'face 'claude-status-error))
        (_ (propertize "● UNKNOWN" 'face 'claude-status-inactive))))))

;; UI rendering
(defun claude-status-bar--render-buffer ()
  "Render the Claude status display in buffer."
  (let ((inhibit-read-only t)
        (status-data (claude-status-bar--read-status-file))
        (keymap (claude-status-bar--create-keymap)))
    (erase-buffer)
    (insert "Claude Code Status\n")
    (insert "==================\n\n")

    ;; Main status display
    (insert (claude-status-bar--get-status-display status-data))
    (insert "\n\n")

    ;; Additional info (Phase 1: minimal)
    (when status-data
      (let ((timestamp (cdr (assq 'timestamp status-data)))
            (task (cdr (assq 'current_task status-data))))
        (when timestamp
          (insert (format "Updated: %s\n"
                         (format-time-string "%H:%M:%S"
                                           (seconds-to-time (/ timestamp 1000))))))
        (when task
          (insert (format "Task: %s\n" (truncate-string-to-width task 20))))))

    (insert "\n[q] quit  [r] refresh")
    (goto-char (point-min))
    (setq buffer-read-only t)
    (use-local-map keymap)))

(defun claude-status-bar--create-keymap ()
  "Create keymap for Claude status bar."
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "q" 'claude-status-bar-hide)
    (define-key keymap "r" 'claude-status-bar-refresh)
    (define-key keymap "g" 'claude-status-bar-refresh)
    keymap))

(defun claude-status-bar-refresh ()
  "Refresh the status bar display."
  (interactive)
  (when (and claude-status-bar-window
             (window-live-p claude-status-bar-window))
    (with-current-buffer (window-buffer claude-status-bar-window)
      (claude-status-bar--render-buffer))))

;; Timer functions
(defun claude-status-bar-start-timer ()
  "Start the status update timer."
  (claude-status-bar-stop-timer)  ; Ensure no duplicate timers
  (setq claude-status-bar-timer
        (run-with-timer 0 claude-status-update-interval
                       'claude-status-bar-refresh)))

(defun claude-status-bar-stop-timer ()
  "Stop the status update timer."
  (when claude-status-bar-timer
    (cancel-timer claude-status-bar-timer)
    (setq claude-status-bar-timer nil)))

;; Auto-start functionality
(defun claude-status-bar-maybe-auto-start ()
  "Auto-start status bar if enabled."
  (when claude-status-auto-start
    (claude-status-bar-show)))

;; Cleanup on exit
(add-hook 'kill-emacs-hook 'claude-status-bar-stop-timer)

(provide 'claude-status-bar)
;;; claude-status-bar.el ends here