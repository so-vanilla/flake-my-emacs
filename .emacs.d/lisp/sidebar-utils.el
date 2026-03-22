;;; sidebar-utils.el --- Unified sidebar management -*- lexical-binding: t; -*-

;;; Commentary:
;; Register sidebar component groups and manage their visibility
;; across perspectives with window protection.

;;; Code:
(require 'cl-lib)

(defgroup sidebar-utils nil
  "Unified sidebar management."
  :group 'convenience)

;;; --- Internal state ---

(defvar sidebar-utils--registry nil
  "Alist of (ID . PLIST).
PLIST keys:
  :components — list of component plists
    each: (:id SYM :buffer STR :show-fn FN :hide-fn FN :ratio NUM)
  :protected — non-nil to set `no-delete-other-windows'.")

(defvar sidebar-utils--global-active nil
  "List of sidebar IDs that should be visible across all perspectives.")

;;; --- Registration ---

(cl-defun sidebar-utils-define (&key id components protected)
  "Register a sidebar group.
ID is a symbol identifying the group.
COMPONENTS is a list of plists, each with :id :buffer :show-fn :hide-fn :ratio.
The :ratio values should sum to 1.0.
PROTECTED non-nil sets `no-delete-other-windows' on component windows."
  (setf (alist-get id sidebar-utils--registry)
        (list :components components :protected protected)))

;;; --- Internal helpers ---

(defun sidebar-utils--get (id)
  "Return the plist for sidebar ID from the registry."
  (alist-get id sidebar-utils--registry))

(defun sidebar-utils--component-window (comp)
  "Return the window displaying COMP's buffer, or nil."
  (get-buffer-window (plist-get comp :buffer) t))

(defun sidebar-utils--all-visible-p (id)
  "Return non-nil if all components of ID have visible windows."
  (let ((def (sidebar-utils--get id)))
    (when def
      (cl-every #'sidebar-utils--component-window
                (plist-get def :components)))))

(defun sidebar-utils--any-visible-p (id)
  "Return non-nil if any component of ID has a visible window."
  (let ((def (sidebar-utils--get id)))
    (when def
      (cl-some #'sidebar-utils--component-window
               (plist-get def :components)))))

(defun sidebar-utils--apply-protection (id)
  "Set `no-delete-other-windows' on all component windows of ID."
  (let ((def (sidebar-utils--get id)))
    (when (and def (plist-get def :protected))
      (dolist (comp (plist-get def :components))
        (when-let ((win (sidebar-utils--component-window comp)))
          (set-window-parameter win 'no-delete-other-windows t))))))

(defun sidebar-utils--adjust-ratios (id)
  "Adjust component window heights of ID according to their :ratio values."
  (let* ((def (sidebar-utils--get id))
         (components (plist-get def :components))
         (windows (mapcar #'sidebar-utils--component-window components))
         (all-present (cl-every #'identity windows)))
    (when (and all-present (> (length windows) 1))
      (let ((total-height (cl-reduce #'+ windows :key #'window-height)))
        (cl-mapc
         (lambda (comp win)
           (let* ((target (round (* total-height (plist-get comp :ratio))))
                  (delta (- target (window-height win))))
             (unless (zerop delta)
               (ignore-errors
                 (window-resize win delta nil t)))))
         components windows)))))

(defun sidebar-utils--show-one (id)
  "Show all components of sidebar ID, apply protection and adjust ratios."
  (let ((def (sidebar-utils--get id)))
    (when def
      (dolist (comp (plist-get def :components))
        (funcall (plist-get comp :show-fn)))
      (sidebar-utils--apply-protection id)
      (sidebar-utils--adjust-ratios id))))

(defun sidebar-utils--hide-one (id)
  "Hide all components of sidebar ID."
  (let ((def (sidebar-utils--get id)))
    (when def
      (dolist (comp (plist-get def :components))
        (funcall (plist-get comp :hide-fn))))))

(defun sidebar-utils--read-id (prompt)
  "Read a sidebar ID with PROMPT using `completing-read'."
  (intern
   (completing-read prompt
                    (mapcar (lambda (entry) (symbol-name (car entry)))
                            sidebar-utils--registry)
                    nil t)))

;;; --- Public API ---

;;;###autoload
(defun sidebar-utils-global-show (id)
  "Activate sidebar ID globally and show it.
The sidebar will persist across perspective switches."
  (interactive (list (sidebar-utils--read-id "Show sidebar: ")))
  (cl-pushnew id sidebar-utils--global-active)
  (sidebar-utils--show-one id))

;;;###autoload
(defun sidebar-utils-global-hide (id)
  "Deactivate sidebar ID globally and hide it.
The sidebar will not reappear on perspective switches."
  (interactive (list (sidebar-utils--read-id "Hide sidebar: ")))
  (setq sidebar-utils--global-active
        (delq id sidebar-utils--global-active))
  (sidebar-utils--hide-one id))

;;;###autoload
(defun sidebar-utils-global-toggle (id)
  "Toggle sidebar ID globally."
  (interactive (list (sidebar-utils--read-id "Toggle sidebar: ")))
  (if (memq id sidebar-utils--global-active)
      (sidebar-utils-global-hide id)
    (sidebar-utils-global-show id)))

(defun sidebar-utils-shown-p (id)
  "Return non-nil if any component of sidebar ID is visible."
  (sidebar-utils--any-visible-p id))

;;; --- Perspective integration ---

(defun sidebar-utils--enforce-global-state ()
  "Ensure sidebar visibility matches `sidebar-utils--global-active'.
Show active sidebars that are missing; hide inactive ones that are visible."
  (dolist (entry sidebar-utils--registry)
    (let ((id (car entry)))
      (cond
       ((and (memq id sidebar-utils--global-active)
             (not (sidebar-utils--all-visible-p id)))
        (sidebar-utils--show-one id))
       ((and (not (memq id sidebar-utils--global-active))
             (sidebar-utils--any-visible-p id))
        (sidebar-utils--hide-one id))))))

(defun sidebar-utils--after-persp-switch (&rest _)
  "Hook for `persp-switch' to enforce sidebar state after perspective change."
  (run-with-idle-timer 0 nil #'sidebar-utils--enforce-global-state))

;;; --- Global minor mode ---

;;;###autoload
(define-minor-mode sidebar-utils-mode
  "Global minor mode for unified sidebar management across perspectives."
  :global t
  :group 'sidebar-utils
  (if sidebar-utils-mode
      (advice-add 'persp-switch :after #'sidebar-utils--after-persp-switch)
    (advice-remove 'persp-switch :after #'sidebar-utils--after-persp-switch)))

(provide 'sidebar-utils)
;;; sidebar-utils.el ends here
