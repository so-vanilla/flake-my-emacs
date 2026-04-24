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

(defvar sidebar-utils-policy 'neutral
  "Current sidebar enforcement policy.
Use `sidebar-utils-set-policy' to change.
Values: `always-show', `neutral', `always-close'.")

;;; --- Registration ---

(cl-defun sidebar-utils-define (&key id components (protected t))
  "Register a sidebar group.
ID is a symbol identifying the group.
COMPONENTS is a list of plists, each with :id :buffer :show-fn :hide-fn :ratio.
The :ratio values should sum to 1.0.
PROTECTED (default t) sets `no-delete-other-windows' on component windows."
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

(defun sidebar-utils--clear-protection (id)
  "Remove `no-delete-other-windows' from all component windows of ID."
  (let ((def (sidebar-utils--get id)))
    (when def
      (dolist (comp (plist-get def :components))
        (when-let ((win (sidebar-utils--component-window comp)))
          (set-window-parameter win 'no-delete-other-windows nil))))))

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
  "Show all components of sidebar ID, apply protection and adjust ratios.
No-op when policy is `always-close'."
  (when (not (eq sidebar-utils-policy 'always-close))
    (let ((def (sidebar-utils--get id)))
      (when def
        (dolist (comp (plist-get def :components))
          (condition-case err
              (funcall (plist-get comp :show-fn))
            (error
             (message "[sidebar-utils] Failed to show %s: %s"
                      (plist-get comp :id) (error-message-string err)))))
        (sidebar-utils--apply-protection id)
        (sidebar-utils--adjust-ratios id)
        (run-with-idle-timer 0.5 nil #'sidebar-utils--verify-and-retry id)))))

(defun sidebar-utils--verify-and-retry (id)
  "Verify all components of ID are visible; retry missing ones once."
  (when (eq sidebar-utils-policy 'always-show)
    (let ((def (sidebar-utils--get id)))
      (when def
        (let ((retried nil))
          (dolist (comp (plist-get def :components))
            (unless (sidebar-utils--component-window comp)
              (condition-case err
                  (progn
                    (funcall (plist-get comp :show-fn))
                    (setq retried t))
                (error
                 (message "[sidebar-utils] Retry failed for %s: %s"
                          (plist-get comp :id) (error-message-string err))))))
          (when retried
            (sidebar-utils--apply-protection id)
            (sidebar-utils--adjust-ratios id)))))))

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

;;; --- C-x 1 protection ---

(defun sidebar-utils--before-delete-other-windows (&rest _)
  "Re-apply sidebar protection before `delete-other-windows' runs."
  (when (eq sidebar-utils-policy 'always-show)
    (dolist (entry sidebar-utils--registry)
      (sidebar-utils--apply-protection (car entry)))))

;;; --- Show-fn gate for always-close ---

(defun sidebar-utils--gate-show-fn (orig-fn &rest args)
  "Around advice that blocks ORIG-FN when policy is `always-close'."
  (unless (eq sidebar-utils-policy 'always-close)
    (apply orig-fn args)))

;;; --- Public API ---

;;;###autoload
(defun sidebar-utils-global-show (id)
  "Activate sidebar ID globally and show it.
The sidebar will persist across perspective switches.
In `always-close' policy, registers ID but does not show."
  (interactive (list (sidebar-utils--read-id "Show sidebar: ")))
  (cl-pushnew id sidebar-utils--global-active)
  (unless (eq sidebar-utils-policy 'always-close)
    (sidebar-utils--show-one id)))

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
  "Ensure sidebar visibility matches the current policy."
  (pcase sidebar-utils-policy
    ('always-show
     (dolist (entry sidebar-utils--registry)
       (let ((id (car entry)))
         (cond
          ;; Active sidebars: always re-show to sync window variables
          ;; and refresh content (e.g. persp-utils-sidebar-window,
          ;; current perspective highlight)
          ((memq id sidebar-utils--global-active)
           (sidebar-utils--show-one id))
          ((sidebar-utils--any-visible-p id)
           (sidebar-utils--hide-one id))))))
    ('always-close
     (dolist (entry sidebar-utils--registry)
       (let ((id (car entry)))
         (when (sidebar-utils--any-visible-p id)
           (sidebar-utils--hide-one id)))))
    ('neutral nil)))

(defun sidebar-utils--after-persp-switch (&rest _)
  "Hook for `persp-switch' to enforce sidebar state after perspective change."
  (run-with-idle-timer 0 nil #'sidebar-utils--enforce-global-state))

;;; --- Policy management ---

;;;###autoload
(defun sidebar-utils-set-policy (policy)
  "Set sidebar enforcement POLICY.
POLICY is one of `always-show', `neutral', `always-close'."
  (interactive
   (list (intern (completing-read "Sidebar policy: "
                                  '("always-show" "neutral" "always-close")
                                  nil t))))
  ;; Remove all existing advice
  (advice-remove 'persp-switch #'sidebar-utils--after-persp-switch)
  (advice-remove 'delete-other-windows #'sidebar-utils--before-delete-other-windows)
  ;; Remove show-fn gates and window protection from registered components
  (dolist (entry sidebar-utils--registry)
    (let ((id (car entry)))
      (dolist (comp (plist-get (cdr entry) :components))
        (advice-remove (plist-get comp :show-fn) #'sidebar-utils--gate-show-fn))
      (unless (eq policy 'always-show)
        (sidebar-utils--clear-protection id))))
  ;; Set new policy
  (setq sidebar-utils-policy policy)
  ;; Apply new policy
  (pcase policy
    ('always-show
     (advice-add 'persp-switch :after #'sidebar-utils--after-persp-switch)
     (advice-add 'delete-other-windows :before #'sidebar-utils--before-delete-other-windows)
     (add-to-list 'window-persistent-parameters '(no-delete-other-windows . writable))
     (dolist (id sidebar-utils--global-active)
       (sidebar-utils--show-one id)))
    ('neutral nil)
    ('always-close
     (advice-add 'persp-switch :after #'sidebar-utils--after-persp-switch)
     ;; Gate all registered show-fns to block external callers
     (dolist (entry sidebar-utils--registry)
       (dolist (comp (plist-get (cdr entry) :components))
         (advice-add (plist-get comp :show-fn) :around #'sidebar-utils--gate-show-fn)))
     ;; Close all visible sidebars now
     (dolist (entry sidebar-utils--registry)
       (let ((id (car entry)))
         (when (sidebar-utils--any-visible-p id)
           (sidebar-utils--hide-one id))))))
  ;; Sync mode variable to avoid desktopmode / customize resetting policy
  (setq sidebar-utils-mode (not (eq policy 'neutral)))
  (message "[sidebar-utils] Policy: %s" policy))

;;;###autoload
(defun sidebar-utils-cycle-policy ()
  "Cycle sidebar policy: always-show -> neutral -> always-close -> always-show."
  (interactive)
  (sidebar-utils-set-policy
   (pcase sidebar-utils-policy
     ('always-show 'neutral)
     ('neutral 'always-close)
     ('always-close 'always-show)
     (_ 'neutral))))

;;;###autoload
(defun sidebar-utils-always-show ()
  "Set sidebar policy to always-show."
  (interactive)
  (sidebar-utils-set-policy 'always-show))

;;;###autoload
(defun sidebar-utils-neutral ()
  "Set sidebar policy to neutral."
  (interactive)
  (sidebar-utils-set-policy 'neutral))

;;;###autoload
(defun sidebar-utils-always-close ()
  "Set sidebar policy to always-close."
  (interactive)
  (sidebar-utils-set-policy 'always-close))

;;; --- Global minor mode ---

;;;###autoload
(define-minor-mode sidebar-utils-mode
  "Global minor mode for unified sidebar management across perspectives."
  :global t
  :group 'sidebar-utils
  (if sidebar-utils-mode
      (sidebar-utils-set-policy 'always-show)
    (sidebar-utils-set-policy 'neutral)))

(provide 'sidebar-utils)
;;; sidebar-utils.el ends here
