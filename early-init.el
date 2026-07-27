;;; early-init.el --- My early-init.el -*- lexical-binding: t; -*-

;; Author: Shuto Omura <somura-vanilla@so-icecream.com>

;; Commentary:

;;; Code:

(when-let* (((getenv "EMACS_PROJECT_DAEMON"))
            (daemon-name (getenv "EMACS_PROJECT_DAEMON_NAME"))
            (state-home (or (getenv "XDG_STATE_HOME")
                            (expand-file-name ".local/state" "~")))
            (state-directory
             (file-name-as-directory
              (expand-file-name daemon-name
                                (expand-file-name "emacs/daemons" state-home)))))
  (dolist (directory (list state-directory
                           (expand-file-name "auto-save-list" state-directory)
                           (expand-file-name "transient" state-directory)
                           (expand-file-name "url" state-directory)
                           (expand-file-name "org-persist" state-directory)
                           (expand-file-name "eshell" state-directory)))
    (make-directory directory t)
    (set-file-modes directory #o700))

  (startup-redirect-eln-cache (expand-file-name "eln-cache" state-directory))

  (setq savehist-file (expand-file-name "history" state-directory)
        project-list-file (expand-file-name "projects.eld" state-directory)
        recentf-save-file (expand-file-name "recentf.eld" state-directory)
        bookmark-default-file (expand-file-name "bookmarks.eld" state-directory)
        tramp-persistency-file-name (expand-file-name "tramp" state-directory)
        save-place-file (expand-file-name "places" state-directory)
        custom-file (expand-file-name "custom.el" state-directory)
        abbrev-file-name (expand-file-name "abbrev_defs" state-directory)
        desktop-dirname state-directory
        desktop-path (list state-directory)
        auto-save-list-file-prefix
        (expand-file-name "auto-save-list/.saves-" state-directory)
        transient-history-file (expand-file-name "transient/history.el" state-directory)
        transient-values-file (expand-file-name "transient/values.el" state-directory)
        transient-levels-file (expand-file-name "transient/levels.el" state-directory)
        url-configuration-directory (expand-file-name "url" state-directory)
        org-persist-directory (expand-file-name "org-persist" state-directory)
        org-id-locations-file (expand-file-name "org-id-locations" state-directory)
        org-clock-persist-file (expand-file-name "org-clock-save.el" state-directory)
        eshell-directory-name (expand-file-name "eshell" state-directory)
        server-stop-automatically nil))

(push '(tool-bar-lines . 0) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(provide 'early-init)

;;; early-init.el ends here
