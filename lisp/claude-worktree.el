;;; claude-worktree.el --- Git worktree management with Claude Code -*- lexical-binding: t; -*-

;;; Author: Shuto Omura <somura-vanilla@so-icecream.com>

;;; Commentary:
;; Manage git worktrees with perspective.el and claude-code-ide integration.

;;; Code:

(require 'perspective)
(require 'magit)

(defgroup claude-worktree nil
  "Git worktree management with Claude Code integration."
  :group 'tools
  :prefix "claude-worktree-")

(defun claude-worktree--sanitize-branch-name (branch)
  "Convert BRANCH name slashes to hyphens for directory naming."
  (replace-regexp-in-string "/" "-" branch))

(defun claude-worktree--get-repo-root ()
  "Get the repository root directory (parent of .git)."
  (let ((git-dir (magit-git-dir)))
    (when git-dir
      (file-name-directory (directory-file-name git-dir)))))

(defun claude-worktree--get-worktree-parent-dir ()
  "Get the parent directory where worktrees should be created."
  (let ((repo-root (claude-worktree--get-repo-root)))
    (when repo-root
      (file-name-directory (directory-file-name repo-root)))))

(defun claude-worktree--current-branch ()
  "Get the current branch name."
  (magit-get-current-branch))

(defun claude-worktree--list-worktrees ()
  "Get list of worktrees as alist of (path . branch)."
  (let ((worktrees (magit-list-worktrees)))
    (mapcar (lambda (wt)
              (cons (car wt) (cadr wt)))
            worktrees)))

(defun claude-worktree--start-claude-code ()
  "Start claude-code-ide if available."
  (when (fboundp 'claude-code-ide)
    (claude-code-ide)))

(defun claude-worktree--stop-claude-code ()
  "Stop claude-code-ide if available."
  (when (fboundp 'claude-code-ide-stop)
    (claude-code-ide-stop)))

;;;###autoload
(defun claude-worktree-add ()
  "Create a new worktree with perspective and start Claude Code.
Prompts for a new branch name with current branch as prefix."
  (interactive)
  (let* ((current-branch (claude-worktree--current-branch))
         (new-branch (read-string "New branch name: "
                                  (concat current-branch "/")))
         (parent-dir (claude-worktree--get-worktree-parent-dir))
         (dir-name (claude-worktree--sanitize-branch-name new-branch))
         (worktree-path (expand-file-name dir-name parent-dir)))
    (when (file-exists-p worktree-path)
      (user-error "Directory already exists: %s" worktree-path))
    (magit-worktree-branch worktree-path new-branch current-branch)
    (persp-switch dir-name)
    (dired worktree-path)
    (claude-worktree--start-claude-code)
    (message "Created worktree: %s" worktree-path)))

;;;###autoload
(defun claude-worktree-attach ()
  "Attach an existing worktree to a new perspective and start Claude Code."
  (interactive)
  (let* ((worktrees (claude-worktree--list-worktrees))
         (choices (mapcar (lambda (wt)
                            (format "%s (%s)" (car wt) (cdr wt)))
                          worktrees))
         (selected (completing-read "Select worktree: " choices nil t))
         (index (cl-position selected choices :test #'string=))
         (worktree-path (car (nth index worktrees)))
         (persp-name (file-name-nondirectory (directory-file-name worktree-path))))
    (persp-switch persp-name)
    (dired worktree-path)
    (claude-worktree--start-claude-code)
    (message "Attached worktree: %s" worktree-path)))

;;;###autoload
(defun claude-worktree-detach ()
  "Detach current perspective (stop Claude Code) but keep the worktree."
  (interactive)
  (let ((persp-name (persp-current-name)))
    (when (string= persp-name "main")
      (user-error "Cannot detach main perspective"))
    (claude-worktree--stop-claude-code)
    (persp-kill persp-name)
    (message "Detached perspective: %s" persp-name)))

;;;###autoload
(defun claude-worktree-delete ()
  "Delete the worktree and its perspective."
  (interactive)
  (let* ((worktrees (claude-worktree--list-worktrees))
         (choices (mapcar (lambda (wt)
                            (format "%s (%s)" (car wt) (cdr wt)))
                          (cdr worktrees)))
         (selected (completing-read "Delete worktree: " choices nil t))
         (index (cl-position selected choices :test #'string=))
         (worktree-path (car (nth (1+ index) worktrees)))
         (persp-name (file-name-nondirectory (directory-file-name worktree-path))))
    (when (persp-with-name persp-name)
      (persp-switch persp-name)
      (claude-worktree--stop-claude-code)
      (persp-kill persp-name))
    (magit-worktree-delete worktree-path)
    (message "Deleted worktree: %s" worktree-path)))

;;;###autoload (autoload 'hydra-claude-worktree/body "claude-worktree" nil t)
(defhydra hydra-claude-worktree (:hint nil)
  "
^Buffer^        ^Worktree^
^^--------------^^------------------
_j_: next       _a_: add
_k_: prev       _A_: attach
_b_: switch     _d_: detach
                _D_: delete
"
  ("j" persp-next-buffer)
  ("k" persp-prev-buffer)
  ("b" persp-switch-to-buffer)
  ("a" claude-worktree-add :exit t)
  ("A" claude-worktree-attach :exit t)
  ("d" claude-worktree-detach :exit t)
  ("D" claude-worktree-delete :exit t)
  ("q" nil :exit t))

(provide 'claude-worktree)

;;; claude-worktree.el ends here
