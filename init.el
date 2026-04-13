;;; init.el --- My init.el -*- lexical-binding: t; -*-

;;; Author: Shuto Omura <somura-vanilla@so-icecream.com>

;;; Commentary:

;;; Code:
(defmacro when-darwin (&rest body)
  (when (string= system-type "darwin")
    `(progn ,@body)))

(let ((private-hosts '("vanilla"))
      (current-host (system-name)))
  (defvar is-private-host
    (if (member current-host private-hosts)
        t
      nil)))

(load (expand-file-name "private.el" user-emacs-directory) t)

(when-darwin
 (setq mac-option-modifier 'super
       mac-command-modifier 'meta
       ns-option-modifier 'super
       ns-command-modifier 'meta))

(use-package leaf)

(leaf *leaf
  :config
  (leaf leaf-keywords
    :config
    (leaf-keywords-init))

  (leaf leaf-convert)

  (leaf leaf-tree
    :custom
    ((imenu-list-size . 30)
     (imenu-list-position . 'left)))

  (leaf hydra))

(leaf *emacs-settings
  :config
  (leaf emacs
    :tag "builtin"
    :hook
    ((prog-mode-hook . (lambda ()
                         (setq-local show-trailing-whitespace t))))
    :custom
    ((frame-title-format . '("%b"))
     (ring-bell-function . 'ignore)
     (use-file-dialog . nil)
     (use-short-answers . t)
     (create-lockfiles . nil)
     (tab-width . 4)
     (gc-cons-threshold . 10000000)
     (read-process-output-max . 1048576)
     (enable-recursive-minibuffers . t)
     (system-time-locale . "C")))

  (leaf startup
    :tag "builtin"
    :custom
    ((inhibit-splash-screen . t)
     (inhibit-startup-screen . t)
     (inhibit-startup-buffer-menu . t)))

  (leaf files
    :tag "builtin"
    :custom
    ((make-backup-files . nil)
     (backup-inhibited . nil)
     (major-mode-remap-alist . '((yaml-mode . yaml-ts-mode)))))

  (leaf simple
    :tag "builtin"
    :preface
    (defun my/clipboard-copy-wayland (text)
      (setq copy-process (make-process :name "clipboard-copy"
					                   :buffer nil
					                   :command '("wl-copy" "-n")
					                   :connection-type 'pipe
					                   :noquery t))
      (process-send-string copy-process text)
      (process-send-eof copy-process))

    (defun my/clipboard-paste-wayland ()
      (if (and copy-process (process-live-p copy-process))
	      nil
	    (shell-command-to-string "wl-paste -n | tr -d \\r")))

    (defun my/clipboard-copy-darwin (text)
      (let ((process-connection-type nil))
        (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
          (process-send-string proc text)
          (process-send-eof proc))))

    (defun my/clipboard-paste-darwin ()
      (shell-command-to-string "pbpaste"))

    (defun my-move-beginning-of-line ()
      "Move point to first non-whitespace character or beginning-of-line."
      (interactive "^")
      (let ((orig-point (point)))
	    (back-to-indentation)
	    (when (= orig-point (point))
	      (move-beginning-of-line 1))))

    (defun my-undo ()
      (interactive)
      (undo)
      (hydra-undo/body))

    (defun my-redo ()
      (interactive)
      (undo-redo)
      (hydra-undo/body))
    :hook
    ((before-save-hook . delete-trailing-whitespace))
    :custom
    ((copy-process . nil)
     (indent-tabs-mode . nil))
    :config
    (if (string= system-type "darwin")
        (setq interprogram-cut-function 'my/clipboard-copy-darwin
              interprogram-paste-function 'my/clipboard-paste-darwin)
      (setq interprogram-cut-function 'my/clipboard-copy-wayland
            interprogram-paste-function 'my/clipboard-paste-wayland))
    :bind
    (("C-x u" . my-undo)
     ("C-x r" . my-redo)
     ("C-a" . my-move-beginning-of-line))
    :hydra
    ((hydra-undo
      (:hint nil)
      "
^Undo^
^^-----------
_u_: undo
_r_: redo
"
      ("u" undo)
      ("r" undo-redo)
      ("C-m" nil :exit t)
      ("q" nil :exit t))))

  (leaf faces
    :tag "builtin"
    :preface
    (defun my/compute-font-height (&optional frame)
      "フレームが表示されているモニターのDPIに基づいてフォントサイズを決定する。"
      (let* ((attrs (frame-monitor-attributes frame))
             (geometry (alist-get 'geometry attrs))
             (mm-size (alist-get 'mm-size attrs))
             (px-w (nth 2 geometry))
             (mm-w (nth 0 mm-size))
             (dpi (if (and mm-w (> mm-w 0))
                      (/ (* (float px-w) 25.4) mm-w)
                    96.0)))
        (cond
         ((> dpi 192) 74)   ; 4K/HiDPI (e.g. 224dpi)
         ((> dpi 144) 90)   ; Retina相当
         ((> dpi 120) 82)   ; やや高DPI
         (t 100))))          ; 標準
    (defun my/adjust-font-for-frame (&optional frame)
      "フレームが表示されているモニターに合わせてフォントサイズを調整する。"
      (set-face-attribute 'default frame
                          :height (my/compute-font-height frame)))
    :config
    (set-face-attribute 'default nil
                        :family "DejaVuSansM Nerd Font Mono"
                        :height (my/compute-font-height)
                        :weight 'normal
                        :width 'normal)
    (dolist (target '(japanese-jisx0208
                      japanese-jisx0213.2004-1
                      japanese-jisx0213-2
                      katakana-jisx0201))
      (set-fontset-font t target (font-spec :family "Noto Sans Mono CJK JP")))
    (set-fontset-font t '(#x2300 . #x23FF) (font-spec :family "Noto Sans Mono"))
    (dolist (target '(symbol unicode))
      (set-fontset-font t target (font-spec :family "Noto Sans Mono") nil 'append))
    (add-to-list 'face-font-rescale-alist '("Noto Sans Mono CJK JP" . 1.0))
    (add-to-list 'face-font-rescale-alist '("Noto Sans Mono" . 1.0))
    (add-hook 'after-make-frame-functions #'my/adjust-font-for-frame)
    (add-hook 'move-frame-functions #'my/adjust-font-for-frame))

  (leaf warning
    :tag "builtin"
    :custom
    ((warning-minimum-level . :error)))

  (leaf mule-cmds
    :tag "builtin"
    :bind
    (("C-\\" . nil)
     (mule-keymap
      ("C-\\" . nil))))

  (leaf battery
    :tag "builtin"
    :global-minor-mode display-battery-mode)

  (leaf time
    :tag "builtin"
    :global-minor-mode display-time-mode
    :custom
    ((display-time-24hr-format . t)
     (display-time-default-load-average . nil)))

  (leaf text-mode
    :tag "builtin"
    :custom
    (text-mode-ispell-word-completion . nil))

  (leaf help
    :tag "builtin"
    :bind
    (("C-h K" . describe-keymap)))

  (leaf hl-line
    :tag "builtin"
    :global-minor-mode global-hl-line-mode)

  (leaf which-key
    :tag "builtin"
    :global-minor-mode t)

  (leaf subr
    :tag "builtin"
    :config
    (keyboard-translate ?\C-h ?\C-?))

  (leaf autorevert
    :tag "builtin"
    :global-minor-mode global-auto-revert-mode)

  (leaf editorconfig
    :tag "builtin"
    :global-minor-mode editorconfig-mode)

  (leaf delsel
    :tag "builtin"
    :global-minor-mode delete-selection-mode)

  (leaf window
    :tag "builtin"
    :custom
    (split-width-threshold . nil))

  (leaf eag-config
    :tag "builtin"
    :custom
    (epa-pinentry-mode . 'loopback))

  (leaf kmacro
    :tag "builtin"
    :preface
    (defun my-kmacro-call-macro ()
      "Call the last keyboard macro."
      (interactive)
      (kmacro-end-and-call-macro 1)
      (hydra-kmacro/body))
    :bind
    (("C-x e" . my-kmacro-call-macro))
    :hydra
    ((hydra-kmacro
      (:hint nil)
      "
^Repeat^
^^--------------
  _e_: call
_C-g_: splice
"
      ("e" kmacro-call-macro)
      ("C-g" nil :exit t)
      ("C-m" nil :exit t)
      ("q" nil :exit t)))))

(leaf *cursor
  :config
  (leaf isearch
    :tag "builtin"
    :custom
    ((isearch-allow-scroll . t)))

  (leaf avy
    :url "https://github.com/abo-abo/avy"
    :bind
    (("C-;" . avy-goto-char-in-line)
     ("C-/" . avy-goto-char-timer)
     ("M-/" . avy-goto-end-of-line)
     ("M-?" . avy-goto-line))))


(leaf *pair
  :config
  (leaf elec-pair
    :tag "builtin"
    :global-minor-mode electric-pair-mode
    :hook
    ((minibuffer-mode-hook . (lambda ()
				               (if (not (eq this-command 'eval-expression))
					               (electric-pair-local-mode 0)
				                 nil)))))

  (leaf paren
    :tag "builtin"
    :global-minor-mode show-paren-mode
    :custom
    (show-paren-style 'mixed))

  (leaf puni
    :url "https://github.com/AmaiKinono/puni"
    :global-minor-mode puni-global-mode
    :bind
    ((puni-mode-map
      ("C-k" . puni-kill-line)
      ("M-C-d" . puni-backward-kill-word)
      ("M-C-p" . hydra-puni/body)))
    :hydra
    ((hydra-puni
      (:hint nil)
      "
^Delete^        ^Move^
^^--------------^^-------------------
_C-w_: squeeze  _]_: slurp-forward
_s_: splice     _}_: barf-forward
                _[_: slurp-backward
                _{_: barf-backward}
"
      ("C-w" puni-squeeze)
      ("s" puni-splice)
      ("]" puni-slurp-forward)
      ("}" puni-barf-forward)
      ("[" puni-slurp-backward)
      ("{" puni-barf-backward)
      ("C-m" nil :exit t)
      ("q" nil :exit t))))

  (leaf rainbow-delimiters
    :url "https://github.com/Fanael/rainbow-delimiters"
    :hook
    ((prog-mode-hook . rainbow-delimiters-mode))
    :config
    (set-face-foreground 'rainbow-delimiters-depth-1-face (catppuccin-get-color 'red))
    (set-face-foreground 'rainbow-delimiters-depth-2-face (catppuccin-get-color 'peach))
    (set-face-foreground 'rainbow-delimiters-depth-3-face (catppuccin-get-color 'yellow))
    (set-face-foreground 'rainbow-delimiters-depth-4-face (catppuccin-get-color 'green))
    (set-face-foreground 'rainbow-delimiters-depth-5-face (catppuccin-get-color 'sapphire))
    (set-face-foreground 'rainbow-delimiters-depth-6-face (catppuccin-get-color 'lavender))
    (set-face-foreground 'rainbow-delimiters-depth-7-face (catppuccin-get-color 'mauve))
    (set-face-foreground 'rainbow-delimiters-depth-8-face (catppuccin-get-color 'pink))
    (set-face-foreground 'rainbow-delimiters-depth-9-face (catppuccin-get-color 'flamingo))))

(leaf *window
  :config
  (leaf windmove
    :tag "builtin"
    :bind
    (("C-x o" . hydra-windmove/body))
    :hydra
    ((hydra-windmove
      (:hint nil)
      "
^Direction^
^^-----------
_C-f_: right
_C-b_: left
_C-p_: up
_C-n_: down
"
      ("C-f" windmove-right)
      ("C-b" windmove-left)
      ("C-p" windmove-up)
      ("C-n" windmove-down)
      ("C-m" nil :exit t)
      ("q" nil :exit t)))))

(leaf *vcs
  :config
  (leaf magit
    :url "https://github.com/magit/magit"
    :config
    (leaf forge
      :after magit))

  (leaf smerge-mode
    :tag "builtin"
    :preface
    (defun my/smerge-keep-upper-all ()
      "Keep upper for all conflicts in the buffer."
      (interactive)
      (save-excursion
        (goto-char (point-min))
        (let ((count 0))
          (condition-case nil
              (while t
                (smerge-next)
                (smerge-keep-upper)
                (cl-incf count))
            (error nil))
          (message "Resolved %d conflicts with upper" count))))
    (defun my/smerge-keep-lower-all ()
      "Keep lower for all conflicts in the buffer."
      (interactive)
      (save-excursion
        (goto-char (point-min))
        (let ((count 0))
          (condition-case nil
              (while t
                (smerge-next)
                (smerge-keep-lower)
                (cl-incf count))
            (error nil))
          (message "Resolved %d conflicts with lower" count))))
    :hydra
    ((hydra-smerge
      (:hint nil)
      "
^Move^          ^Keep^              ^All^               ^Other^
^^--------------^^------------------^^------------------^^-----------
_n_: next       _u_: upper (mine)   _U_: upper all      _e_: ediff
_p_: prev       _l_: lower (other)  _L_: lower all      _r_: resolve
^^              _b_: base           ^^                  _R_: refine
^^              _a_: all            ^^                  _c_: combine
^^              _RET_: current"
      ("n" smerge-next)
      ("p" smerge-prev)
      ("u" smerge-keep-upper)
      ("l" smerge-keep-lower)
      ("b" smerge-keep-base)
      ("a" smerge-keep-all)
      ("RET" smerge-keep-current)
      ("U" my/smerge-keep-upper-all :exit t)
      ("L" my/smerge-keep-lower-all :exit t)
      ("e" smerge-ediff :exit t)
      ("r" smerge-resolve)
      ("R" smerge-refine)
      ("c" smerge-combine-with-next)
      ("q" nil :exit t)))
    :config
    (setcdr smerge-mode-map nil)
    (define-key smerge-mode-map (kbd "C-c ^") #'hydra-smerge/body)))

(leaf *minibuffer
  :config
  (leaf vertico
    :url "https://github.com/minad/vertico"
    :global-minor-mode t)

  (leaf orderless
    :url "https://github.com/oantolin/orderless"
    :custom
    ((completion-styles . '(orderless basic))
     (completion-category-overrides . '((file (style basic partial-completion))))
     (orderless-matching-styles . '(orderless-literal
				                    orderless-prefixes
				                    orderless-initialism
				                    orderless-regexp))))

  (leaf marginalia
    :url "https://github.com/minad/marginalia"
    :global-minor-mode t)

  (leaf consult
    :url "https://github.com/minad/consult"
    :bind
    (([remap switch-to-buffer] . consult-buffer)
     ([remap imenu] . consult-imenu)
     ([remap goto-line] . consult-goto-line)
     ("C-s". consult-line)
     ("C-M-s" . consult-ripgrep)
     (minibuffer-mode-map
      ("C-r" . consult-history))))

  (leaf embark-consult
    :url "https://github.com/oantolin/embark"
    :bind
    ((global-map
      :package emacs
      ("M-." . embark-dwin)
      ("C-." . embark-act)))
    :hook
    (embark-collect-mode . consult-preview-at-point-mode)))

(leaf *completion
  :config
  (leaf lsp-bridge
    :url "https://github.com/manateelazycat/lsp-bridge"
    :init
    (global-lsp-bridge-mode)
    (define-prefix-command 'lsp-bridge-prefix)
    :custom
    ((lsp-bridge-enable-completion-in-minibuffer . t)
     (lsp-bridge-enable-completion-in-string . t)
     (lsp-bridge-enable-hover-diagnostic . t)
     (lsp-bridge-enable-org-babel . t))
    :bind
    (("M-l" . lsp-bridge-prefix)
     (lsp-bridge-mode-map
      ("M-l d" . lsp-bridge-find-def)
      ("M-l r" . lsp-bridge-find-references)
      ("M-l a" . lsp-bridge-code-action)
      ("M-l R" . lsp-bridge-rename)
      ("M-l f" . lsp-bridge-format-code)))
    :config
    ;; ts-mode language server mappings missing from lsp-bridge defaults
    (dolist (entry '((kotlin-ts-mode . "kotlin-language-server")
                     (swift-ts-mode . "swift-sourcekit")
                     (toml-ts-mode . "taplo")
                     (hcl-ts-mode . "terraform-ls")))
      (add-to-list 'lsp-bridge-single-lang-server-mode-list entry))
    ;; ts-mode hooks missing from lsp-bridge defaults
    ;; (toml-ts-mode-hook is already in the default list)
    (dolist (hook '(kotlin-ts-mode-hook
                    swift-ts-mode-hook
                    hcl-ts-mode-hook))
      (add-hook hook (lambda ()
                       (when (cl-every (lambda (pred)
                                         (lsp-bridge-check-predicate pred "global-lsp-bridge-mode"))
                                       lsp-bridge-enable-predicates)
                         (lsp-bridge-mode 1)))))

    (leaf acm
      :url "https://github.com/manateelazycat/lsp-bridge"
      :custom
      ((acm-enable-capf . t)
       (acm-enable-copilot . t))
      :bind
      ((acm-mode-map
        ("TAB" . nil)
        ("C-j" . nil)
        ("C-i" . acm-complete)
        ("RET" . nil)
        ("<tab>" . nil)
        ("M-," . nil)
        ("M-." . nil)
        ("M-H" . nil)
        ("M-h" . nil))))

    (leaf yasnipet
      :url "https://github.com/joaotavora/yasnippet"
      :global-minor-mode yas-global-mode)))

(leaf *inline-completion
  :config
  (leaf cape
    :url "https://github.com/minad/cape"
    :config
    (add-to-list 'completion-at-point-functions #'cape-dabbrev)
    (add-to-list 'completion-at-point-functions #'cape-file)
    (add-to-list 'completion-at-point-functions #'cape-elisp-block)
    (add-to-list 'completion-at-point-functions #'cape-elisp-symbol))

  (leaf tempel
    :url "https://github.com/minad/tempel"
    :global-minor-mode global-tempel-abbrev-mode
    :hook
    ((prog-mode-hook . tempel-setup-capf)
     (text-mode-hook . tempel-setup-capf)
     (conf-mode-hook . tempel-setup-capf))
    :custom
    ((tempel-trigger-prefix . ";"))
    :init
    (defun tempel-setup-capf ()
      (setq-local completion-at-point-functions
                  (cons #'tempel-complete completion-at-point-functions)))
    :bind
    ((tempel-map
      ("M-<down>" . nil)
      ("M-<up>" . nil)
      ("M-{" . nil)
      ("M-}" . nil)
      ("C-]" . tempel-next)
      ("C-[" . tempel-previous)))
    :config
    (leaf tempel-collection
      :url "https://github.com/Crandel/tempel-collection")))

(leaf *coding-assistant
  (leaf flycheck
    :url "https://github.com/flycheck/flycheck"
    :global-minor-mode global-flycheck-mode
    :bind
    (flycheck-mode-map
     ("M-n" . flycheck-next-error)
     ("M-p" . flycheck-previous-error))))

(leaf org
  :tag "builtin"
  :custom
  ((org-src-preserve-indentation . nil)
   (org-edit-src-content-indentation . 0)
   (org-use-speed-commands . t)
   (org-directory . "~/org"))
  :bind
  (("<f2>" . hydra-org/body)
   (org-mode-map
    ("M-SPC" . hydra-org-mode/body)))
  :hydra
  ((hydra-org
    (:hint nil :exit t)
    "
^Command^
^^------------
_a_: agenda
_j_: journal
_r_: roam
_c_: capture
_s_: sync CalDAV
"
    ("a" org-agenda)
    ("j" hydra-org-journal/body)
    ("r" hydra-org-roam/body)
    ("c" org-capture)
    ("s" my/org-caldav-sync)
    ("C-m" nil)
    ("q" nil))
   (hydra-org-mode
    (:hint nil)
    "
^Next^             ^Previous^         ^Functions^
^^^^^^-----------------------------------------
_h_: heading       _H_: heading       _t_: timer
_l_: link          _L_: link
_b_: block         _B_: block
_f_: field(table)  _F_: field(table)
_r_: row(table)
"
    ("h" org-next-visible-heading)
    ("l" org-next-link)
    ("b" org-next-block)
    ("f" org-table-next-field)
    ("r" org-table-next-row)
    ("H" org-previous-visible-heading)
    ("L" org-previous-link)
    ("B" org-previous-block)
    ("F" org-table-previous-field)
    ("t" hydra-org-timer/body :exit t)
    ("C-m" nil :exit t)
    ("q" nil :exit t)))
  :config
  (leaf org-agenda
    :tag "builtin"
    :custom
    ((org-agenda-files . '("~/org/todo.org" "~/org/schedule.org"))
     (org-agenda-span . 'day)
     (org-agenda-skip-deadline-if-done . nil)
     (org-agenda-skip-schedule-if-done . nil)
     (org-agenda-skip-deadline-prewarning-if-scheduled . nil)
     (org-log-done . 'time))
    :hook
    ((org-agenda-finalize-hook . my/org-agenda-add-deadline-info))
    :preface
    (defun my/org-agenda-add-deadline-info ()
      "Add deadline/closed info to todo entries in agenda buffer."
      (when (derived-mode-p 'org-agenda-mode)
        (save-excursion
          (goto-char (point-min))
          (while (not (eobp))
            (let ((marker (get-text-property (point) 'org-marker))
                  (type (get-text-property (point) 'type)))
              (when (and marker (string-match-p "todo" (or type "")))
                (let* ((deadline (org-entry-get marker "DEADLINE"))
                       (closed (org-entry-get marker "CLOSED"))
                       (info
                        (cond
                         (closed
                          (let* ((ct (org-time-string-to-time closed))
                                 (formatted (format-time-string "%m/%d" ct)))
                            (format "[Done:%s] " formatted)))
                         (deadline
                          (let* ((dt (org-time-string-to-time deadline))
                                 (now (current-time))
                                 (diff (floor (/ (float-time (time-subtract dt now)) 86400)))
                                 (date-str (format-time-string "%m/%d" dt)))
                            (format "[%s %+dd] " date-str diff)))
                         (t nil))))
                  (when info
                    (let ((inhibit-read-only t))
                      (end-of-line)
                      (let ((eol (point)))
                        (beginning-of-line)
                        (if (re-search-forward "\\([ \t]+\\)\\(:[a-zA-Z0-9_@:]+:\\)\\s-*$" eol t)
                            (goto-char (match-beginning 1))
                          (goto-char eol)))
                      (insert info))))))
            (forward-line 1))))))

  (leaf org-super-agenda
    :url "https://github.com/alphapapa/org-super-agenda"
    :global-minor-mode t
    :custom
    ((org-agenda-custom-commands .
                                 `(("g" "General (Private)"
                                    ((agenda ""
                                             ((org-agenda-files '("~/org/todo.org" "~/org/schedule.org"))
                                              (org-agenda-span 'day)
                                              (org-super-agenda-groups `((:name "schedule"
                                                                                :time-grid t
                                                                                :date today
                                                                                :deadline today)
                                                                         (:discard (:anything t))))))
                                     (alltodo ""
                                              ((org-agenda-span 'day)
                                               (org-agenda-files '("~/org/todo.org"))
                                               (org-agenda-entry-types '(deadline scheduled timestamp))
                                               (org-super-agenda-groups `((:name "Mind"
                                                                                 :tag "mind")
                                                                          (:name "Overdue"
                                                                                 :deadline past)
                                                                          (:name "Today"
                                                                                 :deadline today)
                                                                          (:name "Due Soon"
                                                                                 :and (:deadline (before ,(org-read-date nil nil "+1w"))
                                                                                                 :deadline (after ,(org-read-date nil nil ""))))
                                                                          (:name "Daily"
                                                                                 :tag "daily")
                                                                          (:name "Weekly"
                                                                                 :tag "weekly")
                                                                          (:name "Monthly"
                                                                                 :tag "monthly")
                                                                          (:name "Life"
                                                                                 :tags ("food" "household" "clothes" "chores"))
                                                                          (:name "Emacs"
                                                                                 :tag "emacs")
                                                                          (:name "NixOS"
                                                                                 :tag "nixos")))))))
                                   ("w" "Work"
                                    ((agenda ""
                                             ((org-agenda-files '("~/org/todo.org" "~/org/schedule.org"))
                                              (org-agenda-span 'day)
                                              (org-super-agenda-groups `((:name "schedule"
                                                                                :time-grid t
                                                                                :date today
                                                                                :deadline today)
                                                                         (:discard (:anything t))))))
                                     (alltodo ""
                                              ((org-agenda-span 'day)
                                               (org-agenda-files '("~/org/todo.org"))
                                               (org-agenda-entry-types '(deadline scheduled timestamp))
                                               (org-super-agenda-groups `((:name "Mind"
                                                                                 :tag "mind")
                                                                          (:name "Overdue"
                                                                                 :deadline past)
                                                                          (:name "Today"
                                                                                 :deadline today)
                                                                          (:name "Due Soon"
                                                                                 :and (:deadline (before ,(org-read-date nil nil "+1w"))
                                                                                                 :deadline (after ,(org-read-date nil nil ""))))
                                                                          (:name "Daily"
                                                                                 :tag "daily")
                                                                          (:name "Weekly"
                                                                                 :tag "weekly")
                                                                          (:name "Monthly"
                                                                                 :tag "monthly")
                                                                          (:name "Emacs"
                                                                                 :tag "emacs")
                                                                          (:name "NixOS"
                                                                                 :tag "nixos")
                                                                          (:discard (:tags ("food" "household" "clothes" "chores")))))))))))))

  (leaf org-capture
    :tag "builtin"
    :custom
    (org-capture-templates .
                           '(("t" "Todo" entry (file "~/org/todo.org")
                              "* TODO %?\n")
                             ("s" "Schedule" entry (file "~/org/schedule.org")
                              "* %?\n"))))

  (leaf org-timer
    :tag "builtin"
    :hydra
    ((hydra-org-timer
      (:hint nil :exit t)
      "
^Command^
^^--------------------
_b_: start
_B_: set timer
_e_: stop
_p_: pause
_i_: insert
_I_: insert as item
"
      ("b" org-timer-start)
      ("B" org-timer-set-timer)
      ("e" org-timer-stop)
      ("p" org-timer-pause-or-continue)
      ("i" org-timer)
      ("I" org-timer-item)
      ("C-m" nil)
      ("q" nil))))

  (leaf org-journal
    :url "https://github.com/bastibe/org-journal"
    :custom
    ((org-journal-dir . "~/org/journal/")
     (org-journal-file-format . "%Y-%m-%d.org"))
    :hydra
    ((hydra-org-journal
      (:hint nil :exit t)
      "
^Open^          ^Search^
^^^^-----------------------------------
_t_: today      _s_: search(calendar)
_n_: new entry  _S_: search(all)
^ ^             _y_: calendar year
^ ^             _m_: calendar month
^ ^             _w_: calendar week
"
      ("t" org-journal-open-current-journal-file)
      ("n" org-journal-new-entry)
      ("s" org-journal-search)
      ("S" org-journal-search-forever)
      ("y" org-journal-search-calendar-year)
      ("m" org-journal-search-calendar-month)
      ("w" org-journal-search-calendar-week)
      ("C-m" nil)
      ("q" nil))))

  (leaf org-roam
    :url "https://github.com/org-roam/org-roam"
    :custom
    `((org-roam-directory . ,(file-truename "~/org/org-roam")))
    :config
    (org-roam-db-autosync-mode)
    :hydra
    ((hydra-org-roam
      (:hint nil :exit t)
      "
^Node^       ^Dailies^                                     ^Other^
^^^^^^--------------------------------------------------------------
_f_: find    _t_: today(goto)     _y_: yesterday(goto)     _s_: sync
_i_: insert  _T_: today(capture)  _Y_: yesterday(capture)  _g_: graph
_r_: random  _d_: date(goto)      _n_: tomorrow(goto)
^ ^          _D_: date(capture)   _N_: tomorrow(capture)
"
      ("f" org-roam-node-find)
      ("i" org-roam-node-insert)
      ("r" org-roam-node-random)
      ("t" org-roam-dailies-goto-today)
      ("T" org-roam-dailies-capture-today)
      ("d" org-roam-dailies-goto-date)
      ("D" org-roam-dailies-capture-date)
      ("y" org-roam-dailies-goto-yesterday)
      ("Y" org-roam-dailies-capture-yesterday)
      ("n" org-roam-dailies-goto-tomorrow)
      ("N" org-roam-dailies-capture-tomorrow)
      ("s" org-roam-db-sync)
      ("g" org-roam-graph)
      ("q" nil))))

  (leaf org-caldav
    :url "https://github.com/dengste/org-caldav"
    :if is-private-host
    :custom
    ((org-icalendar-timezone . "Asia/Tokyo")
     (org-icalendar-include-todo . 'all)
     (org-caldav-sync-todo . t)
     (org-caldav-delete-calendar-entries . 'always)
     (org-caldav-delete-org-entries . 'always)
     (org-icalendar-categories . '(local-tags))
     (org-caldav-show-sync-results . nil))
    :config
    (let ((todo (expand-file-name "todo.org" org-directory))
          (sched (expand-file-name "schedule.org" org-directory)))
      (setq org-caldav-calendars
            `((:calendar-id "67B2-67412200-1A7-7F1B4200" :files (,todo) :inbox ,todo)
              (:calendar-id "11D3-67412200-21D-48611280" :files (,sched) :inbox ,sched))))
    (defun my/org-caldav-sync (&optional silent)
      "org-caldav-sync with unsaved buffer check.
SILENT non-nil skips prompt and aborts if unsaved."
      (interactive)
      (let* ((files (mapcan (lambda (cal)
                              (copy-sequence (plist-get cal :files)))
                            org-caldav-calendars))
             (unsaved (seq-filter
                       (lambda (f)
                         (when-let ((buf (find-buffer-visiting f)))
                           (buffer-modified-p buf)))
                       files)))
        (cond
         ((null unsaved)
          (save-window-excursion (org-caldav-sync)))
         (silent nil)
         ((y-or-n-p (format "未保存のバッファがあります: %s 保存して同期しますか？"
                            (mapconcat #'file-name-nondirectory unsaved ", ")))
          (dolist (f unsaved)
            (with-current-buffer (find-buffer-visiting f)
              (save-buffer)))
          (save-window-excursion (org-caldav-sync))))))
    (run-with-timer 900 900 (lambda () (my/org-caldav-sync t))))

  (leaf org-gfm
    :url "https://github.com/larstvei/ox-gfm"
    :after org)

  (leaf org-pomodoro
    :url "https://github.com/marcinkoziej/org-pomodoro")

  (leaf org-download
    :url "https://github.com/abo-abo/org-download"
    :hook
    ((org-mode-hook . org-download-enable))
    :custom
    ((org-download-method . 'directory)
     (org-download-image-dir . "~/org/images")))

  (leaf valign
    :url "https://github.com/casouri/valign"
    :hook
    ((org-mode-hook . valign-mode)))

  (leaf org-present
    :url "https://github.com/rlister/org-present"
    :hook
    ((org-present-mode-hook
      . (lambda ()
          (org-present-big)
          (org-display-inline-images)
          (org-present-hide-cursor)
          (org-present-read-only)))
     (org-present-mode-quit-hook
      . (lambda ()
          (org-present-small)
          (org-remove-inline-images)
          (org-present-show-cursor)
          (org-present-read-write)))))

  (leaf org-superstar
    :url "https://github.com/integral-dw/org-superstar-mode"
    :hook
    ((org-mode-hook . org-superstar-mode))))

(leaf *language
  :config
  (leaf treesit
    :tag "builtin"
    :custom
    ((treesit-language-source-alist
      . '((bash "https://github.com/tree-sitter/tree-sitter-bash")
          (c "https://github.com/tree-sitter/tree-sitter-c")
          (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
          (go "https://github.com/tree-sitter/tree-sitter-go")
          (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
          (hcl "https://github.com/tree-sitter-grammars/tree-sitter-hcl")
          (html "https://github.com/tree-sitter/tree-sitter-html")
          (java "https://github.com/tree-sitter/tree-sitter-java")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (kotlin "https://github.com/fwcd/tree-sitter-kotlin")
          (lua "https://github.com/tree-sitter-grammars/tree-sitter-lua")
          (make "https://github.com/alemuller/tree-sitter-make")
          (nix "https://github.com/nix-community/tree-sitter-nix")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (rust "https://github.com/tree-sitter/tree-sitter-rust")
          (swift "https://github.com/tree-sitter/tree-sitter-swift")
          (toml "https://github.com/tree-sitter/tree-sitter-toml")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
          (yaml "https://github.com/tree-sitter-grammars/tree-sitter-yaml")))
     (treesit-font-lock-level . 4))
    :config
    (defun my-treesit-install-all-grammars ()
      "Install all tree-sitter grammars."
      (interactive)
      (dolist (lang (mapcar #'car treesit-language-source-alist))
        (unless (treesit-language-available-p lang)
          (message "Installing %s..." lang)
          (treesit-install-language-grammar lang))))

    ;; 起動時に未インストールのグラマーを自動インストール（ccが必要）
    (when (executable-find "cc")
      (my-treesit-install-all-grammars))

    (leaf bash-ts-mode
      :tag "builtin"
      :mode "\\.sh\\'" "\\.bash\\'")

    (leaf css-ts-mode
      :tag "builtin"
      :mode "\\.css\\'")

    (leaf dockerfile-ts-mode
      :tag "builtin"
      :mode "Dockerfile\\'")

    (leaf go-ts-mode
      :tag "builtin"
      :mode "\\.go\\'")

    (leaf go-mod-ts-mode
      :tag "builtin"
      :mode "go\\.mod\\'")

    (leaf hcl-ts-mode
      ;; hcl is not supported in Emacs built-in treesit yet
      :if nil
      :mode "\\.tf\\'" "\\.tfvars\\'")

    (leaf html-ts-mode
      :tag "builtin"
      :mode "\\.html?\\'")

    (leaf java-ts-mode
      :tag "builtin"
      :mode "\\.java\\'")

    (leaf js-ts-mode
      :tag "builtin"
      :mode "\\.js\\'")

    (leaf json-ts-mode
      :tag "builtin"
      :mode "\\.json\\'")

    (leaf kotlin-ts-mode
      :url "https://gitlab.com/bricka/emacs-kotlin-ts-mode/"
      :mode "\\.kt\\'" "\\.kts\\'")

    (leaf lua-ts-mode
      :tag "builtin"
      :mode "\\.lua\\'")

    (leaf nix-ts-mode
      :url "https://github.com/nix-community/nix-ts-mode"
      :mode "\\.nix\\'")

    (leaf python-ts-mode
      :tag "builtin"
      :mode "\\.py\\'")

    (leaf rust-ts-mode
      :tag "builtin"
      :mode "\\.rs\\'")

    (leaf swift-ts-mode
      :url "https://github.com/rechsteiner/swift-ts-mode"
      :mode "\\.swift\\'")

    (leaf toml-ts-mode
      :tag "builtin"
      :mode "\\.toml\\'")

    (leaf typescript-ts-mode
      :tag "builtin"
      :mode ("\\.ts\\'"
             (("\\.tsx\\'") . tsx-ts-mode)))

    (leaf yaml-ts-mode
      :tag "builtin"
      :mode "\\.ya?ml\\'"))

  (leaf cc-mode
    :tag "builtin"
    :custom
    ((c-default-style . "gnu")
     (c-basic-offset . 4)))

  (leaf clojure-mode
    :url "https://github.com/clojure-emacs/clojure-mode"
    :config
    (leaf cider
      :url "https://github.com/clojure-emacs/cider"
      :bind
      ((cider-repl-mode-map
        ("M-RET" . cider-repl-newline-and-indent))))

    (leaf clj-deps-new
      :url "https://github.com/jpe90/emacs-clj-deps-new"))

  (leaf elisp-mode
    :tag "builtin")

  )

(leaf *ai-assistant
  :config
  (leaf copilot
    :url "https://github.com/copilot-emacs/copilot.el"
    :config
    (copilot-mode . nil)
    :bind
    ((copilot-completion-map
      ("<tab>" . copilot-accept-completion-by-line)
      ("C-<tab>" . copilot-accept-completion-by-word))))

  (leaf copilot-chat
    :url "https://github.com/chep/copilot-chat.el")

  (if (file-exists-p "~/repos/github.com/manzaltu/claude-code-ide.el/")
      (progn
        (add-to-list 'load-path "~/repos/github.com/manzaltu/claude-code-ide.el")
        (leaf claude-code-ide
          :ensure nil
          :custom
          ((claude-code-ide-terminal-backend . 'vterm)
           (claude-code-ide-cli-extra-flags . "--dangerously-skip-permissions"))
          :bind (("M-c" . claude-code-ide-menu)))
        (leaf claude-code-ide-modeline
          :after claude-code-ide
          :require t
          :global-minor-mode claude-code-ide-modeline-mode))))

(leaf *others
  :config
  (leaf claude-code-utils-session-status
    :require t
    :global-minor-mode claude-code-utils-session-status-mode)

  (leaf persp-utils
    :require t
    :custom
    ((persp-utils-sidebar-session-status-function . 'claude-code-utils-session-status-format)
     (persp-utils-workspace-auto-setup-on-startup . t)
     (persp-utils-workspace-default-perspective . "general")
     (persp-utils-workspace-kill-initial-perspective . "main")
     (persp-utils-terminal-function . 'eshell)
     (persp-utils-workspace-templates
      . '((:name "general" :dir "~/org/"
                 :setup (lambda (&optional _dir)
                          (let ((org-agenda-window-setup 'current-window))
                            (org-agenda nil "g"))
                          (when (derived-mode-p 'org-agenda-mode)
                            (local-set-key (kbd "q") #'ignore))))
          (:name "emacs"
                 :condition (file-directory-p "~/repos/github.com/so-vanilla/flake-my-emacs")
                 :dir "~/repos/github.com/so-vanilla/flake-my-emacs"
                 :setup (lambda (&optional dir) (dired (or dir default-directory))))
          (:name "claude"
                 :condition (file-directory-p "~/repos/github.com/so-vanilla/flake-my-claude")
                 :dir "~/repos/github.com/so-vanilla/flake-my-claude"
                 :setup (lambda (&optional dir) (dired (or dir default-directory))))
          (:name "work" :dir "~/"
                 :setup (lambda (&optional dir) (dired (or dir default-directory)))))))
    :config
    (with-eval-after-load 'claude-code-utils-session-status
      (add-hook 'claude-code-utils-session-status--change-hook
                (lambda ()
                  (run-with-idle-timer 0.1 nil #'persp-utils-sidebar-refresh)))
      (add-hook 'persp-utils-sidebar-refresh-hook
                #'claude-code-utils-session-status--scan-directory))
    (add-hook 'persp-utils-workspace-post-setup-hook
              (lambda ()
                (sidebar-utils-global-show 'left-panel))))

  (leaf perspective
    :url "https://github.com/nex3/perspective-el"
    :custom
    ((persp-suppress-no-prefix-key-warning . t)
     (persp-show-modestring . nil))
    :init
    (persp-mode)

    :bind
    (("M-m" . hydra-perspective-side-bar/body)
     ("M-j" . persp-next)
     ("M-k" . persp-prev))

    :hydra
    ((hydra-perspective-side-bar
      (:hint nil)
      "
^Perspective^            ^Navigate^          ^Sidebar^
^^-----------------------------------------------------------
_c_: create              _n_: next           _s_: show
_k_: kill                _p_: previous       _t_: toggle
_r_: rename              _j_: next           _f_: focus
_o_: open project        _k_: previous       _q_: quit
"
      ("c" persp-switch)
      ("k" persp-kill)
      ("r" persp-rename)
      ("o" persp-utils-project-ghq :exit t)
      ("n" persp-next)
      ("p" persp-prev)
      ("j" persp-next)
      ("p" persp-prev)
      ("s" persp-utils-sidebar-show :exit t)
      ("t" persp-utils-sidebar-toggle :exit t)
      ("f" persp-utils-sidebar-focus :exit t)
      ("q" nil :exit t)
      ("C-m" nil :exit t))))

  (leaf projectile
    :global-minor-mode t
    :bind
    (("C-x C-p" . projectile-command-map)))

  (leaf direnv
    :url "https://github.com/wbolster/emacs-direnv"
    :global-minor-mode t)

  (leaf vterm
    :url "https://github.com/akermu/emacs-libvterm"
    :custom
    ((vterm-max-scrollback . 10000)
     (vterm-kill-buffer-on-exit . t)
     (vterm-copy-mode-remove-fake-newlines . t))
    :hook
    ((vterm-mode-hook . (lambda () (puni-mode -1))))
    :config
    (customize-set-variable
     'vterm-keymap-exceptions
     (append '("M-j" "M-k" "M-m" "M-c" "M-:" "M-e" "M-i")
             vterm-keymap-exceptions))
    (define-key vterm-mode-map (kbd "M-e") #'vterm-copy-mode)
    (define-key vterm-mode-map (kbd "M-i") #'eat-special-edit-open))

  (leaf pdf-tools
    :mode ("\\.pdf\\'" . pdf-view-mode)
    :config
    (pdf-tools-install :no-query))

  (leaf nov
    :mode ("\\.epub\\'" . nov-mode))

  (leaf reader
    :load-path "~/.emacs.d/lisp"
    :require t
    :custom
    ((reader-frame-parameters . '((width . 90) (height . 50)))
     (reader-pdf-default-fit . 'fit-page)
     (reader-pdf-dual-page . nil)
     (reader-pdf-right-to-left . nil)
     (reader-pdf-dual-odd-left . t))
    :bind
    (("<f4>" . hydra-reader/body))
    :hydra
    ((hydra-reader
      (:hint nil :exit nil)
      "
^Navigate^          ^View^           ^Layout^         ^Frame^
^^--------------------------------------------------------------
_n_: next page      _+_: zoom in     _d_: dual page   _o_: open
_p_: prev page      _-_: zoom out    _r_: direction   _q_: close
_j_: scroll down    _w_: fit width   _e_: odd/even
_k_: scroll up      _f_: fit page
_g_: goto page
"
      ("n" reader-next-page)
      ("p" reader-previous-page)
      ("j" reader-scroll-up)
      ("k" reader-scroll-down)
      ("g" reader-goto-page :exit t)
      ("+" reader-zoom-in)
      ("-" reader-zoom-out)
      ("w" reader-fit-width)
      ("f" reader-fit-page)
      ("d" reader-toggle-dual-page)
      ("r" reader-toggle-direction)
      ("e" reader-toggle-odd-even)
      ("o" reader-open :exit t)
      ("q" reader-close :exit t)
      ("C-m" nil :exit t))))

  (leaf eat-special-edit
    :load-path "~/.emacs.d/lisp"
    :require t
    :custom
    ((eat-special-edit-major-mode . 'org-mode)
     (eat-special-edit-use-default-template . t)
     (eat-special-edit-default-template . "default")
     (eat-special-edit-templates
      . '((:name "default"
                 :template "* 要求\n** 背景\n\n** 要求\n\n* 要件\n\n* ワークフロー\n1. 現状の調査\n2. 妥当性の確認\n3. 実装について不足の情報について私に問う\n4. ブランチの作成、チェックアウト(現在のワークスペースがworktreeなら不要)\n5. プランの作成\n   ここで調整\n6. 実行\n7. コミット\n   マージやプッシュは手動または別途指示を出すためしないこと\n")
          (:name "adjust"
                 :template "* 対象\n\n* 変更内容\n\n* 補足\n")))))

  (leaf org-timeblock
    :load-path "~/.emacs.d/lisp"
    :require t
    :custom
    `((org-timeblock-worklog-break-title . "休憩")
      (org-timeblock-worklog-other-category . ,(or (getenv "TB_OTHER") "その他")))
    :config
    (org-timeblock-setup)
    (add-to-list 'display-buffer-alist
                 '("\\*Org Timeblock\\*"
                   (display-buffer-in-side-window)
                   (side . left)
                   (slot . 1)
                   (window-width . 28)
                   (preserve-size . (t . nil))
                   (window-parameters . ((no-delete-other-windows . t)))))
    :bind
    (("<f3>" . org-timeblock-toggle)))

  (leaf sidebar-utils
    :load-path "~/.emacs.d/lisp"
    :require t
    :config
    (sidebar-utils-define
     :id 'left-panel
     :protected t
     :components
     (list (list :id 'persp-sidebar
                 :buffer "*Persp Utils Sidebar*"
                 :show-fn #'persp-utils-sidebar--ensure-displayed
                 :hide-fn #'persp-utils-sidebar-close
                 :ratio 1/3)
           (list :id 'org-timeblock
                 :buffer "*Org Timeblock*"
                 :show-fn #'org-timeblock-show
                 :hide-fn #'org-timeblock-close
                 :ratio 2/3)))
    (sidebar-utils-mode 1))

  (leaf eshell
    :tag "builtin"
    :custom
    ((eshell-history-size . 1024)
     (eshell-buffer-maximum-lines . 10000)
     (eshell-hist-ignoredups . t)
     (eshell-destroy-buffer-when-process-dies . t))
    :hook
    ((eshell-mode-hook . my/eshell-setup-capf)
     (eshell-mode-hook . my/eshell-setup-aliases)
     (eshell-mode-hook . my/eshell-setup-keybinds))
    :config
    ;; ACM capf backend for eshell
    (add-to-list 'acm-backend-capf-mode-list 'eshell-mode)

    ;; capf: pcomplete (eshell default) + cape-file
    (defun my/eshell-setup-capf ()
      (setq-local completion-at-point-functions
                  (list #'pcomplete-completions-at-point
                        #'cape-file)))

    ;; consult-history on M-r
    (defun my/eshell-setup-keybinds ()
      (keymap-set eshell-hist-mode-map "M-r" #'consult-history))

    ;; Aliases (fish abbr equivalent)
    (defun my/eshell-setup-aliases ()
      (eshell/alias "ls" "eza $*")
      (eshell/alias "cat" "bat $*")
      (eshell/alias "grep" "rg $*")
      (eshell/alias "h" "cd $1")
      (eshell/alias "q" "exit"))

    ;; Override eshell/rm to use trash-put
    (defun eshell/rm (&rest args)
      "Use trash-put instead of rm."
      (let ((args (flatten-tree args)))
        (dolist (arg args)
          (eshell-command-result (format "trash-put %s" (eshell-quote-argument arg))))))

    ;; Environment variables (same as fish interactiveShellInit)
    (let ((python-home (string-trim (shell-command-to-string
                                     "python3 -c 'import sys; print(sys.prefix, end=\"\")' 2>/dev/null"))))
      (when (and (not (string-empty-p python-home))
                 (file-directory-p python-home))
        (setenv "PYTHON_HOME" python-home)))
    (let ((gh-token (string-trim (shell-command-to-string "gh auth token 2>/dev/null"))))
      (when (and (not (string-empty-p gh-token))
                 (not (string-match-p "error" gh-token)))
        (setenv "GITHUB_PERSONAL_ACCESS_TOKEN" gh-token)))
    (setenv "HM_USERNAME" (user-login-name))
    (setenv "HM_GIT_EMAIL"
            (string-trim (shell-command-to-string "git config user.email")))

    ;; PATH additions
    (dolist (dir '("~/.local/bin" "/opt/homebrew/bin" "~/.rd/bin"))
      (let ((expanded (expand-file-name dir)))
        (when (file-directory-p expanded)
          (setenv "PATH" (concat expanded ":" (getenv "PATH")))
          (add-to-list 'exec-path expanded))))

    ;; Pure-style two-line prompt
    (defun my/eshell-prompt ()
      "Pure-style two-line prompt."
      (let* ((dir (abbreviate-file-name (eshell/pwd)))
             (branch (when (executable-find "git")
                       (string-trim (shell-command-to-string
                                     "git symbolic-ref --short HEAD 2>/dev/null"))))
             (nix-shell (getenv "IN_NIX_SHELL"))
             (aws-profile (getenv "AWS_PROFILE"))
             (venv (getenv "VIRTUAL_ENV"))
             (indicators
              (string-join
               (delq nil
                     (list
                      (when nix-shell "nix")
                      (when aws-profile (format "aws:%s" aws-profile))
                      (when venv (format "py:%s" (file-name-nondirectory venv)))))
               " ")))
        (concat
         (propertize dir 'face 'font-lock-constant-face)
         (when (and branch (not (string-empty-p branch)))
           (concat " " (propertize branch 'face 'font-lock-function-name-face)))
         "\n"
         (unless (string-empty-p indicators)
           (concat (propertize (format "[%s] " indicators) 'face 'font-lock-comment-face)))
         (propertize "❯" 'face (if (= (user-uid) 0) 'error 'success))
         " ")))

    (setq eshell-prompt-function #'my/eshell-prompt)
    (setq eshell-prompt-regexp "^[^❯]*❯ "))

  (leaf eshell-syntax-highlighting
    :url "https://github.com/akreisher/eshell-syntax-highlighting"
    :hook
    ((eshell-mode-hook . eshell-syntax-highlighting-mode)))

  (leaf eat
    :url "https://codeberg.org/akib/emacs-eat"
    :hook
    ((eshell-load-hook . eat-eshell-mode))
    :custom
    ((eat-enable-auto-line-mode . t))
    :config
    (customize-set-variable
     'eat-semi-char-non-bound-keys
     (append
      (list (vector meta-prefix-char ?e) (vector meta-prefix-char ?o)
            (vector meta-prefix-char ?j) (vector meta-prefix-char ?k)
            (vector meta-prefix-char ?m) (vector meta-prefix-char ?i)
            (vector meta-prefix-char ?c))
      eat-semi-char-non-bound-keys))
    :bind
    (eat-mode-map
     ("M-e" . eat-toggle-mode)
     ("M-i" . eat-special-edit-open)))

  (leaf jinx
    :url "https://github.com/minad/jinx"
    :global-minor-mode global-jinx-mode
    :custom
    ((jinx-languages . "en_US")
     (jinx-exclude-regexps . '((emacs-lisp-mode "Package-Requires:.*$")
                               (t
                                "[ぁ-んァ-ヶ一-龠ー]+[a-zA-Z]+"
                                "[a-zA-Z]+[ぁ-んァ-ヶ一-龠ー]+"
                                "[ぁ-んァ-ヶ一-龠ー]+"
                                "[A-Z]+\\>" "-+\\>"
                                "\\w*?[0-9]\\w*\\>"
                                "[a-z]+://\\S-+"
                                "<?[-+_.~a-zA-Z][-+_.~:a-zA-Z0-9]*@[-.a-zA-Z0-9]+>?"
                                "\\(?:Local Variables\\|End\\):\\s-*$"
                                "jinx-\\(?:languages\\|local-words\\):\\s-+.*$"))))
    :bind
    (("M-$" . jinx-correct)
     ("C-M-$" . jinx-languages)))

  (leaf dirvish
    :url "https://github.com/alexluigit/dirvish"
    :init
    (dirvish-override-dired-mode)
    :bind
    (("C-x d" . dirvish)))

  (leaf ace-window
    :url "https://github.com/abo-abo/ace-window"
    :custom
    ((aw-keys . '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
     (aw-background . nil))
    :bind
    (("M-o" . ace-window)))

  (leaf ddskk
    :url "https://github.com/skk-dev/ddskk"
    ;; 会社環境(macOS)でのみ使用。個人環境(vanilla)ではfcitx5を使用
    :if (not is-private-host)
    :custom
    ((skk-preload . t)
     (skk-user-directory . "~/.ddskk")
     (skk-init-file . "~/.ddskk/init")
     (skk-large-jisyo . "~/.ddskk/SKK-JISYO.L"))
    :bind
    (("C-x C-j" . skk-mode)))

  (leaf apheleia
    :url "https://github.com/radian-software/apheleia"
    :require t
    :global-minor-mode apheleia-global-mode
    :config
    ;; go
    (setf (alist-get 'goimports apheleia-formatters) '("," "goimports"))
    (setf (alist-get 'go-ts-mode apheleia-mode-alist) 'goimports)
    ;; python
    (setf (alist-get 'ruff apheleia-formatters) '("ruff" "format" "--stdin-filename" filepath "-"))
    (setf (alist-get 'python-ts-mode apheleia-mode-alist) 'ruff)
    ;; nix
    (setf (alist-get 'nixfmt apheleia-formatters) '("nixfmt"))
    (setf (alist-get 'nix-ts-mode apheleia-mode-alist) 'nixfmt)
    ;; rust
    (setf (alist-get 'rustfmt apheleia-formatters) '("rustfmt"))
    (setf (alist-get 'rust-ts-mode apheleia-mode-alist) 'rustfmt)
    ;; bash
    (setf (alist-get 'shfmt apheleia-formatters) '("shfmt" "-i" "2" "-"))
    (setf (alist-get 'bash-ts-mode apheleia-mode-alist) 'shfmt)
    ;; lua
    (setf (alist-get 'stylua apheleia-formatters) '("stylua" "--search-parent-directories" "-"))
    (setf (alist-get 'lua-ts-mode apheleia-mode-alist) 'stylua)
    ;; java
    (setf (alist-get 'google-java-format apheleia-formatters) '("google-java-format" "--aosp" "-"))
    (setf (alist-get 'java-ts-mode apheleia-mode-alist) 'google-java-format)
    ;; kotlin
    (setf (alist-get 'ktlint apheleia-formatters) '("ktlint" "--format" "--log-level=none" "--stdin" "--stdin-path" filepath))
    (setf (alist-get 'kotlin-ts-mode apheleia-mode-alist) 'ktlint)
    ;; prettier (組み込み定義済み、mode-alistのみ)
    (setf (alist-get 'js-ts-mode apheleia-mode-alist) 'prettier)
    (setf (alist-get 'typescript-ts-mode apheleia-mode-alist) 'prettier)
    (setf (alist-get 'tsx-ts-mode apheleia-mode-alist) 'prettier)
    (setf (alist-get 'css-ts-mode apheleia-mode-alist) 'prettier)
    (setf (alist-get 'html-ts-mode apheleia-mode-alist) 'prettier)
    (setf (alist-get 'json-ts-mode apheleia-mode-alist) 'prettier)
    (setf (alist-get 'yaml-ts-mode apheleia-mode-alist) 'prettier)
    ;; toml
    (setf (alist-get 'taplo apheleia-formatters) '("taplo" "fmt" "-"))
    (setf (alist-get 'toml-ts-mode apheleia-mode-alist) 'taplo))

  (leaf exec-path-from-shell
    :init
    (when (memq window-system '(mac ns x))
      (exec-path-from-shell-initialize))))

(leaf *appearance
  :config
  (leaf doom-modeline
    :url "https://github.com/seagle0128/doom-modeline"
    :global-minor-mode t
    :custom
    ((doom-modeline-battery . t)
     (doom-modeline-time . t))
    :config
    (leaf nerd-icons
      :url "https://github.com/rainstormstudio/nerd-icons.el"))

  (leaf catppuccin-theme
    :url "https://github.com/catppuccin/emacs"
    :custom
    ((catppuccin-flavor . 'latte))
    :config
    (load-theme 'catppuccin :no-confirm)
    (set-cursor-color (catppuccin-get-color 'pink)))

  (leaf org-modern
    :url "https://github.com/minad/org-modern"
    :global-minor-mode global-org-modern-mode
    :custom
    ((org-auto-align-tags . nil)
     (org-tags-column . 0)
     (org-fold-catch-invisible-edits . 'show-and-error)
     (org-special-ctrl-a/e . t)
     (org-insert-heading-respect-content . t)
     (org-hide-emphasis-markers . t)
     (org-pretty-entities . t)
     (org-ellipsis . "…")
     (org-agenda-tags-column . 0)
     (org-agenda-block-separator . ?─)
     (org-agenda-time-grid . '((daily today require-timed)
                               (800 1000 1200 1400 1600 1800 2000)
                               " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"))
     (org-agenda-current-time-string . "◀── now ─────────────────────────────────────────────────")))

  (leaf nyan-mode
    :url "https://github.com/TeMPOraL/nyan-mode"
    :global-minor-mode t
    :custom
    ((nyan-wavy-trail . t)
     (nyan-animate-nyancat . t)
     (nyan-bar-length . 16)
     (nyan-minimub-window-width . 80)))

  (leaf parrot
    :url "https://github.com/dp12/parrot"
    :global-minor-mode t
    :custom
    ((parrot-num-rotations . nil)))

  (leaf indent-bars
    :url "https://github.com/jdtsmith/indent-bars"
    :hook
    ((prog-mode-hook . indent-bars-mode))
    :custom
    ((indent-bars-color . `(:face default :blend 0.15))
     (indent-bars-highlight-current-depth . `(:blend 0.5 :face default))
     (indent-bars-treesit-support . t))))

(if is-private-host
    (progn
      (defun playerctl-play ()
        (interactive)
        (shell-command "playerctl play"))

      (defun playerctl-pause ()
        (interactive)
        (shell-command "playerctl pause"))

      (defun playerctl-play-pause ()
        (interactive)
        (shell-command "playerctl play-pause"))

      (defun playerctl-next ()
        (interactive)
        (shell-command "playerctl next"))

      (defun playerctl-previous ()
        (interactive)
        (shell-command "playerctl previous"))

      (defun libpython-init ()
        (interactive)
        (let ((python-home (shell-command-to-string "python -c 'import sys; print(sys.prefix, end=\"\")'")))
          (insert (format "(initialize! :python-home \"%s\")" python-home))))

      (defvar my/idle-agenda-seconds 600
        "Seconds of idle time before showing org-agenda via Claude.")

      (defvar my/idle-agenda-process nil
        "Running Claude process for idle agenda.")

      (defvar my/idle-agenda-prompt
        "あなたはEmacsを emacsclient -e 経由で操作するエージェントです。
org-agendaの \"g\" カスタムビューを適切なウィンドウに表示してください。

手順:
1. ウィンドウレイアウトを取得:
   emacsclient -e '(mapcar (lambda (w) (list (buffer-name (window-buffer w)) (window-edges w) (window-dedicated-p w))) (window-list))'

2. 結果を分析し「ワークスペース」ウィンドウを特定:
   - window-dedicated-p が非nil のウィンドウを全て除外（サイドバー、Claude、その他dedicated）
   - ミニバッファ（ *Minibuf- で始まるもの）を除外
   - 残り（dedicated=nil）の中で面積 (right-left)*(bottom-top) が最大のウィンドウを選択
   - 注: eat ターミナルはワークスペースで利用されるため dedicated=nil。除外しないこと
   - 該当なしなら操作不要。doneと出力して終了

3. 特定したバッファ名 BUFFER を使って実行:
   emacsclient -e '(let ((w (get-buffer-window \"BUFFER\"))) (when w (with-selected-window w (let ((org-agenda-window-setup (quote current-window))) (org-agenda nil \"g\")))))'
   BUFFERは特定したバッファ名に置換。

完了したら done とだけ出力。エラー時はエラー内容を出力。")

      (defun my/show-idle-agenda ()
        "Call Claude Code headlessly to show org-agenda in workspace window."
        (when (and (not (process-live-p my/idle-agenda-process))
                   (not (minibufferp)))
          (let ((check-proc (start-process "claude-agenda-check" nil
                                           "claude" "-p" "ok" "--max-turns" "1" "--model" "haiku")))
            (set-process-sentinel
             check-proc
             (lambda (proc event)
               (when (and (string-match-p "finished" event)
                          (zerop (process-exit-status proc)))
                 (setq my/idle-agenda-process
                       (start-process "claude-idle-agenda" "*claude-idle-agenda*"
                                      "claude" "-p" my/idle-agenda-prompt
                                      "--dangerously-skip-permissions"
                                      "--model" "haiku"
                                      "--max-turns" "3"))))))))

      (run-with-idle-timer my/idle-agenda-seconds t #'my/show-idle-agenda)

      t)
  nil)

(require 'server)
(unless (server-running-p)
  (server-start))

(provide 'init)

;;; init.el ends here
