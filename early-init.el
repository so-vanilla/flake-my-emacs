;;; early-init.el --- My early-init.el

;; Author: Shuto Omura <somura-vanilla@so-icecream.com>

;; Commentary:

;;; Code:

(setq package-enable-at-startup nil)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(provide 'early-init)

;;; early-init.el ends here
