;;; config-use-package.el --- Summary:

;;; Commentary:
;;; Setup archives and ensure we have use-package loaded

;;; Code:

(require 'package)
(setq package-enable-at-startup nil)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

(dolist (package '(use-package diminish))
  (unless (package-installed-p package)
    (package-install package))
  (require package))

(require 'bind-key)

(provide 'config-use-package)
;;; config-use-package.el ends here
