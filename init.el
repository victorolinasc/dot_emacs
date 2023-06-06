;;; init.el --- Summary:

;;; Commentary:
;;; Emacs initialization and configuration

;;; Code:
(load-file (expand-file-name "lisp/core-emacs-config.el" user-emacs-directory))
(load-file (expand-file-name "lisp/core-emacs-packages-config.el" user-emacs-directory))
(load-file (expand-file-name "lisp/external-packages-config.el" user-emacs-directory))

(provide 'init)
;;; init.el ends here
