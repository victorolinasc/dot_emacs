;;; init.el --- Summary:  -*- lexical-binding: t; -*-

;;; Commentary:
;;; Emacs initialization and configuration
(add-to-list 'load-path "~/.emacs.d/lisp")

;;; Code:
(load-file (expand-file-name "lisp/core-emacs-config.el" user-emacs-directory))
(load-file (expand-file-name "lisp/core-emacs-packages-config.el" user-emacs-directory))
(load-file (expand-file-name "lisp/external-packages-config.el" user-emacs-directory))

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(provide 'init)
;;; init.el ends here

