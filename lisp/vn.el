;;; vn.el --- Summary:

;;; Commentary:
;;; My useful functions :)

(defun vn/init-el ()
  "Opens init.el"
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun vn/earyly-init-el ()
  "Opens early-init.el"
  (interactive)
  (find-file "~/.emacs.d/early-init.el"))

(defun vn/core-emacs-config-el ()
  "Opens core-emacs-config.el"
  (interactive)
  (find-file "~/.emacs.d/lisp/core-emacs-config.el"))

(defun vn/core-emacs-packages-config-el ()
  "Opens core-emacs-packages-config.el"
  (interactive)
  (find-file "~/.emacs.d/lisp/core-emacs-packages-config.el"))

(defun vn/external-packages-config-el ()
  "Opens external-packages-config.el"
  (interactive)
  (find-file "~/.emacs.d/lisp/external-packages-config.el"))

;;; vn.el ends here
