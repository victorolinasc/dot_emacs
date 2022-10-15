;;; vn --- Summary:

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

(defun vn/simple-config-el ()
  "Opens early-init.el"
  (interactive)
  (find-file "~/.emacs.d/lisp/simple-config.el"))

;;; simple-config.el ends here
