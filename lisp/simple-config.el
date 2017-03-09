;;; olinasc-simple --- Summary:

;;; Commentary:
;;; Simple configuration of Emacs built in functions

;;; Code:

(load "~/.emacs.d/lisp/custom.el")
(load "~/.emacs.d/lisp/custom-eshell.el")

;; Init in full frame
(custom-set-variables
 '(initial-frame-alist (quote ((fullscreen . maximized)))))

;; Backup settings
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))

;; No tabs by default
(setq-default indent-tabs-mode nil)

;; line and column number
(global-linum-mode t)
(column-number-mode 1)

;; Set default font-size
(set-face-attribute 'default nil :height 160)
;; Font resizing keybinding
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
;; C-x C-0 restores the default font size

;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; Activates delete selection mode
(delete-selection-mode 1)

;; inhibit init buffer
(setq inhibit-startup-screen t)

;; hide menus
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

;; global git commit mode
(global-git-commit-mode)

;; Dired config
(setq dired-listing-switches "-laGh1v --group-directories-first")


;;; simple-config.el ends here
