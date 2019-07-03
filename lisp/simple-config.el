;;; simple-config --- Summary:

;;; Commentary:
;;; Simple configuration of Emacs built in functions

;;; Code:
(setq custom-file (expand-file-name "lisp/custom.el" user-emacs-directory))
(load custom-file 'noerror)

;; Init in full frame
(custom-set-variables
 '(initial-frame-alist (quote ((fullscreen . maximized)))))

;; Introduce myself
(setq user-full-name "Victor Oliveira Nascimento")

;; Always load newest byte code
(setq load-prefer-newer t)

;; Initial scratch message
(setq initial-scratch-message "")

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(setq create-lockfiles nil)

;; UTF-8 all the things
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Highlight current line
(global-hl-line-mode +1)

;; No tabs by default
(setq-default indent-tabs-mode nil)

;; Line and column number
(global-display-line-numbers-mode)
(column-number-mode t)
(size-indication-mode t)

;; More useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; Set default font-size
(set-face-attribute 'default nil
                    :family "Source Code Pro" :height 150)
(set-face-attribute 'variable-pitch nil
                    :family "Fira Sans" :height 150 :weight 'regular)

(bind-key "C-c t V" #'variable-pitch-mode)

;; Font resizing keybinding
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
;; C-x C-0 restores the default font size

;; Lines are 100 now
(setq fill-column 100)

;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; Activates delete selection mode
(delete-selection-mode 1)

;; Inhibit init buffer
(setq inhibit-startup-screen t)

;; Opt out from the startup message in the echo area by simply disabling this
;; ridiculously bizarre thing entirely.
(fset 'display-startup-echo-area-message #'ignore)

;; hide menus
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

(bind-key "C-c h b" #'describe-personal-keybindings)

;; Ensure cursor has the same color when run on daemon or not
(require 'frame)
(defun set-cursor-hook (frame)
  "Ensure cursor has the same color when run on FRAME."
  (modify-frame-parameters
   frame (list (cons 'cursor-color "DeepSkyBlue"))))

(add-hook 'after-make-frame-functions 'set-cursor-hook)

;; Maximize on start
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Custom key bindings
(global-set-key (kbd "<f5>") 'treemacs)
(global-set-key (kbd "<f6>") 'flycheck-list-errors)
(global-set-key (kbd "<f7>") 'eshell)

;; Enable prettify symbols
(global-prettify-symbols-mode)

;; Use windmove default key binding
(windmove-default-keybindings)

;;; simple-config.el ends here
