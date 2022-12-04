;;; simple-config --- Summary:

;;; Commentary:
;;; Simple configuration of Emacs built in functions

;;; Code:
(setq custom-file (expand-file-name "lisp/custom.el" user-emacs-directory))
(load custom-file 'noerror)

;; Introduce myself
(setq user-full-name "Victor Oliveira Nascimento")

;; Always load newest byte code
(setq load-prefer-newer t)

;; Higher garbage collector
(setq gc-cons-threshold (* 20 1024 1024))
;; Optimization for LSP
(setq read-process-output-max (* 1024 1024))

;; Disable bi-directional text
(setq-default bidi-paragraph-direction 'left-to-right)

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
(tool-bar-mode -1)
(scroll-bar-mode -1)

(set-face-attribute 'default nil :font "Fira Code" :height 170)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Fira Code" :height 170)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Droid Sans" :height 170 :weight 'regular)

;; Better ease on reading lines
(setq line-spacing 0.1)

(defun vn/customize-frame ()
  ;; Ensure cursor has the same color when run on daemon or not
  (set-cursor-color "DeepSkyBlue"))

(if (daemonp)
    (add-hook 'server-after-make-frame-hook #'vn/customize-frame)
    (vn/customize-frame))

;; Custom key bindings
(global-set-key (kbd "<f5>") 'treemacs)
(global-set-key (kbd "<f6>") 'flymake-show-buffer-diagnostics)
(global-set-key (kbd "<f7>") 'eshell)
(global-set-key (kbd "C-o") 'other-window)
(global-set-key (kbd "s-SPC") 'switch-to-buffer)
(global-set-key (kbd "C-q") 'kill-buffer)

(unbind-key "C-z")

;; Enable prettify symbols
(global-prettify-symbols-mode)

;; Emacs 28: Hide commands in M-x which do not work in the current mode.
;; Vertico commands are hidden in normal buffers.
(setq read-extended-command-predicate #'command-completion-default-include-p)

(load-file (expand-file-name "lisp/vn.el" user-emacs-directory))
;;; simple-config.el ends here
