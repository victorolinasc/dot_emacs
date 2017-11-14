;;; init.el --- Summary:

;;; Commentary:
;;; Emacs initialization and configuration

;;; Code:
(package-initialize)

(load-file (expand-file-name  "lisp/simple-config.el" user-emacs-directory))
(load-file (expand-file-name "lisp/config-use-package.el" user-emacs-directory))

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package vc
  :ensure t)

(use-package window-numbering
  :ensure t
  :config
  (window-numbering-mode))

(use-package spacemacs-theme
  :ensure t
  :init
  (load-theme 'spacemacs-dark))

(use-package spaceline
  :ensure t
  :init
  (setq powerline-height '35)
  :config
  (require 'spaceline-config)
  (spaceline-spacemacs-theme))

(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . gfm-mode))

(use-package flx-ido
  :ensure t
  :init
  ;; disable ido faces to see flx highlights.
  (setq ido-enable-flex-matching t)
  (setq ido-use-faces nil)
  :config
  (ido-mode t)
  (ido-everywhere t)
  (flx-ido-mode t))

(use-package projectile
  :ensure t
  :config
  (projectile-global-mode))

(use-package magit
  :ensure t
  :bind ("C-c m s" . magit-status))

(use-package company
  :ensure t
  :bind ("M-/" . company-complete)
  :defer t
  :config
  (global-company-mode))

(use-package ob-elixir
  :ensure t
  :defer t)

(use-package ox-reveal
  :ensure t
  :init
  (setq org-reveal-root "file:///home/victornascimento/.emacs.d/vendor/reveal.js"))

(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode))

(use-package alchemist
  :ensure t
  :bind ("C-c C-r" . alchemist-mix-compile)
  :init
  (setq alchemist-execute-command (substitute-in-file-name "$HOME/.asdf/shims/elixir"))
  (setq alchemist-compile-command (substitute-in-file-name "$HOME/.asdf/shims/elixirc"))
  (setq alchemist-mix-command (substitute-in-file-name "$HOME/.asdf/shims/mix"))
  (setq alchemist-iex-program-name (substitute-in-file-name "$HOME/.asdf/shims/iex"))

(use-package elixir-mode
  :ensure t
  :init
  (add-hook 'elixir-mode-hook 'alchemist-mode))

(use-package yasnippet
  :ensure t
  :defer t)

(use-package neotree
  :ensure t
  :config
  (global-set-key [f8] 'neotree-toggle))

(use-package elixir-yasnippets
  :ensure t)

(use-package erlang
  :ensure t)

(use-package multiple-cursors
  :ensure t)

(use-package yaml-mode
  :ensure t)

;; JavaScript configuration
(use-package js2-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package org
  :ensure t
  :init
  (setq org-hide-leading-stars t
	org-list-allow-alphabetical t
	org-src-fontify-natively t  ;; you want this to activate coloring in blocks
	org-src-tab-acts-natively t ;; you want this to have completion in blocks
	org-hide-emphasis-markers t ;; to hide the *,=, or / markers
	org-pretty-entities t       ;; to have \alpha, \to and others display as utf8
	org-ditaa-jar-path (expand-file-name "vendor/ditaa0_9.jar" user-emacs-directory)
	org-confirm-babel-evaluate 'do-not-ask-for-confirmation-for-elixir-evaluate
	)
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (sh . t)
     (elixir . t)
     (org . t)
     (java . t)
     (ditaa . t))))

(provide 'init)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default)))
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(package-selected-packages
   (quote
    (exec-path-from-shell yasnippet company markdown-mode spacemacs-theme window-numbering spaceline projectile flx-ido restclient yaml-mode use-package ox-reveal ob-elixir neotree multiple-cursors magit flycheck erlang elixir-yasnippets alchemist))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
