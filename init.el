;;; init.el --- Summary:

;;; Commentary:
;;; Emacs initialization and configuration

;;; Code:
(package-initialize)

(load-file "~/.emacs.d/lisp/simple-config.el")
(load-file "~/.emacs.d/lisp/config-use-package.el")

(use-package vc
  :ensure t)

(use-package restclient
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
  (setq org-reveal-root "file:///~/.emacs.d/vendor/reveal.js"))

(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode))

(use-package alchemist
  :ensure t
  :bind ("C-c C-r" . alchemist-mix-compile))

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

;; ATTENTION: do not ask for confirmation for elixir sources
(defun do-not-ask-for-confirmation-for-elixir-evaluate (lang body)
  "Evaluate LANG BODY without confirmation."
  (not (string= lang "elixir")))

(use-package org
  :ensure t
  :init
  (setq org-hide-leading-stars t
	org-list-allow-alphabetical t
	org-src-fontify-natively t  ;; you want this to activate coloring in blocks
	org-src-tab-acts-natively t ;; you want this to have completion in blocks
	org-hide-emphasis-markers t ;; to hide the *,=, or / markers
	org-pretty-entities t       ;; to have \alpha, \to and others display as utf8
	org-ditaa-jar-path "~/.emacs.d/vendor/ditaa0_9.jar"
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
;;; init.el ends here

