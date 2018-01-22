;;; init.el --- Summary:

;;; Commentary:
;;; Emacs initialization and configuration

;;; Code:
(package-initialize)

(load-file (expand-file-name "lisp/simple-config.el" user-emacs-directory))
(load-file (expand-file-name "lisp/config-use-package.el" user-emacs-directory))
(load-file (expand-file-name "lisp/asdf.el" user-emacs-directory))

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package alert
  :ensure t)

(use-package diminish
  :ensure t)

(use-package move-text
  :ensure t
  :config
  (move-text-default-bindings))

(use-package vc
  :ensure t)

(use-package window-numbering
  :ensure t
  :config
  (window-numbering-mode))

(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :config
  (require 'smartparens-config)
  (smartparens-global-mode)
  (show-smartparens-global-mode))

(use-package monokai-theme
  :ensure t
  :init
  (load-theme 'monokai t))

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
  :diminish company-mode
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
  (setq org-reveal-root
        (expand-file-name "vendor/reveal.js" user-emacs-directory)))

(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode))

(use-package alchemist
  :ensure t
  :bind ("C-c C-r" . alchemist-mix-compile)
  :init
  (setq alchemist-goto-erlang-source-dir
        (expand-file-name "otp_src" (grab-asdf-plugin-version-path "erlang"))
        alchemist-goto-elixir-source-dir (grab-asdf-plugin-version-path "elixir")))

(use-package elixir-mode
  :ensure t
  :init
  (add-hook 'elixir-mode-hook 'alchemist-mode)
  (add-hook 'elixir-mode-hook 'flyspell-prog-mode))

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
  :diminish editorconfig-mode
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
        org-confirm-babel-evaluate 'do-not-ask-for-confirmation-for-elixir-evaluate)
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (sh . t)
     (elixir . t)
     (org . t)
     (java . t)
     (ditaa . t))))

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

(provide 'init)
;;; init.el ends here
