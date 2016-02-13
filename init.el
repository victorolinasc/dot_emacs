;;; init.el --- Summary:

;;; Commentary:
;;; Emacs initialization and configuration

;;; Code:

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(load-file "~/.emacs.d/lisp/simple-config.el")
(load-file "~/.emacs.d/lisp/config-use-package.el")

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

(use-package elixir-mode
  :ensure t)

(use-package alchemist
  :ensure t
  :bind ("C-c C-r" . alchemist-mix-compile))

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

