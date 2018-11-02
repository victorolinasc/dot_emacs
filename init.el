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

(use-package eshell
  :config
  (add-hook 'eshell-mode-hook (lambda () (linum-mode -1)))
  (add-hook 'eshell-mode-hook (lambda () (exec-path-from-shell-initialize)))
  )

(use-package diminish
  :ensure t)

(use-package feature-mode
  :ensure t)

(use-package helpful
  :ensure t
  :bind
  ("C-h f" . helpful-callable)
  ("C-h v" . helpful-variable)
  ("C-h k" . helpful-key)
  ("C-h F" . helpful-function)
  ("C-h C" . helpful-command))

(use-package counsel
  :ensure t
  :diminish t
  :config
  (setq ivy-use-virtual-buffers t
        confirm-nonexistent-file-or-buffer t
        ivy-count-format "(%d/%d) ")
  (define-key ivy-minibuffer-map (kbd "C-j") #'ivy-immediate-done)
  (define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done)
  :init
  (ivy-mode 1)
  :bind
  ("C-c C-r" . ivy-resume)
  ("C-x C-f" . counsel-find-file)
  ("C-s" . swiper)
  ("C-x b" . ivy-switch-buffer))

(use-package ivy
  :diminish t)

(use-package ripgrep
  :ensure t)

(use-package dired
  :defer t
  :config
  (setq
   dired-auto-revert-buffer t           ; Revert on re-visiting
   ;; Better dired flags: `-l' is mandatory, `-a' shows all files, `-h' uses
   ;; human-readable sizes, and `-F' appends file-type classifiers to file names
   ;; (for better highlighting)
   dired-listing-switches "-laFGh1v --group-directories-first"
   dired-ls-F-marks-symlinks t          ; -F marks links with @
   ;; Inhibit prompts for simple recursive operations
   dired-recursive-copies 'always
   ;; Auto-copy to other Dired split window
   dired-dwim-target t))

(use-package move-text
  :ensure t
  :config
  (move-text-default-bindings))

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

(use-package rainbow-mode
  :diminish rainbow-mode
  :ensure t)

(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . gfm-mode))

(use-package projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (setq projectile-completion-system 'ivy)
  (projectile-mode +1))

(use-package window-numbering
  :ensure t
  :config
  (window-numbering-mode))

(use-package treemacs
  :ensure t
  :defer t
  :config
  (progn
    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null (executable-find "python3"))))
      (`(t . t)
       (treemacs-git-mode 'extended))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind ("C-x t t"   . treemacs))

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

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

(use-package restclient
  :defer t
  :ensure t)

(use-package ob-elixir
  :ensure t
  :defer t)

(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode))

(use-package company-quickhelp
  :after (company)
  :ensure t
  :init
  (setq company-quickhelp-delay 1)
  :config
  (company-quickhelp-mode))

(use-package lsp-mode
  :init (setq lsp-inhibit-message t
              lsp-eldoc-render-all nil
              lsp-enable-eldoc nil)
  :ensure t)

(use-package lsp-ui
  :ensure t
  :after (lsp-mode)
  :init
  (setq lsp-ui-doc-enable nil))

(use-package company-lsp
  :ensure t
  :after (company)
  :config
  (add-hook 'java-mode-hook (lambda () (push 'company-lsp company-backends)))
  (setq company-lsp-enable-snippet t
        company-lsp-cache-candidates t)
  (push 'company-lsp company-backends))

(use-package alchemist
  :after (elixir-mode)
  :load-path "../.alchemist" ; Must first clone https://github.com/Trevoke/alchemist.el ont ~/.alchemist
  :config
  (require 'alchemist-elixir-ls)
  (require 'alchemist-goto)
  (require 'alchemist)
  :init
  (setq alchemist-goto-erlang-source-dir
        (expand-file-name "otp_src" (grab-asdf-plugin-version-path "erlang"))
        alchemist-goto-elixir-source-dir (grab-asdf-plugin-version-path "elixir")))

(use-package elixir-mode
  :load-path "~/dev/projects/emacs-elixir"
  :init
  (add-hook 'elixir-mode-hook
          (lambda () (add-hook 'before-save-hook 'elixir-format 'local)))
  (add-hook 'elixir-format-hook
            (lambda ()
              (if (projectile-project-p)
                  (setq elixir-format-arguments
                        (list "--dot-formatter"
                              (concat (locate-dominating-file buffer-file-name ".formatter.exs") ".formatter.exs")))
                (setq elixir-format-arguments nil))
              )))

(use-package tabbar
  :ensure t
  :init
  (tabbar-mode)
  :bind
  (("<C-next>" . tabbar-forward-tab)
   ("<C-prior>" . tabbar-backward-tab)))

(use-package yasnippet
  :ensure t
  :defer t)

;; On a clean build, we need to call 'all-the-icons-install-fonts' function
(use-package all-the-icons
  :ensure t
  :defer t)

(use-package erlang
  :defer t
  :ensure t)

(use-package multiple-cursors
  :ensure t
  :bind
  ("C->" . 'mc/mark-next-like-this)
  ("C-<" . 'mc/mark-previous-like-this))

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

(use-package pass
  :ensure t
  :defer t)

(use-package lsp-java
  :ensure t
  :requires (lsp-ui-flycheck lsp-ui-sideline)
  :config
  (add-hook 'java-mode-hook  'lsp-java-enable)
  (add-hook 'java-mode-hook  'company-mode))

(use-package org
  :ensure t
  :init
  (setq org-hide-leading-stars t
        org-list-allow-alphabetical t
        org-src-fontify-natively t  ;; you want this to activate coloring in blocks
        org-src-tab-acts-natively t ;; you want this to have completion in blocks
        org-hide-emphasis-markers t ;; to hide the *,=, or / markers
        org-pretty-entities t       ;; to have \alpha, \to and others display as utf8
        org-export-with-sub-superscripts nil ;; Disable sub and superscripts
        org-ditaa-jar-path (expand-file-name "vendor/ditaa0_9.jar" user-emacs-directory)
        org-confirm-babel-evaluate 'do-not-ask-for-confirmation-for-elixir-evaluate)
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (elixir . t)
     (org . t)
     (java . t)
     (ditaa . t))))

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

(provide 'init)
;;; init.el ends here
