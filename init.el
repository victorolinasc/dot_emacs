;;; init.el --- Summary:

;;; Commentary:
;;; Emacs initialization and configuration

;;; Code:
(package-initialize)

(load-file (expand-file-name "lisp/simple-config.el" user-emacs-directory))
(load-file (expand-file-name "lisp/config-use-package.el" user-emacs-directory))
(load-file (expand-file-name "lisp/asdf.el" user-emacs-directory))

(use-package zenburn-theme
  :init (load-theme 'zenburn t)
  :defer t
  :ensure t)

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package eshell
  :init
  (setq eshell-visual-commands '("vi" "screen" "top" "less" "more" "lynx"
                                 "ncftp" "pine" "tin" "trn" "elm" "vim"
                                 "nmtui" "alsamixer" "htop" "el" "elinks"
                                 ))
  (setq eshell-hist-ignoredups t)
  :config
  (add-hook 'eshell-mode-hook (lambda () (linum-mode -1)))
  (add-hook 'eshell-mode-hook (lambda () (exec-path-from-shell-initialize)))
  (setenv "PAGER" "cat"))

(use-package eshell-prompt-extras
  :ensure t
  :after eshell
  :config
  (autoload 'epe-theme-lambda "eshell-prompt-extras")
  (setq eshell-highlight-prompt nil
        eshell-prompt-function 'epe-theme-lambda))

(use-package diminish
  :ensure t)

(use-package flyspell
  :config
  (setq ispell-program-name "aspell"
        ispell-dictionary "en")
  (add-hook 'text-mode-hook #'flyspell-mode)
  (add-hook 'prog-mode-hook #'flyspell-prog-mode))

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
  ("C-x b" . ivy-switch-buffer)
  ("C-x C-b" . ivy-switch-buffer))

(use-package ivy
  :diminish ivy-mode)

(use-package eldoc
  :diminish eldoc-mode)

(use-package autorevert
  :diminish auto-revert-mode)

(use-package deadgrep
  :ensure t)
 
(use-package smooth-scrolling
  :ensure t
  :config
  (smooth-scrolling-mode 1))

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

(use-package treemacs
  :ensure t
  :defer t
  :config
  (treemacs-filewatch-mode t)
  (treemacs-git-mode 'simple)
  (treemacs-resize-icons 22)
  (add-hook 'treemacs-mode-hook (lambda() (display-line-numbers-mode -1))))

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :after (treemacs dired)
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after (treemacs magit)
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

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :defer t)

(use-package lsp-mode
  :commands lsp
  :ensure t
  :hook
  (elixir-mode . lsp))

(use-package lsp-ui
  :commands lsp-ui-mode
  :ensure t
  :after (lsp-mode)
  :init
  (setq lsp-ui-doc-enable nil)
  (setq lsp-prefer-flymake nil))

(use-package company-lsp
  :commands company-lsp
  :ensure t
  :after (company)
  :config
  (setq company-lsp-cache-candidates t)
  (push 'company-lsp company-backends))

(defun vn/elixir-ls-reload ()
  "Reloads the path of the elixir language server."
  (interactive)
  (add-to-list 'exec-path "~/dev/tools/elixir-ls/release/erl21"))

(use-package elixir-mode
  :load-path "~/dev/projects/emacs-elixir"
  :init
  (add-to-list 'exec-path "~/dev/tools/elixir-ls/release/erl21")
  (add-hook 'elixir-mode-hook
            (lambda ()
              (push '(">=" . ?\u2265) prettify-symbols-alist)
              (push '("<=" . ?\u2264) prettify-symbols-alist)
              (push '("!=" . ?\u2260) prettify-symbols-alist)
              (push '("==" . ?\u2A75) prettify-symbols-alist)
              (push '("=~" . ?\u2245) prettify-symbols-alist)
              (push '("<-" . ?\u2190) prettify-symbols-alist)
              (push '("->" . ?\u2192) prettify-symbols-alist)
              (push '("<-" . ?\u2190) prettify-symbols-alist)
              (push '("|>" . ?\u25B7) prettify-symbols-alist))))

(use-package reformatter
  :config
  (reformatter-define +elixir-format
    :program "mix"
    :args '("format" "-"))

  (defun +set-default-directory-to-mix-project-root (original-fun &rest args)
    (if-let* ((mix-project-root (and buffer-file-name
                                     (locate-dominating-file buffer-file-name
                                                             ".formatter.exs"))))
        (let ((default-directory mix-project-root))
          (apply original-fun args))
      (apply original-fun args)))
  (advice-add '+elixir-format-region :around #'+set-default-directory-to-mix-project-root)

  (add-hook 'elixir-mode-hook #'+elixir-format-on-save-mode))

(use-package exunit
  :ensure t
  :diminish t
  :bind
  ("C-c e ." . exunit-verify-single)
  ("C-c e b" . exunit-verify)
  ("C-c e u a" . exunit-verify-all-in-umbrella)
  ("C-c e a" . exunit-verify-all)
  ("C-c e l" . exunit-rerun))

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

(use-package web-mode
  :ensure t
  :init
  (setq
   web-mode-markup-indent-offset 2
   web-mode-css-indent-offset 2
   web-mode-code-indent-offset 2)
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.eex?\\'" . web-mode))
  )

(use-package ox-gfm
  :ensure t)

(use-package ox-spectacle
  :ensure t)

(use-package plantuml-mode
  :ensure t
  :init
  (setq plantuml-jar-path (expand-file-name "vendor/plantuml.1.2019.7.jar" user-emacs-directory))
  (setq plantuml-default-exec-mode "jar")
  :mode "\\.uml\\'")

(use-package org
  :bind (("C-c a" . org-agenda))
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
  (setq org-plantuml-jar-path
      (expand-file-name "vendor/plantuml.1.2019.7.jar" user-emacs-directory))
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (elixir . t)
     (org . t)
     (java . t)
     (ditaa . t)
     (plantuml . t))))

(use-package org-projectile
  :bind (("C-c n p" . org-projectile-project-todo-completing-read)
         ("C-c c" . org-capture))
  :config
  (progn
    (setq org-projectile-projects-file (concat user-emacs-directory "org/projects-todo.org"))
    (setq org-agenda-files (append org-agenda-files (org-projectile-todo-files)))
    (push (org-projectile-project-todo-entry) org-capture-templates))
  :ensure t)

(use-package docker
  :ensure t
  :bind ("C-c d" . docker))

(use-package kubernetes
  :ensure t
  :commands (kubernetes-overview))

(provide 'init)
;;; init.el ends here
