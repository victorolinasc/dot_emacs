;;; init.el --- Summary:

;;; Commentary:
;;; Emacs initialization and configuration

;;; Code:
(load-file (expand-file-name "lisp/config-use-package.el" user-emacs-directory))
(load-file (expand-file-name "lisp/simple-config.el" user-emacs-directory))

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-copy-env "SSH_AUTH_SOCK")
  (exec-path-from-shell-initialize))

(use-package eshell
  :ensure nil
  :init
  (setq eshell-visual-commands '("vi" "screen" "top" "less" "more" "lynx"
                                 "ncftp" "pine" "tin" "trn" "elm" "vim"
                                 "nmtui" "alsamixer" "htop" "el" "elinks"
                                 ))
  (setq eshell-hist-ignoredups t)
  :config
  (add-hook 'eshell-mode-hook (lambda () (exec-path-from-shell-initialize)))
  (setenv "PAGER" "cat"))

(use-package eshell-prompt-extras
    :after eshell
  :config
  (autoload 'epe-theme-lambda "eshell-prompt-extras")
  (setq eshell-highlight-prompt nil
        eshell-prompt-function 'epe-theme-lambda))

(use-package diminish)

(use-package vterm
  :config
  (add-hook 'vterm-mode-hook
          (lambda ()
            (display-line-numbers-mode -1)))
  :bind
  ("C-o" . other-window))

(use-package multi-vterm
  :bind
  ("<f2>" . multi-vterm-project))

(use-package flyspell
  :ensure nil
  :config
  (setq ispell-program-name "aspell"
        ispell-dictionary "en")
  (add-hook 'text-mode-hook #'flyspell-mode)
  (add-hook 'prog-mode-hook #'flyspell-prog-mode))

(use-package helpful
  :bind
  ("C-h f" . helpful-callable)
  ("C-h v" . helpful-variable)
  ("C-h k" . helpful-key)
  ("C-h F" . helpful-function)
  ("C-h C" . helpful-command))

(use-package which-key
  :config
  (which-key-mode))

(use-package counsel
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

(use-package ivy  :diminish ivy-mode)

(use-package prescient)
(use-package ivy-prescient :after (counsel))
(use-package company-prescient :after (company))

(use-package eldoc :ensure nil  :diminish eldoc-mode)

(use-package autorevert :ensure nil  :diminish auto-revert-mode)

(use-package deadgrep
  :commands deadgrep
  :bind
  ("C-S-f" . deadgrep))
 
(use-package smooth-scrolling
  :config
  (smooth-scrolling-mode 1))

(use-package dired
  :ensure nil
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
  :config
  (move-text-default-bindings))

(use-package smartparens
  :diminish smartparens-mode
  :config
  (require 'smartparens-config)
  (smartparens-global-mode)
  (show-smartparens-global-mode))

(use-package rainbow-mode :diminish rainbow-mode)

(use-package markdown-mode :mode ("\\.md\\'" . gfm-mode))

(use-package projectile
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (setq projectile-completion-system 'ivy)
  (projectile-mode +1))

(use-package treemacs
  :defer t
  :config
  (treemacs-filewatch-mode t)
  (treemacs-git-mode 'simple)
  (treemacs-resize-icons 22)
  (add-hook 'treemacs-mode-hook (lambda() (display-line-numbers-mode -1))))

(use-package treemacs-projectile  :after (treemacs projectile))

(use-package treemacs-icons-dired
  :after (treemacs dired)
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit :after (treemacs magit))

(use-package magit :bind ("C-c m s" . magit-status))

(use-package company
  :diminish company-mode
  :bind ("M-/" . company-complete)
  :defer t
  :config
  (global-company-mode))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package restclient :defer t)

(use-package ob-elixir :defer t)

(use-package flycheck
  :config
  (global-flycheck-mode))

(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (yas-global-mode 1))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (add-to-list 'exec-path "/home/victorolinasc/Projects/elixir-ls/release/erl23/")
  :config
  (setq lsp-file-watch-threshold 2000)
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.elixir_ls\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\deps\\'")
  :hook
  (elixir-mode . lsp))

(use-package lsp-ui
  :commands lsp-ui-mode
  :after (lsp-mode)
  :init
  (setq lsp-ui-doc-enable nil)
  (setq lsp-prefer-flymake nil))

(use-package elixir-mode
  :init
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
  :diminish t
  :bind
  ("C-c e ." . exunit-verify-single)
  ("C-c e b" . exunit-verify)
  ("C-c e u a" . exunit-verify-all-in-umbrella)
  ("C-c e a" . exunit-verify-all)
  ("C-c e l" . exunit-rerun))

;; On a clean build, we need to call 'all-the-icons-install-fonts' function
(use-package all-the-icons :defer t)

(use-package erlang :defer t)

(use-package multiple-cursors
  :bind
  ("C->" . 'mc/mark-next-like-this)
  ("C-<" . 'mc/mark-previous-like-this))

(use-package yaml-mode)

;; JavaScript configuration
(use-package js2-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))

(use-package editorconfig
  :diminish editorconfig-mode
  :config
  (editorconfig-mode 1))

(use-package pass :defer t)

(use-package web-mode
  :init
  (setq
   web-mode-markup-indent-offset 2
   web-mode-css-indent-offset 2
   web-mode-code-indent-offset 2)
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.eex?\\'" . web-mode))
  )

(use-package ox-gfm)

(use-package ox-spectacle)

(use-package plantuml-mode
  :init
  (setq plantuml-jar-path (expand-file-name "vendor/plantuml.1.2019.7.jar" user-emacs-directory))
  (setq plantuml-default-exec-mode "jar")
  :mode "\\.uml\\'")

(use-package org
  :bind (("C-c a" . org-agenda))
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
  )

(use-package docker :bind ("C-c d" . docker))

(use-package kubernetes :commands (kubernetes-overview))

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t 
        doom-themes-enable-italic t)
  (load-theme 'doom-vibrant t)
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config))

(use-package doom-modeline
  :init (doom-modeline-mode 1))


(provide 'init)
;;; init.el ends here
