;;; init.el --- Summary:

;;; Commentary:
;;; Emacs initialization and configuration

;;; Code:
(load-file (expand-file-name "lisp/simple-config.el" user-emacs-directory))

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-copy-env "SSH_AUTH_SOCK")
  (exec-path-from-shell-initialize))

(use-package diminish)

(use-package vterm
  :config
  (add-hook 'vterm-mode-hook
            (lambda ()
              (display-line-numbers-mode -1)))
  :bind
  (("C-o" . other-window)
   ("<f5>" . treemacs)))

(use-package multi-vterm
  :bind
  ("<f2>" . multi-vterm-project))

(use-package tree-sitter
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs)

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

(defun corfu-enable-in-minibuffer ()
  "Enable Corfu in the minibuffer if `completion-at-point' is bound."
  (when (where-is-internal #'completion-at-point (list (current-local-map)))
    ;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
    (setq-local corfu-echo-delay nil ;; Disable automatic echo and (point)opup
                corfu-popupinfo-delay nil)
    (corfu-mode 1)))

(use-package corfu
  :bind (:map corfu-map
	      ("<escape>". corfu-quit)
	      ("<return>" . corfu-insert)
              ("M-n" . corfu-popupinfo-scroll-up)
              ("M-p" . corfu-popupinfo-scroll-down))
  :custom
  ;; Works with `indent-for-tab-command'. Make sure tab doesn't indent when you
  ;; want to perform completion
  (tab-always-indent 'complete)
  (corfu-auto nil)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.25)

  (corfu-preview-current 'insert)
  (corfu-preselect-first t)
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  (corfu-echo-mode)
  :hook
  (minibuffer-setup . corfu-enable-in-minibuffer))

(use-package cape
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :bind (("M-/" . completion-at-point)     ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p h" . cape-history)
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-symbol)
         ("C-c p a" . cape-abbrev)
         ("C-c p i" . cape-ispell)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ("C-c p \\" . cape-tex)
         ("C-c p _" . cape-tex)
         ("C-c p ^" . cape-tex)
         ("C-c p &" . cape-sgml)
         ("C-c p r" . cape-rfc1345))
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-ispell))

;; Enable vertico
(use-package vertico
  :init
  (vertico-mode))

(use-package consult
  :bind (;; C-c bindings (mode-specific-map)
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("C-s" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s u" . consult-focus-lines)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  :hook (completion-list-mode . consult-preview-at-point-mode))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

(use-package marginalia
  :init
  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

(use-package svg-lib)

(use-package kind-icon
  :after corfu svg-lib
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly  
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
  (setq kind-icon-default-style
        '(:padding -1 :stroke 0 :margin 0 :radius 0 :height 0.4 :scale 1.0)))

(use-package eldoc
  :ensure nil
  :diminish eldoc-mode)

(use-package autorevert
  :ensure nil
  :diminish auto-revert-mode)

(use-package deadgrep
  :commands deadgrep
  :bind
  ("C-S-f" . deadgrep))

(use-package dired
  :ensure nil
  :defer t
  :config
  (setq
   dired-auto-revert-buffer t           ; Revert on re-visiting
   ;; Better dired flags:
   ;; `-l' is mandatory
   ;; `-a' shows all files
   ;; `-h' uses human-readable sizes
   ;; `-F' appends file-type classifiers to file names (for better highlighting)
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
  :bind (:map smartparens-mode-map
              ("C-M-k" . sp-kill-sexp)
              ("C-M-w" . sp-copy-sexp)
              ("M-<delete>" . sp-unwrap-sexp)
              ("M-<backspace>" . sp-backward-unwrap-sexp)
              ("C-<right>" . sp-forward-slurp-sexp)
              ("C-<left>" . sp-forward-barf-sexp)
              ("C-M-<left>" . sp-backward-slurp-sexp)
              ("C-M-<right>" . sp-backward-barf-sexp)
              ("M-D" . sp-splice-sexp)
              ("C-M-<delete>" . sp-splice-sexp-killing-forward)
              ("C-M-<backspace>" . sp-splice-sexp-killing-backward)
              ("C-S-<backspace>" . sp-splice-sexp-killing-around))
  :diminish smartparens-mode
  :hook (prog-mode-hook . turn-on-smartparens-strict-mode)
  :config
  (require 'smartparens-config)
  (smartparens-global-mode))

(use-package rainbow-mode
  :diminish rainbow-mode)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package super-save
  :defer t
  :custom
  ;; Disable auto-saving for remote files
  (super-save-remote-files nil)
  :config
  (super-save-mode))

(use-package markdown-mode
  :mode ("\\.md\\'" . gfm-mode))

(use-package projectile
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (setq projectile-completion-system 'ivy)
  (projectile-mode +1))

(use-package treemacs
  :hook (treemacs-mode-hook . (lambda () (display-line-numbers-mode -1)))
  :config
  (treemacs-filewatch-mode t)
  (treemacs-git-mode 'simple))

(use-package treemacs-projectile  :after (treemacs projectile))
(use-package treemacs-magit :after (treemacs magit))
(use-package treemacs-icons-dired :after (treemacs dired) :config (treemacs-icons-dired-mode))

(use-package magit :bind ("C-c m s" . magit-status))

(use-package restclient :defer t)
(use-package ob-elixir :defer t)

(use-package yasnippet :diminish yas-minor-mode :config (yas-global-mode 1))
(use-package yasnippet-snippets :after (yasnippet))

(use-package lsp-mode
  :commands (lsp lsp-deferred lsp-format-buffer)
  :custom
  (lsp-completion-provider :none) ;; we use Corfu!
  :init
  (defun vn/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(flex))) ;; Configure orderless
  (add-to-list 'exec-path "/home/victornascimento/Projects/elixir-ls/erl24/")
  :config
  (setq lsp-file-watch-threshold 2000)
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.elixir_ls\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\deps\\'")
  (setq lsp-diagnostics-provider :flymake)
  (setq lsp-lens-enable nil)
  :hook
  (lsp-completion-mode . vn/lsp-mode-setup-completion)
  (elixir-mode . lsp))

(use-package lsp-ui
  :commands lsp-ui-mode
  :after (lsp-mode)
  :init
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-show-with-mouse t)
  (setq lsp-prefer-flymake t))

(use-package elixir-mode
  :hook (elixir-mode . (lambda ()
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
(use-package all-the-icons
  :if (display-graphic-p))

(use-package erlang
  :defer t)

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

(use-package web-mode
  :init
  (setq
   web-mode-markup-indent-offset 2
   web-mode-css-indent-offset 2
   web-mode-code-indent-offset 2)
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.eex?\\'" . web-mode)))

(use-package ox-gfm)
(use-package ox-spectacle)

(use-package plantuml-mode
  :init
  (setq plantuml-jar-path (expand-file-name "vendor/plantuml.1.2019.7.jar" user-emacs-directory))
  (setq plantuml-default-exec-mode "jar")
  :mode "\\.uml\\'")

(use-package org
  :bind (("C-c a" . org-agenda))
  :custom
  (org-hide-leading-stars t)
  (org-list-allow-alphabetical t)
  (org-src-fontify-natively t "You want this to activate coloring in blocks")
  (org-src-tab-acts-natively t "You want this to have completion in blocks")
  (org-hide-emphasis-markers t "This hides the *,=, or / markers")
  (org-pretty-entities t "to have \alpha, \to and others display as utf8")
  (org-pretty-entities-include-sub-superscripts nil)
  (org-export-with-sub-superscripts nil "Disable sub and superscripts in exports only")
  (org-ditaa-jar-path (expand-file-name "vendor/ditaa0_9.jar" user-emacs-directory))
  (org-bulle)
                                        ;  (org-bullets-bullet-list (quote ("◉" "◆" "✚" "☀" "○")))
  (org-plantuml-jar-path
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
    (push (org-projectile-project-todo-entry) org-capture-templates)))

(use-package org-superstar
  :init
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1))))

(use-package org-tree-slide
  :hook ((org-tree-slide-play . org-display-inline-images))
  :custom
  (org-image-actual-width nil))

(use-package docker
  :pin melpa
  :bind ("C-c d" . docker)) 

(use-package kubernetes
  :commands (kubernetes-overview))

(use-package graphviz-dot-mode)

(use-package transient
  :pin melpa)

(use-package solaire-mode
  :after doom-themes
  :init (solaire-global-mode +1))

(use-package doom-modeline
  :after all-the-icons
  :init (doom-modeline-mode)
  :custom  (doom-modeline-height 35))

(use-package doom-themes
  :pin melpa
  :after (treemacs)
  :config  
  (load-theme 'doom-city-lights t)
  (setq doom-themes-treemacs-theme "doom-colors")
  (setq doom-treemacs-enable-variable-pitch t)
  (doom-themes-treemacs-config))

(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))

;; Make GC pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))

(provide 'init)
;;; init.el ends here

