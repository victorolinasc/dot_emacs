;;; external-packages-config.el --- Summary:  -*- lexical-binding: t; -*-

;;; Commentary:
;;; Configuration of external packages fetches with use-package
(use-package inheritenv)

(use-package mise :init (add-hook 'after-init-hook 'global-mise-mode -10))

(use-package
 exec-path-from-shell
 :after (mise inheritenv)
 :custom
 (exec-path-from-shell-variables '("SSH_AUTH_SOCK" "JAVA_HOME" "PATH"))
 (exec-path-from-shell-arguments '("-l"))
 (exec-path-from-shell-check-startup-files nil)
 (exec-path-from-shell-debug nil)

 :init (exec-path-from-shell-initialize))

(use-package
 treesit-auto
 :custom (treesit-auto-install 'prompt)
 :config
 (treesit-auto-add-to-auto-mode-alist 'all)
 (global-treesit-auto-mode))

(use-package
 helpful
 :bind
 ("C-h f" . helpful-callable)
 ("C-h v" . helpful-variable)
 ("C-h k" . helpful-key)
 ("C-h F" . helpful-function)
 ("C-h C" . helpful-command))

(defun corfu-enable-in-minibuffer ()
  "Enable Corfu in the minibuffer if `completion-at-point' is bound."
  (when (where-is-internal #'completion-at-point (list (current-local-map)))
    ;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
    (setq-local
     corfu-echo-delay nil ;; Disable automatic echo and (point)opup
     corfu-popupinfo-delay nil)
    (corfu-mode 1)))

(use-package
 corfu
 :bind
 (:map
  corfu-map
  ("<escape>" . corfu-quit)
  ("<return>" . corfu-insert)
  ("M-n" . corfu-popupinfo-scroll-up)
  ("M-p" . corfu-popupinfo-scroll-down))
 :custom
 ;; Works with `indent-for-tab-command'. Make sure tab doesn't indent when you
 ;; want to perform completion
 (tab-always-indent 'complete) (corfu-auto nil) (corfu-auto-prefix 2) (corfu-auto-delay 0.25)

 (corfu-preview-current 'insert) (corfu-preselect-first t)
 :init (global-corfu-mode) (corfu-popupinfo-mode) (corfu-echo-mode)
 :hook (minibuffer-setup . corfu-enable-in-minibuffer))

(use-package
 cape
 ;; Bind dedicated completion commands
 ;; Alternative prefix keys: C-c p, M-p, M-+, ...
 :bind
 (("M-/" . completion-at-point) ;; capf
  ("C-c p t" . complete-tag) ;; etags
  ("C-c p d" . cape-dabbrev) ;; or dabbrev-completion
  ("C-c p h" . cape-history)
  ("C-c p f" . cape-file)
  ("C-c p k" . cape-keyword)
  ("C-c p s" . cape-symbol)
  ("C-c p a" . cape-abbrev)
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
 (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster))

;; Enable vertico
(use-package vertico :init (vertico-mode))

(use-package
 embark
 ;; Embark is an Emacs package that acts like a context menu, allowing
 ;; users to perform context-sensitive actions on selected items
 ;; directly from the completion interface.
 :defer t
 :commands (embark-act embark-dwim embark-export embark-collect embark-bindings embark-prefix-help-command)
 :bind
 (("C-." . embark-act) ;; pick some comfortable binding
  ("C-;" . embark-dwim) ;; good alternative: M-.
  ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

 :init (setq prefix-help-command #'embark-prefix-help-command)

 :config
 ;; Hide the mode line of the Embark live/completions buffers
 (add-to-list
  'display-buffer-alist
  '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
    nil
    (window-parameters (mode-line-format . none)))))

(use-package embark-consult :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package
 consult
 :bind
 ( ;; C-c bindings (mode-specific-map)
  ("C-x b" . consult-buffer) ;; orig. switch-to-buffer
  ("C-x C-b" . consult-buffer)
  ("C-x p b" . consult-project-buffer) ;; orig. project-switch-to-buffer
  ("s-b" . consult-project-buffer) ;; orig. project-switch-to-buffer
  ;; Other custom bindings
  ("M-y" . consult-yank-pop) ;; orig. yank-pop
  ("M-g f" . consult-flymake) ;; Alternative: consult-flycheck
  ("M-g g" . consult-goto-line) ;; orig. goto-line
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
  :map
  isearch-mode-map
  ("M-e" . consult-isearch-history) ;; orig. isearch-edit-string
  ("M-s e" . consult-isearch-history) ;; orig. isearch-edit-string
  ("M-s l" . consult-line) ;; needed by consult-line to detect isearch
  ("M-s L" . consult-line-multi) ;; needed by consult-line to detect isearch
  ;; Minibuffer history
  :map
  minibuffer-local-map
  ("M-s" . consult-history) ;; orig. next-matching-history-element
  ("M-r" . consult-history)) ;; orig. previous-matching-history-element

 :hook (completion-list-mode . consult-preview-at-point-mode))

;; Optionally use the `orderless' completion style.
(use-package
 orderless
 :init
 (setq
  completion-styles '(orderless basic)
  completion-category-defaults nil
  completion-category-overrides '((file (styles partial-completion)))))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist :init (savehist-mode))

(use-package
 marginalia
 :init
 ;; Must be in the :init section of use-package such that the mode gets
 ;; enabled right away. Note that this forces loading the package.
 (marginalia-mode))

(use-package
 outline-indent
 :defer t
 :commands outline-indent-minor-mode

 :custom (outline-indent-ellipsis " ▼ ")

 :init
 ;; The minor mode can also be automatically activated for a certain modes.
 (add-hook 'python-mode-hook #'outline-indent-minor-mode)
 (add-hook 'python-ts-mode-hook #'outline-indent-minor-mode)

 (add-hook 'yaml-mode-hook #'outline-indent-minor-mode)
 (add-hook 'yaml-ts-mode-hook #'outline-indent-minor-mode))

(use-package svg-lib)

(use-package
 kind-icon
 :after
 corfu
 svg-lib
 :custom
 (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly  
 :config
 (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
 (setq kind-icon-default-style '(:padding -1 :stroke 0 :margin 0 :radius 0 :height 0.4 :scale 1.0)))

(use-package move-text :config (move-text-default-bindings))

(use-package
 smartparens
 :bind
 (:map
  smartparens-mode-map
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
 :config (require 'smartparens-config) (smartparens-global-mode))

(use-package
 colorful-mode
 :diminish t
 :custom (colorful-use-prefix t) (colorful-only-strings nil) (css-fontify-colors nil)
 :config
 (add-to-list
  'colorful-extra-color-keyword-functions
  '(dart-mode . (colorful-add-rgb-colors colorful-add-hsl-colors colorful-add-hex-colors))))

(use-package
 super-save
 :defer t
 :custom
 ;; Disable auto-saving for remote files
 (super-save-remote-files nil)
 :config (super-save-mode))

(use-package markdown-mode :mode ("\\.md\\'" . gfm-mode))

(use-package
 treemacs
 :hook (treemacs-mode-hook . (lambda () (display-line-numbers-mode -1)))
 :config (treemacs-filewatch-mode t) (treemacs-git-mode 'simple)

 (defun ignore-pycache (_ abs-path)
   "Hide __pycache__ matches on ABS-PATH in treemacs"
   (if (not (string-search "__pycache__" abs-path))
       nil
     t))

 (add-to-list 'treemacs-ignored-file-predicates 'ignore-pycache))

(use-package treemacs-magit :after (treemacs magit))
(use-package treemacs-icons-dired :after (treemacs dired) :config (treemacs-icons-dired-mode))

(use-package magit :bind ("C-c m s" . magit-status))

(use-package restclient :defer t :mode "\\.restclient\\'")
(use-package ob-elixir :defer t)
(use-package yasnippet :diminish yas-minor-mode :config (yas-global-mode 1))
(use-package yasnippet-snippets :after (yasnippet))

(use-package
 elixir-ts-mode
 :hook (elixir-ts-mode . eglot-ensure)
 (elixir-ts-mode
  .
  (lambda ()
    (push '(">=" . ?\u2265) prettify-symbols-alist)
    (push '("<=" . ?\u2264) prettify-symbols-alist)
    (push '("!=" . ?\u2260) prettify-symbols-alist)
    (push '("==" . ?\u2A75) prettify-symbols-alist)
    (push '("=~" . ?\u2245) prettify-symbols-alist)
    (push '("<-" . ?\u2190) prettify-symbols-alist)
    (push '("->" . ?\u2192) prettify-symbols-alist)
    (push '("<-" . ?\u2190) prettify-symbols-alist)
    (push '("|>" . ?\u25B7) prettify-symbols-alist)))
 (before-save . eglot-format))

(use-package
 exunit
 :diminish t
 :bind
 ("C-c e ." . exunit-verify-single)
 ("C-c e b" . exunit-verify)
 ("C-c e u a" . exunit-verify-all-in-umbrella)
 ("C-c e a" . exunit-verify-all)
 ("C-c e l" . exunit-rerun))

;; On a clean build, we need to call 'all-the-icons-install-fonts' function
(use-package all-the-icons :if (display-graphic-p))

(use-package erlang :defer t)

(use-package
 multiple-cursors
 :bind ("C->" . 'mc/mark-next-like-this) ("C-<" . 'mc/mark-previous-like-this))

(use-package yaml-mode)

(use-package
 web-mode
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

(use-package
 plantuml-mode
 :init
 (setq plantuml-jar-path
       (expand-file-name "vendor/plantuml.1.2019.7.jar" user-emacs-directory))
 (setq plantuml-default-exec-mode "jar")
 :mode "\\.uml\\'")

(use-package
 org
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
 (org-plantuml-jar-path (expand-file-name "vendor/plantuml.1.2019.7.jar" user-emacs-directory))
 :config
 (org-babel-do-load-languages
  'org-babel-load-languages
  '((emacs-lisp . t) (elixir . t) (org . t) (java . t) (ditaa . t) (plantuml . t))))

(use-package org-superstar :init (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1))))

(use-package
 org-tree-slide
 :hook ((org-tree-slide-play . org-display-inline-images))
 :custom (org-image-actual-width nil))

(use-package
 docker
 :pin melpa
 :bind ("C-c d" . docker)
 :hook (docker-mode . (lambda () (display-line-numbers-mode -1))))

(use-package kubernetes :commands (kubernetes-overview))

(use-package kubed :config (keymap-global-set "C-c k" 'kubed-prefix-map))

(use-package graphviz-dot-mode)

(use-package
 elisp-autofmt
 :commands (elisp-autofmt-mode elisp-autofmt-buffer)
 :hook (emacs-lisp-mode . elisp-autofmt-mode))

(use-package mermaid-mode)

;; (use-package
;; doom-modeline
;; :after all-the-icons
;; :init (doom-modeline-mode)
;; :custom (doom-modeline-height 50))

(use-package
 doom-themes
 :pin melpa
 :after (treemacs)
 :config
 (load-theme 'doom-monokai-pro t)
 (setq doom-themes-treemacs-theme "doom-colors")
 (setq doom-treemacs-enable-variable-pitch t)
 (doom-themes-treemacs-config))

(use-package
 solaire-mode
 :init (solaire-global-mode +1)
 :hook (dashboard-before-initialize . solaire-mode))

(use-package
 kotlin-ts-mode
 :hook
 (kotlin-ts-mode . eglot-ensure)
 (before-save . eglot-format)
 :mode ("\\.kt\\'" "\\.kts\\'"))

(use-package
 eglot-java
 :custom (eglot-java-java-program (expand-file-name "~/.local/share/mise/shims/java"))
 :bind
 ("C-c l n" . eglot-java-file-new)
 ("C-c l x" . eglot-java-run-main)
 ("C-c l t" . eglot-java-run-test)
 ("C-c l N" . eglot-java-project-new)
 ("C-c l T" . eglot-java-project-build-task)
 ("C-c l R" . eglot-java-project-build-refresh))

(use-package rust-mode :init (setq rust-mode-treesitter-derive t))

(use-package
 rustic
 :pin melpa
 :after rust-mode
 :init
 (setq rustic-lsp-client 'eglot)
 (setq rustic-cargo-test-runner 'nextest)
 (setq rustic-cargo-nextest-exec-command '("nextest" "run" "--no-capture")))

(use-package
 pet
 :pin melpa
 ;; :ensure-system-package (dasel sqlite3)
 :config
 (add-hook
  'python-base-mode-hook
  (lambda ()
    (setq-local
     python-shell-interpreter (pet-executable-find "python")
     python-shell-virtualenv-root (pet-virtualenv-root))

    (pet-eglot-setup)
    (pet-dape-setup)
    (eglot-ensure)

    (setq-local python-pytest-executable (pet-executable-find "pytest"))))

 (add-hook 'python-base-mode-hook 'pet-mode -10))

(use-package python-pytest :bind ("C-x t f" . python-pytest-function))

(use-package envrc :hook (after-init . envrc-global-mode))

(use-package zig-mode)

(require 'cl-lib)
(defun csv-highlight (&optional separator)
  (interactive (list
                (when current-prefix-arg
                  (read-char "Separator: "))))
  (font-lock-mode 1)
  ;; format: off
  (let* ((separator (or separator ?\,))
         (n (count-matches (string separator) (pos-bol) (pos-eol)))
         (colors (cl-loop for i from 0 to 1.0 by (/ 2.0 n)
                          collect (apply #'color-rgb-to-hex
                                         (color-hsl-to-rgb i 0.3 0.5)))))
        (cl-loop for i from 2 to n by 2
                 for c in colors
                 for r = (format "^\\([^%c\n]+%c\\)\\{%d\\}" separator separator i)
                 do (font-lock-add-keywords nil `((,r (1 '(face (:foreground ,c))))))))
  ;; format: on
  )

(use-package csv-mode :hook (csv-mode . csv-align-mode))

(use-package
 dape
 :hook
 ;; Save breakpoints on quit
 (kill-emacs . dape-breakpoint-save)
 ;; Load breakpoints on startup
 (after-init . dape-breakpoint-load)

 :config
 ;; Turn on global bindings for setting breakpoints with mouse
 (dape-breakpoint-global-mode)

 ;; Info buffers like gud (gdb-mi)
 (setopt dape-buffer-window-arrangement 'gud) ;
 (setopt dape-info-hide-mode-line nil)

 ;; Showing inlay hints
 (setopt dape-inlay-hints t)

 ;; Kill compile buffer on build success
 (add-hook 'dape-compile-hook 'kill-buffer))

(use-package hcl-mode)

(use-package
 terraform-mode
 :custom (terraform-indent-level 4)
 :config
 (defun my-terraform-mode-init ()
   (outline-minor-mode 1))

 :hook (terraform-mode . my-terraform-mode-init) (terraform-mode . eglot-ensure))

(use-package
 centaur-tabs
 :demand
 :init
 (setq centaur-tabs-height 35)
 (setq centaur-tabs-set-icons t)
 (setq centaur-tabs-icon-type 'all-the-icons)
 :config (centaur-tabs-mode t)
 :bind
 ("C-<prior>" . centaur-tabs-backward)
 ("C-<next>" . centaur-tabs-forward))

(use-package sqlformat :config (setq sqlformat-command 'pgformatter))

(use-package
 dart-mode
 ;; Optional
 :hook (dart-mode . eglot-ensure) (before-save . eglot-format))

(use-package eat :bind ("<f2>" . eat-project) :config (setq-local display-line-number -1))

(use-package
 moody
 :config
 (moody-replace-mode-line-front-space)
 (moody-replace-mode-line-buffer-identification)
 (moody-replace-vc-mode))

(use-package minions :config (minions-mode 1))

(use-package yaml)

(use-package
 ultra-scroll
 :vc (:url "https://github.com/jdtsmith/ultra-scroll")
 :init
 (setq
  scroll-conservatively 101 ; important!
  scroll-margin 0)
 :config (ultra-scroll-mode 1))

(use-package
 treesit-fold
 :init (setq treesit-fold-line-count-show t)
 :hook (prog-mode . treesit-fold-mode))

(use-package
 hurl-mode
 :ensure nil
 :vc (:url "https://github.com/JasZhe/hurl-mode.git" :rev :newest :branch "main")
 :mode "\\.hurl\\'")

(use-package just-ts-mode)
(use-package justl)

(use-package
 eldoc-box
 :bind
 ("M-n" . eldoc-box-scroll-up)
 ("M-p" . eldoc-box-scroll-down)
 ("C-h ." . eldoc-box-help-at-point))

(use-package
 dashboard
 :pin melpa
 :config (dashboard-setup-startup-hook)
 :init
 (setq
  dashboard-banner-logo-title "Willbank Capitão"
  dashboard-startup-banner 'logo
  dashboard-icon-type 'all-the-icons
  dashboard-set-heading-icons t
  dashboard-set-file-icons t
  dashboard-projects-backend 'project-el
  dashboard-items '((recents . 5) (projects . 5) (agenda . 5))))

(use-package
 uniline
 :defer t
 :bind ("C-<insert>" . uniline-mode)
 :init (setq uniline-interface-type :transient))

(provide 'external-packages-config)
;;; external-packages-config.el ends here
