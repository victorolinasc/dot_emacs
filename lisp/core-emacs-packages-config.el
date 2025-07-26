;;; core-emacs-packages-config.el --- Summary:  -*- lexical-binding: t; -*-

;;; Commentary:
;;; Configuration of core emacs packages

(use-package repeat :config (repeat-mode))

(use-package
 flyspell
 :ensure nil
 :hook (text-mode . flyspell-mode) (prog-mode . flyspell-prog-mode)
 :config
 (setopt
  ispell-program-name "aspell"
  ispell-dictionary "en"))

(use-package eldoc :ensure nil :diminish eldoc-mode)
(use-package autorevert :ensure nil :diminish auto-revert-mode)

(use-package which-key :ensure nil :config (which-key-mode))

(use-package
 dired
 :ensure nil
 :defer t
 :config
 (setopt
  dired-auto-revert-buffer t ; Revert on re-visiting
  ;; Better dired flags:
  ;; `-l' is mandatory
  ;; `-a' shows all files
  ;; `-h' uses human-readable sizes
  ;; `-F' appends file-type classifiers to file names (for better highlighting)
  dired-listing-switches "-laFGh1v --group-directories-first"
  dired-ls-F-marks-symlinks t ; -F marks links with @
  ;; Inhibit prompts for simple recursive operations
  dired-recursive-copies 'always
  ;; Auto-copy to other Dired split window
  dired-dwim-target t))

(use-package project :ensure nil :bind ("s-f" . project-find-file))

(use-package
 eglot
 :ensure nil
 :hook
 (rust-ts-mode . eglot-ensure)
 (conf-toml-mode . eglot-ensure)
 (toml-ts-mode . eglot-ensure)
 (elixir-ts-mode . eglot-ensure)
 (kotlin-ts-mode . eglot-ensure)
 (terraform-mode . eglot-ensure)
 (dart-mode . eglot-ensure)
 (before-save . eglot-format)
 :bind
 ("C-M-i" . eglot-code-actions)
 ("s-r" . eglot-rename)
 :config
 (fset #'jsonrpc--log-event #'ignore)
 (add-to-list 'eglot-server-programs '((elixir-ts-mode heex-ts-mode elixir-mode) . ("elixir-ls")))
 (add-to-list 'eglot-server-programs '((ponylang-ts-mode ponylang-mode) . ("pony-lsp")))
 (add-to-list 'eglot-server-programs '((toml-ts-mode) . ("taplo" "lsp" "stdio")))
 (add-to-list
  'eglot-server-programs '((text-mode markdown-mode gfm-mode) . ("harper-ls" "--stdio")))
 (add-to-list
  'eglot-server-programs
  '((rust-ts-mode rust-mode)
    .
    ("rust-analyzer" :initializationOptions (:check (:command "clippy"))))))

(use-package dockerfile-ts-mode :ensure nil :defer t :mode "Dockerfile\\'")

(use-package shell :ensure nil :hook (shell-mode . (lambda () (display-line-numbers-mode -1))))

(use-package
 hs-minor-mode
 :ensure nil
 :hook (prog-mode . hs-minor-mode)
 :bind
 ("C-{" . hs-hide-block)
 ("C-}" . hs-show-block))
