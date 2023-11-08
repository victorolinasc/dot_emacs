;;; core-emacs-packages-config.el --- Summary:

;;; Commentary:
;;; Configuration of core emacs packages

(use-package
 flyspell
 :ensure nil
 :config
 (setq
  ispell-program-name "aspell"
  ispell-dictionary "en")
 (add-hook 'text-mode-hook #'flyspell-mode) (add-hook 'prog-mode-hook #'flyspell-prog-mode))

(use-package eldoc :ensure nil :diminish eldoc-mode)
(use-package autorevert :ensure nil :diminish auto-revert-mode)

(use-package
 dired
 :ensure nil
 :defer t
 :config
 (setq
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
 :config
 (add-to-list
  'eglot-server-programs `((elixir-ts-mode heex-ts-mode elixir-mode) . ("language_server.sh"))))

(use-package rust-ts-mode :ensure nil :defer t :hook (rust-ts-mode . eglot-ensure))
