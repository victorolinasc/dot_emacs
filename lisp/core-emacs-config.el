;;; core-emacs-config.el --- Summary:  -*- lexical-binding: t; -*-

;;; Commentary:
;;; Configuration of core emacs variables, hooks and so on
(use-package
 emacs
 :ensure nil
 :hook
 (after-init
  .
  (lambda ()
    (setopt gc-cons-threshold (* 2 1000 1000))
    (window-divider-mode)))
 (prog-mode . (lambda () (display-line-numbers-mode 1)))
 (text-mode . (lambda () (display-line-numbers-mode 1)))
 (prog-mode . editorconfig-mode)
 :config (setopt auto-revert-avoid-polling t)
 ;; Set custom file early
 (setopt custom-file (expand-file-name "lisp/.custom.el" user-emacs-directory))

 ;; Introduction
 (setopt user-full-name "Victor Oliveira Nascimento")

 ;; Tune system limits
 (setopt
  gc-cons-threshold (* 20 1024 1024)
  read-process-output-max (* 1024 1024)))

(use-package
 emacs
 :ensure nil
 :config
 (setopt
  load-prefer-newer t
  bidi-paragraph-direction 'left-to-right
  initial-scratch-message "")
 ;; store all backup and autosave files in the tmp dir
 (setopt backup-directory-alist `((".*" . ,temporary-file-directory)))
 (setopt auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
 (setopt create-lockfiles nil)
 (setopt fill-column 100)
 (setopt inhibit-startup-screen t)

 (setopt line-spacing 0.15)

 (setopt kill-do-not-save-duplicates t)

 (setopt indent-tabs-mode nil)
 (setopt use-short-answers t)
 (setopt display-line-numbers-width 3)
 (setopt
  column-number-mode t
  size-indication-mode t
  delete-selection-mode t
  menu-bar-mode nil
  tool-bar-mode nil
  scroll-bar-mode nil)

 (setq-default
  ;display-line-numbers 'absolute
  display-line-numbers-width 4
  display-line-numbers-widen t)

 (unbind-key "C-z")
 (global-prettify-symbols-mode)

 (load-file (expand-file-name "lisp/vn.el" user-emacs-directory))

 (setopt auto-revert-interval 5)
 (setopt auto-revert-check-vc-info t)
 (global-auto-revert-mode)
 (savehist-mode)
 (setopt sentence-end-double-space nil)
 (setopt enable-recursive-minibuffers t) ; Use the minibuffer whilst in the minibuffer
 (setopt tab-always-indent 'complete) ; When I hit TAB, try to complete, otherwise, indent
 (setopt completions-detailed t) ; Show annotations
 (setopt completions-group t)

 (with-eval-after-load 'comint
   (add-hook 'comint-mode-hook #'completion-preview-mode))

 (with-eval-after-load 'completion-preview
   ;; Show the preview already after two symbol characters
   (setopt completion-preview-minimum-symbol-length 2)

   ;; Cycle the completion candidate that the preview shows
   (keymap-set completion-preview-active-mode-map "M-n" #'completion-preview-next-candidate)
   (keymap-set completion-preview-active-mode-map "M-p" #'completion-preview-prev-candidate)
   ;; Convenient alternative to C-i after typing one of the above
   (keymap-set completion-preview-active-mode-map "M-i" #'completion-preview-insert))

 :init
 (load custom-file 'noerror)
 (setq-default bidi-paragraph-direction 'left-to-right)
 (prefer-coding-system 'utf-8)
 (set-default-coding-systems 'utf-8)
 (set-terminal-coding-system 'utf-8)
 (set-keyboard-coding-system 'utf-8)
 (global-hl-line-mode t)
 (setq-default indent-tabs-mode nil)

 ;; Fonts
 (set-face-attribute 'default nil :font "Fira Code Nerd Font" :height 170 :weight 'light)
 (set-face-attribute 'fixed-pitch nil :font "Fira Code Nerd Font" :height 170 :weight 'light)
 (set-face-attribute 'variable-pitch nil :font "Fira Code Nerd Font" :height 170 :weight 'light)

 :hook
 (prog-mode . completion-preview-mode)
 (text-mode-hook . completion-preview-mode))

(use-package
 emacs
 :ensure nil
 :config
 ;; Should use:
 ;; (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))
 ;; at least once per installation or while changing this list
 (setopt treesit-language-source-alist
         '((bash "https://github.com/tree-sitter/tree-sitter-bash")
           (css "https://github.com/tree-sitter/tree-sitter-css")
           (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
           (elisp "https://github.com/Wilfred/tree-sitter-elisp")
           (heex "https://github.com/phoenixframework/tree-sitter-heex")
           (elixir "https://github.com/elixir-lang/tree-sitter-elixir")
           (go "https://github.com/tree-sitter/tree-sitter-go")
           (html "https://github.com/tree-sitter/tree-sitter-html")
           (java "https://github.com/tree-sitter/tree-sitter-java")
           (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
           (json "https://github.com/tree-sitter/tree-sitter-json")
           (kotlin "https://github.com/fwcd/tree-sitter-kotlin")
           (make "https://github.com/alemuller/tree-sitter-make")
           (markdown "https://github.com/ikatyang/tree-sitter-markdown")
           (ponylang "https://github.com/mfelsche/tree-sitter-ponylang")
           (proto "https://github.com/Clement-Jean/tree-sitter-proto")
           (python "https://github.com/tree-sitter/tree-sitter-python")
           (rust "https://github.com/tree-sitter/tree-sitter-rust")
           (toml "https://github.com/tree-sitter/tree-sitter-toml")
           (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
           (typescript
            "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
           (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

 (add-to-list 'major-mode-remap-alist '(yaml-mode . yaml-ts-mode))
 (add-to-list 'major-mode-remap-alist '(rust-mode . rust-ts-mode))
 (add-to-list 'major-mode-remap-alist '(conf-toml-mode . toml-ts-mode)))

(use-package
 emacs
 :ensure nil
 :bind
 ;; C-x C-0 restores the default font size
 (("C-+" . text-scale-increase)
  ("C--" . text-scale-decrease)
  ("<f5>" . treemacs)
  ("<f6>" . flymake-show-buffer-diagnostics)
  ("<f7>" . vterm)
  ("C-o" . other-window)
  ("s-b" . switch-to-buffer)
  ("C-q" . kill-buffer)
  ("C-<prior>" . previous-buffer)
  ("C-<next>" . next-buffer)))

(use-package
 emacs
 :ensure nil
 :config (setopt sql-postgres-program "~/.local/share/mise/installs/postgres/16.5/bin/psql"))

;;; Core-emacs-config.el ends here
