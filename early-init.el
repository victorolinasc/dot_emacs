;;; early-init.el -*- lexical-binding: t; -*-

;; Increase the GC threshold for faster startup
;; The default is 800 kilobytes.  Measured in bytes.
(setopt gc-cons-threshold (* 50 1000 1000))

(setq byte-compile-warnings '(not obsolete))
(setq warning-suppress-log-types '((comp) (bytecomp)))
(setq native-comp-async-report-warnings-errors 'silent)

;; Prefer loading newest compiled .el file
(customize-set-variable 'load-prefer-newer noninteractive)

(setopt frame-resize-pixelwise t)
(tool-bar-mode -1)                      ; All these tools are in the menu-bar anyway
(setopt default-frame-alist '((fullscreen . maximized)

                            ;; You can turn off scroll bars by uncommenting these lines:
                            ;; (vertical-scroll-bars . nil)
                            ;; (horizontal-scroll-bars . nil)

                            ;; Setting the face in here prevents flashes of
                            ;; color as the theme gets activated
                            (background-color . "#000000")
                            (ns-appearance . dark)
                            (ns-transparent-titlebar . t)))

;;; package configuration
(require 'package)
(require 'use-package-ensure)

(add-to-list 'package-archives '("stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(customize-set-variable
 'package-archive-priorities
 '(("gnu" . 99) ; prefer GNU packages
   ("nongnu" . 80) ; use non-gnu packages if not found in GNU elpa
   ("stable" . 70) ; prefer "released" versions from melpa
   ("melpa" . 0))) ; if all else fails, get it from melpa

(package-initialize)

; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

(setopt use-package-always-ensure t)

(setopt load-prefer-newer t)
(setopt package-native-compile t)

(use-package diminish)
(use-package bind-key)

(use-package compile-angel
  :ensure t
  :demand t
  :config
  ;; Set `compile-angel-verbose' to nil to silence compile-angel.
  (setopt compile-angel-verbose t)

  (compile-angel-on-load-mode)
  (add-hook 'emacs-lisp-mode-hook #'compile-angel-on-save-local-mode))

;; Native compilation settings
(when (featurep 'native-compile)
  ;; Silence compiler warnings as they can be pretty disruptive
  (setopt native-comp-async-report-warnings-errors nil)

  ;; Make native compilation happens asynchronously
  (setopt native-comp-jit-compilation t)

  ;; Set the right directory to store the native compilation cache
  ;; NOTE the method for setting the eln-cache directory depends on the emacs version
  (when (fboundp 'startup-redirect-eln-cache)
    (if (version< emacs-version "29")
        (add-to-list
         'native-comp-eln-load-path
         (convert-standard-filename (expand-file-name "var/eln-cache/" user-emacs-directory)))
      (startup-redirect-eln-cache
       (convert-standard-filename (expand-file-name "var/eln-cache/" user-emacs-directory)))))

  (add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory)))

;; Make the initial buffer load faster by setting its mode to fundamental-mode
(customize-set-variable 'initial-major-mode 'fundamental-mode)

;; Always start maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))
