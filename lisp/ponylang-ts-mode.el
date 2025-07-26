;;; ponylang-ts-mode.el --- A major mode for the Pony programming language
;;
;; Authors: olinasc
;; Version: 0.0.1
;; Keywords: languages programming
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Description:
;;
;; This is a major mode for the Pony programming language
;;
;; Installation:
;;
;;; License:
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(require 'treesit)

(add-to-list 'treesit-language-source-alist
             '(ponylang
               "https://github.com/mfelsche/tree-sitter-ponylang"
               :commit "cc8a0ff12f4f9e56f8a0d997c55155b702938dfe")
             t)

(defgroup ponylang-ts nil
  "Major mode for editing Ponylang code."
  :prefix "ponylang-ts-"
  :group 'languages)

;;; Code:
(defconst ponylang-ts--imenu-generic-expression
  '(("TODO" ".*TODO:[ \t]*\\(.*\\)$" 1)
    ("fun"
     "[ \t]*fun[ \t]+$?\\(iso\\|trn\\|ref\\|val\\|box\\|tag\\)?[ \t]*\\([a-zA-Z0-9_]+\\)[ \t]*"
     2)
    ("be" "[ \t]*be[ \t]+\\([a-zA-Z0-9_]+\\)[ \t]*(" 1)
    ("new"
     "[ \t]*new[ \t]+\\(iso\\|trn\\|ref\\|val\\|box\\|tag\\)*[ \t]*\\([a-zA-Z0-9_]+\\)[ \t]*("
     2)
    ("type" "^[ \t]*type[ \t]+\\([a-zA-Z0-9_]+\\)" 1)
    ("interface"
     "^[ \t]*interface[ \t]+\\(iso\\|trn\\|ref\\|val\\|box\\|tag\\)?[ \t]*\\([a-zA-Z0-9_]+\\)"
     2)
    ("trait"
     "^[ \t]*trait[ \t]+\\(iso\\|trn\\|ref\\|val\\|box\\|tag\\)?[ \t]*\\([a-zA-Z0-9_]+\\)"
     2)
    ("struct" "^[ \t]*struct[ \t]+\\([a-zA-Z0-9_]+\\)" 1)
    ("primitive" "^[ \t]*primitive[ \t]+\\([a-zA-Z0-9_]+\\)" 1)
    ("actor" "^[ \t]*actor[ \t]+\\([a-zA-Z0-9_]+\\)" 1)
    ("class"
     "^[ \t]*class[ \t]+\\(iso\\|trn\\|ref\\|val\\|box\\|tag\\)?[ \t]*\\([a-zA-Z0-9_]+\\)"
     2)
    ("use" "^[ \t]*use[ \t]+\\([a-zA-Z0-9_]+\\)" 1)))

(defconst ponylang-ts--syntax-table
  (let ((table (make-syntax-table)))
    ;; fontify " using ponylang-keywords

    ;; / is punctuation, but // is a comment starter
    (modify-syntax-entry ?/ ". 124" table)

    ;; /* */ comments, which can be nested
    (modify-syntax-entry ?* ". 23bn" table)

    ;; \n is a comment ender
    (modify-syntax-entry ?\n ">" table)

    ;; string
    (modify-syntax-entry ?\" "\"" table)

    ;; Don't treat underscores as whitespace
    (modify-syntax-entry ?_ "w" table)
    table))


;;;###autoload
(define-derived-mode
 ponylang-ts-mode prog-mode "Ponylang" "Major mode for editing Ponylang, powered by tree-sitter."
 :group 'ponylang-ts
 :syntax-table ponylang-ts--syntax-table

 ;; Compile.
 (setq-local compile-command "ponyc")

 (when (treesit-ready-p 'ponylang)

   (setq-local treesit-primary-parser (treesit-parser-create 'ponylang))

   ;; Font-lock.
   ;   (setq-local treesit-font-lock-settings ponylang-ts--font-lock-settings)
   ;   (setq-local treesit-font-lock-feature-list ponylang-ts--font-lock-feature-list)

   ;; Imenu.
   (setq-local treesit-simple-imenu-settings ponylang-ts--imenu-generic-expression)

   ;; Indent.
   ;; (setq-local treesit-simple-indent-rules elixir-ts--indent-rules)

   ;; Navigation.
   ;; (setq-local treesit-thing-settings
   ;;             `((elixir ,@elixir-ts--thing-settings) (heex ,@heex-ts--thing-settings)))
   ;; (setq-local treesit-defun-type-regexp '("call" . elixir-ts--defun-p))

   ;; (setq-local treesit-defun-name-function #'elixir-ts--defun-name)

   (treesit-major-mode-setup)

   (setq-local syntax-propertize-function #'elixir-ts--syntax-propertize)

   ;; Enable the 'sexp' navigation by default
   (setq-local
    forward-sexp-function #'treesit-forward-sexp
    treesit-sexp-thing 'sexp
    ;; But still use 'list' for `down-list' and `up-list'
    treesit-sexp-thing-down-list 'list
    treesit-sexp-thing-up-list 'list)))

(if (treesit-ready-p 'ponylang)
    (progn
      (add-to-list 'auto-mode-alist '("\\.pony\\'" . ponylang-ts-mode))))

(provide 'ponylang-ts-mode)

;;; ponylang-mode.el ends here
