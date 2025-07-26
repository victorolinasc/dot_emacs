;;; -*- lexical-binding: t -*-

;;; Commentary:
;;; My useful functions :)

(require 'yaml)

(defun vn/init-el ()
  "Opens init.el"
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun vn/earyly-init-el ()
  "Opens early-init.el"
  (interactive)
  (find-file "~/.emacs.d/early-init.el"))

(defun vn/core-emacs-config-el ()
  "Opens core-emacs-config.el"
  (interactive)
  (find-file "~/.emacs.d/lisp/core-emacs-config.el"))

(defun vn/core-emacs-packages-config-el ()
  "Opens core-emacs-packages-config.el"
  (interactive)
  (find-file "~/.emacs.d/lisp/core-emacs-packages-config.el"))

(defun vn/external-packages-config-el ()
  "Opens external-packages-config.el"
  (interactive)
  (find-file "~/.emacs.d/lisp/external-packages-config.el"))

(defun vn/sqls_config ()
  "Opens SQLS configuration file"
  (interactive)
  (find-file "~/.config/sqls/config.yml"))

(defun vn/sql-connect-with-compose (service)
  "Opens a SQL connection to a database configured in compose.yml with name SERVICE"
  (interactive "sDatabase service name: ")
  (progn
    (let* ((root (project-root (project-current)))
           (compose-file (expand-file-name (concat root "compose.yaml")))
           (compose-content
            (with-temp-buffer
              (insert-file-contents compose-file)
              (buffer-string)))
           (compose (yaml-parse-string compose-content))
           (service (gethash (intern service) (gethash 'services compose)))
           (envs (gethash 'environment service))
           (user (gethash 'POSTGRES_USER envs))
           (pass (gethash 'POSTGRES_PASSWORD envs))
           (database (gethash 'POSTGRES_DB envs)))
      (setq-local sql-product 'postgres)
      (setq-local sql-database (concat "postgres://" user ":" pass "@localhost:5432" "/" database))
      (sql-postgres))))

(setopt sql-connection-alist
        '((local-fw
           (sql-product 'postgres)
           (sql-server "localhost")
           (sql-user "postgres")
           (sql-password "postgres")
           (sql-database "financedwalletdb")
           (sql-port 5432))))

(defun vn/sql-connect (connection)
  "Connects to local CONNECTION"
  (interactive "sConnection name: ")
  (sql-connect connection))

;;; vn.el ends here
