(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(connection-local-criteria-alist
    '(((:application tramp)
        tramp-connection-local-default-system-profile tramp-connection-local-default-shell-profile)))
 '(connection-local-profile-alist
    '((tramp-connection-local-darwin-ps-profile
        (tramp-process-attributes-ps-args "-acxww" "-o" "pid,uid,user,gid,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state=abcde" "-o" "ppid,pgid,sess,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etime,pcpu,pmem,args")
        (tramp-process-attributes-ps-format
          (pid . number)
          (euid . number)
          (user . string)
          (egid . number)
          (comm . 52)
          (state . 5)
          (ppid . number)
          (pgrp . number)
          (sess . number)
          (ttname . string)
          (tpgid . number)
          (minflt . number)
          (majflt . number)
          (time . tramp-ps-time)
          (pri . number)
          (nice . number)
          (vsize . number)
          (rss . number)
          (etime . tramp-ps-time)
          (pcpu . number)
          (pmem . number)
          (args)))
       (tramp-connection-local-busybox-ps-profile
         (tramp-process-attributes-ps-args "-o" "pid,user,group,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "stat=abcde" "-o" "ppid,pgid,tty,time,nice,etime,args")
         (tramp-process-attributes-ps-format
           (pid . number)
           (user . string)
           (group . string)
           (comm . 52)
           (state . 5)
           (ppid . number)
           (pgrp . number)
           (ttname . string)
           (time . tramp-ps-time)
           (nice . number)
           (etime . tramp-ps-time)
           (args)))
       (tramp-connection-local-bsd-ps-profile
         (tramp-process-attributes-ps-args "-acxww" "-o" "pid,euid,user,egid,egroup,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state,ppid,pgid,sid,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etimes,pcpu,pmem,args")
         (tramp-process-attributes-ps-format
           (pid . number)
           (euid . number)
           (user . string)
           (egid . number)
           (group . string)
           (comm . 52)
           (state . string)
           (ppid . number)
           (pgrp . number)
           (sess . number)
           (ttname . string)
           (tpgid . number)
           (minflt . number)
           (majflt . number)
           (time . tramp-ps-time)
           (pri . number)
           (nice . number)
           (vsize . number)
           (rss . number)
           (etime . number)
           (pcpu . number)
           (pmem . number)
           (args)))
       (tramp-connection-local-default-shell-profile
         (shell-file-name . "/bin/sh")
         (shell-command-switch . "-c"))
       (tramp-connection-local-default-system-profile
         (path-separator . ":")
         (null-device . "/dev/null"))))
 '(load-prefer-newer t t nil "Customized with use-package emacs")
 '(package-selected-packages
    '(eglot elixir-ts-mode mermaid-mode elisp-autofmt consult vertico bind-key yasnippet-snippets super-save rainbow-delimiters dockerfile-mode corfu-doc kind-icon cape corfu orderless marginalia pkg-info yasnippet yaml-mode which-key web-mode use-package treemacs-projectile treemacs-magit treemacs-icons-dired tree-sitter-langs solaire-mode smartparens restclient reformatter rainbow-mode plantuml-mode pass ox-spectacle ox-gfm org-tree-slide org-superstar org-projectile ob-elixir multiple-cursors multi-vterm move-text lsp-ui kubernetes js2-mode helpful graphviz-dot-mode exunit exec-path-from-shell erlang elixir-mode editorconfig doom-themes doom-modeline docker diminish deadgrep all-the-icons))
 '(safe-local-variable-values '((st-rulers . [70]) (indent-tabs-mode . 1))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
