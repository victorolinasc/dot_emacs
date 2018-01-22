;;; asdf --- Summary:

;;; Commentary:
;;; Simple configuration of Emacs built in functions

;;; Code:

(require 'subr-x)

(defun grab-asdf-plugin-version (plugin-name)
  "Return the version of an ADSF plugin named PLUGIN-NAME."
  (when (executable-find "asdf")
      (nth 0 (split-string (shell-command-to-string (format "asdf current %s" plugin-name))))))

(defun grab-asdf-plugin-version-path (plugin-name)
  "Return the path of the given PLUGIN-NAME."
  (when (executable-find "asdf")
    (expand-file-name (grab-asdf-plugin-version plugin-name)
                      (string-trim-right (shell-command-to-string
                                          (format "asdf where %s" plugin-name)))
    )
  ))

(provide 'asdf)
;;; asdf.el ends here
