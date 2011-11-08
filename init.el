(defconst st-init-dir (expand-file-name "init.d" user-emacs-directory))
(defconst st-elisp-dir (expand-file-name "elisp" user-emacs-directory))

(if (file-exists-p st-init-dir)
    (dolist (file (directory-files st-init-dir t "\\.el$"))
      (load file)))

(color-theme-zenburn)
(server-start)
