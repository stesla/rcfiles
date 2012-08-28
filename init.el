(defconst st-init-dir (expand-file-name "init.d" user-emacs-directory))
(defconst st-elisp-dir (expand-file-name "elisp" user-emacs-directory))

(if (file-exists-p st-init-dir)
    (dolist (file (directory-files st-init-dir t "\\.el$"))
      (load file)))

(color-theme-zenburn)
(server-start)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(tab-width 4))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
