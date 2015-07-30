(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")
        ("Tromey" . "http://tromey.com/elpa/")))

(package-initialize)

(defvar st-required-packages '(clojure-mode
                               color-theme
                               magit
                               paredit
                               slime))

(dolist (package st-required-packages)
  (when (not (package-installed-p package))
    (package-refresh-contents)
    (package-install package)))
