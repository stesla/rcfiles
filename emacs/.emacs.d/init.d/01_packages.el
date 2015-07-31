;; emacs --script ~/.emacs.d/init.d/01_packages.el

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")
        ("Tromey" . "http://tromey.com/elpa/")))

(package-initialize)

(defvar st-required-packages '(clojure-mode
                               company
                               flycheck
                               go-mode
                               haml-mode
                               haskell-mode
                               highlight-parentheses
                               js2-mode
                               lua-mode
                               paredit
                               slime
                               yaml-mode))

(dolist (package st-required-packages)
  (when (not (package-installed-p package))
    (package-refresh-contents)
    (package-install package)))
