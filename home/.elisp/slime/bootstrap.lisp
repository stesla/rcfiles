;; Load Swank
(load (merge-pathnames ".elisp/slime/swank-loader" (user-homedir-pathname)))

;; Save image
(sb-ext:save-lisp-and-die "sbcl-with-slime.core")
