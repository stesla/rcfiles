(defface st-paren-face
   '((((class color) (background dark))
      (:foreground "grey50"))
     (((class color) (background light))
      (:foreground "grey55")))
   "Face used to dim parentheses."
   :group 'st-faces)

(autoload 'enable-paredit-mode "paredit")

(dolist (x '(scheme emacs-lisp lisp clojure))
  ;; dim parens
  (when window-system
    (font-lock-add-keywords
     (intern (concat (symbol-name x) "-mode"))
     '(("(\\|)" . 'st-paren-face))))
  ;; turn on paredit
  (add-hook
   (intern (concat (symbol-name x) "-mode-hook")) 'enable-paredit-mode))
