(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (mouse-wheel-mode t)
  (blink-cursor-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1))

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(ansi-color-for-comint-mode-on)

(defalias 'yes-or-no-p 'y-or-n-p)

(setq color-theme-is-global t
      confirm-kill-emacs 'yes-or-no-p
      echo-keystrokes 0.1
      font-lock-maximum-decoration t
      inhibit-startup-message t
      kill-whole-line t
      require-final-newline t
      transient-mark-mode t
      uniquify-buffer-name-style 'forward
      x-select-enable-clipboard t
      visible-bell t)

(set-default 'indent-tabs-mode nil)
(set-default 'indicate-empty-lines t)

;; I use these all the time, enable them by default.
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; Transparently open compressed files
(auto-compression-mode t)

;; Automatically pick up changes on the filesystem
(global-auto-revert-mode 1)

;; Enable syntax highlighting for older Emacsen that have it off
(global-font-lock-mode t)

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

;; Show the column number in the modeline
(column-number-mode 1)

 ;; Seed the random-number generator
(random t)

;; Platform-specific stuff
(when (eq system-type 'darwin)
  ;; Work around a bug on OS X where system-name is FQDN
  (setq system-name (car (split-string system-name "\\."))))

;; gofmt all the things
(add-hook 'before-save-hook #'gofmt-before-save)

