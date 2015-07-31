;; gofmt all the things
(add-hook 'before-save-hook 'gofmt-before-save)

;; goflymake - go get -u github.com/dougm/goflymake
(add-to-list 'load-path "~/golib/src/github.com/dougm/goflymake")
(require 'go-flymake)
(require 'go-flycheck)

;; gocode - go get -u github.com/nsf/gocode
(add-to-list 'load-path "~/golib/src/github.com/nsf/gocode/emacs-company")
(require 'company)
(require 'company-go)
(setq company-tooltip-limit 20)                      ; bigger popup window
(setq company-idle-delay .3)                         ; decrease delay before autocompletion popup shows
(setq company-echo-delay 0)                          ; remove annoying blinking
(setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing
(add-hook 'go-mode-hook (lambda ()
                          (set (make-local-variable 'company-backends) '(company-go))
                          (company-mode)))
