;; Trailing whitespace is evil, but evil is relative.
(setq-default show-trailing-whitespace t)

(defun st-hide-trailing-whitespace ()
  "Turn off trailing whitespace highlighting in this buffer."
  (interactive)
  (setq show-trailing-whitespace nil))

(mapc (lambda (mode-hook)
        (add-hook mode-hook 'st-hide-trailing-whitespace))
      '(Buffer-menu-mode-hook text-mode-hook
        custom-mode-hook term-mode-hook Info-mode-hook
        comint-mode-hook buffer-menu-mode-hook apropos-mode-hook
        tooltip-show-hook gnus-article-mode-hook mail-mode-hook
        gnus-summary-mode-hook message-mode-hook gnus-group-mode-hook
        eshell-mode-hook w3-mode-hook w3m-mode-hook help-modeq erc-mode))

(defun st-delete-trailing-whitespace ()
  "Delete all trailing whitespace in the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "[\t ]+$" nil t)
      (delete-region (match-beginning 0) (match-end 0)))))
