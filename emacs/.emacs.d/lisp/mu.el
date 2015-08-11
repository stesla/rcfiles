(defun delete-indentation-region (start end)
  "Remove all the indentation in the region"
  (interactive "*r")
  (save-excursion
    (goto-char end)
    (while (> (point) start)
      (delete-indentation))))

(defun delete-indentation-paragraph (arg)
  "Remove all indentation in the paragraph at POINT"
  (interactive "*p")
  (save-excursion
    (let ((end   (progn (forward-paragraph arg) (point)))
          (start (progn (backward-paragraph arg) (point))))
      (delete-indentation-region start end))))

(provide 'mu)
