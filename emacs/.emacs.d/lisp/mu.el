(defun delete-indentation-in-region (start end)
  "Remove all the indentation in the region by repeatedly calling DELETE-INDENTATION"
  (interactive "*r")
  (save-excursion
    (goto-char end)
    (while (> (point) start)
      (delete-indentation))))
