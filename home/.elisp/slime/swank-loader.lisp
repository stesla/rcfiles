(if (not (find-package 'swank-loader))
    ;; Edit SLIME-DIR to be where you have SLIME installed.
    (let ((slime-dir (merge-pathnames ".elisp/slime/" (user-homedir-pathname))))
      (load (merge-pathnames "swank-loader-original" slime-dir))))
