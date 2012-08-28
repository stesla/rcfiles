;;; Get the same path for Emacs that we do for Bash
(let ((path (shell-command-to-string "/bin/bash --login -c 'echo -n \"$PATH\"'")))
  (setenv "PATH" path)
  (setq exec-path (split-string path ":")))


(let ((path (shell-command-to-string "/bin/bash --login -c 'echo -n \"$LD_LIBRARY_PATH\"'")))
  (setenv "LD_LIBRARY_PATH" path))
