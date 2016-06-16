;;; package -- Summary
;;; Commentary:
;;; Code:

(defun synelics/shell-exec-bash (&optional bash-args shell-file async)
  "Exec bash file."
  (interactive)
  (let ((current-dir (car (synelics/uppest-git-directory)))
        (cmd (if async
                 'async-shell-command
               'shell-command)))

    (if (and current-dir shell-file)
        (setq shell-file (concat current-dir shell-file))
      (setq shell-file (read-file-name "Bash file: " current-dir)))

    (funcall cmd (concat
                  "bash "
                  shell-file
                  " "
                  (or bash-args
                      (read-string "Please enter sh args: "))))))

(defun synelics/shell-exec-bash-async (&optional bash-args shell-file)
  "Exec bash file."
  (interactive)
  (synelics/shell-exec-bash bash-args shell-file 'async))

(defun synelics/dk-create-component ()
  "docstring"
  (interactive)
  (synelics/shell-exec-bash nil "cret-comp.sh"))

(provide 'ext-shell)
;;; ext-shell.el ends here
