;;; package -- Summary
;;; Commentary:
;;; Code:

;;; Set color in shell
(require-package 'xterm-color)
(require 'xterm-color)
(require 'shell)

(progn (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter)
       (setq comint-output-filter-functions (remove 'ansi-color-process-output
                                                    comint-output-filter-functions))
       (setq font-lock-unfontify-region-function 'xterm-color-unfontify-region))

(add-hook 'comint-preoutput-filter-functions
          (lambda (string)
            "Remove default bg-color in shell."
            (replace-regexp-in-string ";40" "" string)))

;;; Set editor for shell and eshell
(require 'with-editor)

(dolist (hook '(shell-mode-hook eshell-mode-hook))
  (add-hook hook
            'with-editor-export-git-editor))

;;; Use with-editor CMD to replace default
(define-key (current-global-map)
  [remap async-shell-command] 'with-editor-async-shell-command)
(define-key (current-global-map)
  [remap shell-command] 'with-editor-shell-command)

(defun sylc/shell-exec-bash (&optional shell-file bash-args async)
  "Exec bash file."
  (interactive)
  (let ((current-dir (file-name-directory buffer-file-name))
        (project-dir (sylc/uppest-git-directory))
        (sh-cmd (if async
                    'with-editor-async-shell-command
                  'with-editor-shell-command)))

    (cd project-dir)
    (if (and project-dir shell-file)
        (setq shell-file (concat project-dir shell-file))
      (setq shell-file (read-file-name "Bash file: " project-dir)))

    (funcall sh-cmd (concat
                     "bash "
                     shell-file
                     " "
                     (or bash-args
                         (read-string "Please enter sh args: "))))
    (cd current-dir)))

(defun sylc/shell-exec-bash-async (&optional shell-file bash-args)
  "Exec bash file."
  (interactive)
  (sylc/shell-exec-bash shell-file bash-args 'async))

(provide 'ext-shell)
;;; ext-shell.el ends here
