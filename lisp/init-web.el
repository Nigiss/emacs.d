;;; package -- Summary
;;; Commentary:
;;; Code:

(setq-default js2-basic-offset 4)
(setq-default sgml-basic-offset 4)
(setq-default css-indent-offset 4)

(add-auto-mode 'html-mode ".tpl")

;;(shell-command "sudo apt-get install nodejs")
;;(shell-command "sudo npm install -g tern")
(require-package 'tern)
(require-package 'tern-auto-complete)

(defun delete-tern-process ()
  (interactive)
  (delete-process "Tern"))

(add-hook 'js2-mode-hook
          (lambda ()
            (tern-mode t)
            (add-hook 'after-save-hook
                      (lambda ()
                        (and
                         (string-equal (file-name-extension (buffer-file-name)) "tpl")
                         (shell-command (concat "~/Work/DKWeb/tpl.py " buffer-file-name " > /dev/null")))))))

(eval-after-load 'tern
  '(progn
     (require 'tern-auto-complete)
     (tern-ac-setup)))

(provide 'init-web)
;;; init-web.el ends here
