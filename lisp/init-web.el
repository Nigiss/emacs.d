;;; package -- Summary
;;; Commentary:
;;; Code:

;; (defun delete-tern-process ()
;;   (interactive)
;;   (delete-process "Tern"))

(setq-default js2-basic-offset 4)
(setq-default sgml-basic-offset 4)
(setq-default css-indent-offset 4)

(add-auto-mode 'html-mode ".tpl")

(add-hook 'js2-mode-hook
          (lambda ()
            (add-hook 'after-save-hook
                      (lambda ()
                        (and
                         (string-equal (file-name-extension (buffer-file-name)) "tpl")
                         (shell-command (concat "~/Work/DKWeb/tpl.py " buffer-file-name " > /dev/null")))))))


(provide 'init-web)
;;; init-web.el ends here
