;;; package -- Summary
;;; Commentary:
;;; Code:

(require 'company)
(require 'sgml-mode)

(setq-default sgml-basic-offset 4)

(add-auto-mode 'html-mode ".tpl")

(define-key html-mode-map (kbd "M-.") 'synelics/find-tag)
(add-hook 'html-mode-hook
          (lambda ()
            (add-hook 'after-save-hook
                      (lambda ()
                        (and
                         (string-equal (file-name-extension (buffer-file-name)) "tpl")
                         (shell-command (concat "~/browser-fe/common/build/tpl.py " buffer-file-name " > /dev/null")))))))

(provide 'ext-html)
;;; ext-html.el ends here
