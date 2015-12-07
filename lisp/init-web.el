;;; package -- Summary
;;; Commentary:
;;; Code:

(setq-default js2-basic-offset 4)
(setq-default sgml-basic-offset 4)
(setq-default css-indent-offset 4)

(add-auto-mode 'html-mode ".tpl")

(add-hook 'js2-mode-hook
          (add-hook 'after-save-hook
                    (lambda ()
                      (and
                       (string-equal (file-name-extension (buffer-file-name)) "tpl")
                       (shell-command (concat "~/browser-fe/common/build/tpl.py " buffer-file-name " > /dev/null"))))))

(provide 'init-web)
;;; init-web.el ends here
