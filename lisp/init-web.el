;;; package -- Summary
;;; Commentary:
;;; Code:

(setq-default js2-basic-offset 4)
(setq-default sgml-basic-offset 4)
(setq-default css-indent-offset 4)

(add-auto-mode 'html-mode ".tpl")

(add-hook 'js2-mode-hook
          (lambda ()
            (progn
              (local-set-key (kbd "C-M-.") 'Synelics/find-tag))))

(add-hook 'html-mode-hook
          (lambda ()
            (progn
              (local-set-key (kbd "M-.") 'Synelics/find-tag)
              (add-hook 'after-save-hook
                        (lambda ()
                          (and
                           (string-equal (file-name-extension (buffer-file-name)) "tpl")
                           (shell-command (concat "~/browser-fe/common/build/tpl.py " buffer-file-name " > /dev/null"))))))))



;; (add-hook 'after-save-hook 'Synelics/update-tags-table)

(provide 'init-web)
;;; init-web.el ends here
