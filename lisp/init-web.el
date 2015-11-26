;;; package -- Summary
;;; Commentary:
;;; Code:

;; (defun delete-tern-process ()
;;   (interactive)
;;   (delete-process "Tern"))

(setq-default js2-basic-offset 4)

;; (add-hook 'js2-mode-hook 'offset-config)
;; (add-hook 'js2-mode-hook (lambda () (tern-mode t)))
;; (eval-after-load 'tern
;;   '(progn
;;      (require 'tern-auto-complete)
;;      (tern-ac-setup)))

(defun offset-config ()
  "Modify keymaps used by 'js2-mode'."
  (setq tab-width 4))

(add-hook 'html-mode-hook 'offset-config)
(add-hook 'css-mode-hook 'offset-config)

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
