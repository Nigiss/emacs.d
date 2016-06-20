;;; package -- Summary
;;; Commentary:
;;; Code:

(require-package 'ac-js2)
(require-package 'js2-refactor)

(require 'js2-mode)
(require 'ac-js2)
(require 'js2-refactor)
(require 'ext-company)

(add-hook 'js2-mode-hook
          (lambda ()
            (ac-js2-mode)
            (setq completion-at-point-functions (delete 'ac-js2-completion-function completion-at-point-functions))

            (js2-refactor-mode)
            (js2r-add-keybindings-with-prefix "C-c C-m")))

(synelics/company-add-backend 'js2-mode 'ac-js2-company)

;;; Key bindings
(substitute-key-definition 'ac-js2-jump-to-definition
                           'synelics/js-goto-definition
                           js2-mode-map)

(setq-default js2-basic-offset 4)

(defun synelics/js-goto-definition ()
  "Use default first, if failed, then use TAGS."
  (interactive)
  (condition-case nil
      (js2-jump-to-definition)
    (error
     (synelics/find-tag))))

(provide 'ext-js)
;;; ext-js.el ends here
