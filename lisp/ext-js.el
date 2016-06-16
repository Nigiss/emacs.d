;;; package -- Summary
;;; Commentary:
;;; Code:

(require-package 'js2-refactor)

(require 'js2-mode)
(require 'js2-refactor)

(add-hook 'js2-mode-hook
          (lambda ()
            (js2-refactor-mode)
            (js2r-add-keybindings-with-prefix "C-c C-m")))

;;; Key bindings
(define-key js2-mode-map (kbd "M-.") 'synelics/js-goto-definition)

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
