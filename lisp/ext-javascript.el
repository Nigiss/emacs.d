;;; package -- Summary
;;; Commentary:
;;; Code:

(require 'js2-mode)

(setq-default sgml-basic-offset 4)

(define-key js2-mode-map (kbd "M-.") 'synelics/js-goto-definition)

(defun synelics/js-goto-definition ()
  "Use default first, if failed, then use TAGS."
  (interactive)
  (condition-case nil
      (js2-jump-to-definition)
    (error
     (synelics/find-tag))))

(provide 'ext-javascript)
;;; ext-javascript.el ends here
