;;; package -- Summary
;;; Commentary:
;;; Code:

(require 'company)
(require 'css-mode)

(add-hook 'css-mode-hook
          (lambda ()
            (add-to-list 'company-backends 'company-css)))

(setq-default css-indent-offset 4)

(provide 'ext-css)
;;; ext-css.el ends here
