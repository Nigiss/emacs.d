;;; package -- Summary
;;; Commentary:
;;; Code:

(require 'css-mode)
(require 'company-css)
(require 'ext-company)

(synelics/company-add-backend 'css-mode 'company-css)

(setq-default css-indent-offset 4)

(provide 'ext-css)
;;; ext-css.el ends here
