;;; package -- Summary
;;; Commentary:
;;; Code:

(require 'css-mode)
(require 'company-css)
(require 'ext-company)

(after-load 'css-mode
  (after-load 'company
    (add-hook 'css-mode-hook
              (lambda () (sanityinc/local-push-company-backend 'company-css)))))

(setq-default css-indent-offset 4)

(provide 'ext-css)
;;; ext-css.el ends here
