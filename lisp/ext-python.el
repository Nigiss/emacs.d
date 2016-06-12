;;; package -- Summary
;;; Commentary:
;;; Code:

(require-package 'anaconda-mode)
(require-package 'company)
(require-package 'company-anaconda)

(require 'anaconda-mode)

(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)

(provide 'ext-python)
;;; ext-python.el ends here
