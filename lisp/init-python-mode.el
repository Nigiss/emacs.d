(setq auto-mode-alist
      (append '(("SConstruct\\'" . python-mode)
		("SConscript\\'" . python-mode))
              auto-mode-alist))

(require-package 'pip-requirements)
(require-package 'anaconda-mode)
(require 'anaconda-mode)

(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)
(define-key anaconda-mode-map (kbd "M-,") #'anaconda-mode-go-back)
(define-key anaconda-mode-map (kbd "M-.") #'anaconda-mode-find-assignments)

(provide 'init-python-mode)
