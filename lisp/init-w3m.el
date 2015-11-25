;;; package -- Summary
;;; Commentary:
;;; Code:
(require-package 'w3m)

(add-hook 'w3m-mode-hook
          (lambda ()
            (local-set-key (kbd "n") 'w3m-next-anchor)
            (local-set-key (kbd "p") 'w3m-previous-anchor)
            (local-set-key (kbd "/") 'isearch-forward)))

(provide 'init-w3m)
;;; init-w3m.el ends here
