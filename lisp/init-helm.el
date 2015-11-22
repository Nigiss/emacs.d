;;; package --- helm-config

;;; Commentary:
(require 'helm)

;;; Code:
(helm-mode 1)
(helm-autoresize-mode 1)

(global-set-key (kbd "C-x C-m") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'ido-find-file)
(global-set-key (kbd "C-x C-p") 'helm-projects-find-files)
(global-set-key (kbd "C-x C-b") 'helm-multi-files)
(global-set-key (kbd "C-x C-d") 'dired)

(provide 'init-helm)
;;; init-helm ends here
