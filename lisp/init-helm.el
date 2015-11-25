;;; package --- helm-config

;;; Commentary:
(require-package 'helm)

;;; Code:
(helm-mode 1)
(helm-autoresize-mode 1)

;;; helm-do-grep recursive
(eval-after-load 'helm-grep
  '(setq helm-grep-default-command helm-grep-default-recurse-command))

(require-package 'helm-swoop)
(global-set-key (kbd "C-s") 'helm-swoop)

(global-set-key (kbd "C-x C-m") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'ido-find-file)
(global-set-key (kbd "C-x C-p") 'helm-projects-find-files)
(global-set-key (kbd "C-x C-b") 'helm-multi-files)
(global-set-key (kbd "M-'") 'helm-do-grep)
(global-set-key (kbd "M-Y") 'helm-show-kill-ring)

(provide 'init-helm)
;;; init-helm ends here