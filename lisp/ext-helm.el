;;; package --- helm-config

;;; Commentary:
(require-package 'helm)
(require-package 'helm-ls-git)
(require-package 'helm-swoop)

(require 'helm)
(require 'grep)
(require 'helm-grep)

;;; Code:
(helm-mode -1)
(helm-autoresize-mode 1)
(setq helm-autoresize-max-height 20)
(setq helm-autoresize-min-height 20)

(add-to-list 'helm-grep-ignored-directories "_build")
(add-to-list 'helm-grep-ignored-directories "dist-phone")
(add-to-list 'helm-grep-ignored-directories "dist-pad")
(add-to-list 'helm-grep-ignored-directories "dist-h5")
(add-to-list 'helm-grep-ignored-directories "duokan")
(add-to-list 'helm-grep-ignored-directories "dist*")

(defun synelics/helm-rgrep ()
  (interactive)
  (helm-do-grep-1 (Synelics/uppest-git-directory)
                  :recursive
                  nil
                  '("*.js" "*.css" "*.tpl" "*.py" "*.el")
                  nil
                  (grep-read-regexp)))

;;; Search by helm
;; (require-package 'company)
;; (require-package 'helm-company)
;; (global-set-key (kbd "C-c C-i") 'helm-company)

(defun synelics/helm-search ()
  (interactive)
  (require 'helm-swoop)
  (helm-swoop :$query (grep-read-regexp)))

;; (require-package 'company)
;; (require-package 'helm-company)
;; (global-set-key (kbd "C-c C-i") 'helm-company)

;; (global-set-key (kbd "C-x C-d") 'helm-ls-git-ls)
;; (global-set-key (kbd "C-x C-m") 'helm-M-x)
;; (global-set-key (kbd "C-x C-p") 'helm-projects-find-files)
;; (global-set-key (kbd "C-x C-b") 'helm-multi-files)
(global-set-key (kbd "C-M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-M-s") 'synelics/helm-search)

(provide 'ext-helm)
;;; ext-helm.el ends here
