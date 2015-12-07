;;; package --- helm-config

;;; Commentary:
(require-package 'helm)
(require-package 'helm-ls-git)

(require 'helm)
(require 'grep)
(require 'helm-grep)
(require 'helm-swoop)


;;; Code:
(helm-mode 1)
(helm-autoresize-mode 1)

(add-to-list 'helm-grep-ignored-directories "_build")
(add-to-list 'helm-grep-ignored-directories "duokan")

(defun synelics/helm-rgrep ()
  (interactive)
  (helm-do-grep-1 '("/home/vagrant/webroot/pad-v2/phone-v2")
                  :recursive
                  nil
                  '("*.js" "*.tpl" "*.css")
                  nil
                  (grep-read-regexp)))

(let ((current-file-name (buffer-file-name (current-buffer))))
  (dolist (string-match "/.*/" current-file-name))
  (> (list-length (directory-files (substring current-file-name 0 (- (match-end 0) 1)) :MATCH ".git$")) 0))

(progn
  (string-match "/.*?/" "/home/vag/web/src")
  (match-end 0))

(string-ma)


(defun synelics/helm-search ()
  (interactive)
  (helm-swoop :$query ""))

;; (require-package 'company)
;; (require-package 'helm-company)
;; (global-set-key (kbd "C-c C-i") 'helm-company)

(global-set-key (kbd "C-x C-d") 'helm-ls-git-ls)
(global-set-key (kbd "C-x C-m") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'ido-find-file)
(global-set-key (kbd "C-x C-p") 'helm-projects-find-files)
(global-set-key (kbd "C-x C-b") 'helm-multi-files)
(global-set-key (kbd "C-M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-s") 'synelics/helm-search)

(provide 'init-helm)
;;; init-helm ends here
