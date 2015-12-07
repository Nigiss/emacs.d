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

(let* ((base-dir (file-name-directory (buffer-file-name)))
       (home-dir (substring base-dir (string-match "\/.*?\/.*?\/" base-dir) (match-end 0))))
  (do ((curr-dir base-dir (substring curr-dir (string-match "\/.*\/" (substring curr-dir 0 -1)) (match-end 0))))
      ((equal nil (string-match "/home/.*?/" curr-dir)))
    (if (> (list-length (directory-files curr-dir nil ".*\.git$")) 0)
        (setq home-dir curr-dir)))
  (print home-dir))

;;; Search by helm
(require-package 'company)
(require-package 'helm-company)
(global-set-key (kbd "C-c C-i") 'helm-company)

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
