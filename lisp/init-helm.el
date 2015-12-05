;;; package --- helm-config

;;; Commentary:
(require-package 'helm)

;;; Code:
(helm-mode 1)
(helm-autoresize-mode 1)

;;; helm-do-grep recursive
(eval-after-load 'helm-grep
  '(setq helm-grep-default-command helm-grep-default-recurse-command))

;;; Search by helm
(require-package 'helm-swoop)
(global-set-key (kbd "C-s") 'helm-swoop)

(let* ((base-dir (file-name-directory (buffer-file-name)))
       (home-dir (substring base-dir (string-match "\/.*?\/.*?\/" base-dir) (match-end 0))))
  (do ((curr-dir base-dir (substring curr-dir (string-match "\/.*\/" (substring curr-dir 0 -1)) (match-end 0))))
      ((equal nil (string-match "/home/.*?/" curr-dir)))
    (if (> (list-length (directory-files curr-dir nil ".*\.git$")) 0)
        (setq home-dir curr-dir)))
  (return home-dir))

;;; Search by helm
(require-package 'company)
(require-package 'helm-company)
(global-set-key (kbd "C-c C-i") 'helm-company)

;;; Search current git repository
(require-package 'helm-ls-git)
(global-set-key (kbd "C-x C-d") 'helm-ls-git-ls)

(global-set-key (kbd "C-x C-m") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'ido-find-file)
(global-set-key (kbd "C-x C-p") 'helm-projects-find-files)
(global-set-key (kbd "C-x C-b") 'helm-multi-files)
(global-set-key (kbd "M-'") 'helm-do-grep)
(global-set-key (kbd "C-M-y") 'helm-show-kill-ring)

(provide 'init-helm)
;;; init-helm ends here
