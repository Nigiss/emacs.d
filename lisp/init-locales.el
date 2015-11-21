(defun sanityinc/utf8-locale-p (v)
  "Return whether locale string V relates to a UTF-8 locale."
  (and v (string-match "UTF-8" v)))

(defun sanityinc/locale-is-utf8-p ()
  "Return t iff the \"locale\" command or environment variables prefer UTF-8."
  (or (sanityinc/utf8-locale-p (and (executable-find "locale") (shell-command-to-string "locale")))
      (sanityinc/utf8-locale-p (getenv "LC_ALL"))
      (sanityinc/utf8-locale-p (getenv "LC_CTYPE"))
      (sanityinc/utf8-locale-p (getenv "LANG"))))

(when (or window-system (sanityinc/locale-is-utf8-p))
  (set-language-environment 'utf-8)
  (setq locale-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-selection-coding-system (if (eq system-type 'windows-nt) 'utf-16-le 'utf-8))
  (prefer-coding-system 'utf-8))

;; My configurations
;; Golbal
(global-set-key (kbd "M-/") 'undo-tree-redo)

(helm-mode 1)
(helm-autoresize-mode 1)
(window-numbering-mode 1)
(auto-save-mode nil)

(global-set-key (kbd "C-x C-m") 'helm-M-x)
(global-set-key (kbd "C-x C-b") 'ido-find-file)
(global-set-key (kbd "C-x C-f") 'helm-for-files)
(global-set-key (kbd "C-x C-p") 'helm-projects-find-files)
(global-set-key (kbd "C-x C-d") 'helm-find)

(add-to-list 'auto-mode-alist '("\\.tpl$" . html-mode))
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/molokai-theme")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/emacs-color-theme-solarized")

;; Local
(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))

(defun previous-line-end-and-newline-and-indent ()
  "Move curve to previous line end then newline and indent."
  (interactive)
  (move-beginning-of-line nil)
  (sanityinc/open-line-with-reindent 1))

(defun next-line-beginning-and-newline-and-indent ()
  "Move curve to next line beginnning then newline and indent."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

(defun back-to-indentation-or-beginning ()
  (interactive)
  (if (= (point) (progn (back-to-indentation) (point)))
      (beginning-of-line)))

(defun kill-current-line ()
  (interactive)
  (move-beginning-of-line nil)
  (kill-line 1)
  (back-to-indentation-or-beginning))

(defun delete-tern-process ()
  (interactive)
  (delete-process "Tern"))

(defun my-prog-config ()
  "Modify keymaps used by 'js2-mode'."
  (setq tab-width 4)
  (local-set-key (kbd "M-?") 'comment-or-uncomment-region-or-line)
  (local-set-key (kbd "C-M-j") 'next-line-beginning-and-newline-and-indent)
  (local-set-key (kbd "C-j") 'previous-line-end-and-newline-and-indent)
  (local-set-key (kbd "C-a") 'back-to-indentation-or-beginning)
  (local-set-key (kbd "M-'") 'rgrep)
  (local-set-key (kbd "M-k") 'kill-current-line)
  (local-set-key (kbd "C-M-p") 'scroll-down-line)
  (local-set-key (kbd "C-M-n") 'scroll-up-line))

(add-hook 'js2-mode-hook 'my-prog-config)
(add-hook 'js2-mode-hook (lambda () (tern-mode t)))
(eval-after-load 'tern
  '(progn
     (require 'tern-auto-complete)
     (tern-ac-setup)))
(add-hook 'html-mode-hook 'my-prog-config)
(add-hook 'css-mode-hook 'my-prog-config)

(add-hook 'eww-mode-hook
          (lambda ()
            (local-set-key (kbd "j") 'scroll-up-line)
            (local-set-key (kbd "k") 'scroll-down-line)
            (local-set-key (kbd "l") 'recenter-top-bottom)
            (local-set-key (kbd "]") 'w3m-next-buffer)
            (local-set-key (kbd "[") 'w3m-previous-buffer)
            (local-set-key (kbd "i") 'w3m-next-anchor)
            (local-set-key (kbd "o") 'w3m-previous-anchor)
            (local-set-key (kbd "t") 'w3m-create-empty-session)
            (local-set-key (kbd "x") 'w3m-delete-buffer)
            (local-set-key (kbd "r") 'w3m-reload-this-page)))

(defun my-browse-config ()
  (local-set-key (kbd "j") 'scroll-up-line)
  (local-set-key (kbd "k") 'scroll-down-line))


(add-hook 'Info-mode-hook 'my-browse-config)
(add-hook 'help-mode-hook 'my-browse-config)

(add-hook 'after-save-hook
          #'(lambda ()
              (and
               (string-equal (file-name-extension (buffer-file-name)) "tpl")
               (shell-command (concat "~/browser-fe/common/build/tpl.py " buffer-file-name " > /dev/null"))
               (message
                (concat "Saved as script: " buffer-file-name)))))

(provide 'init-locales)
