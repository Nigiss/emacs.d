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
(global-set-key (kbd "C-M-/") 'undo-tree-redo)
(global-set-key (kbd "M-?") 'comment-or-uncomment-region-or-line)
(global-set-key (kbd "C-M-j") 'next-line-beginning-and-newline-and-indent)
(global-set-key (kbd "C-j") 'previous-line-end-and-newline-and-inndent)
(global-set-key (kbd "C-a") 'back-to-indentation-or-beginning)
(global-set-key (kbd "M-'") 'rgrep)

(window-numbering-mode 1)

:; General
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

(defun info-config ()
  (local-set-key (kbd "j") 'scroll-up-line)
  (local-set-key (kbd "k") 'scroll-down-line))

(add-hook 'Info-mode-hook 'info-config)
(add-hook 'help-mode-hook 'info-config)

(provide 'init-locales)
