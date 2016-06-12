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

;;; My configurations

;;; Golbal key bindings
(global-set-key (kbd "C-M-_") 'undo-tree-redo)
(global-set-key (kbd "M-?") 'comment-or-uncomment-region-or-line)
(global-set-key (kbd "M-w") 'copy-selection-or-current-string)
(global-set-key (kbd "C-w") 'kill-region-or-current-line)
(global-set-key (kbd "C-M-w") 'del-current-string-and-paste)
(global-set-key (kbd "C-M-d") 'del-current-string)
(global-set-key (kbd "C-x f") 'find-file-in-project)
(global-set-key (kbd "M-k") 'copy-current-line)
(global-set-key (kbd "C-M-j") 'next-line-beginning-and-newline-and-indent)
(global-set-key (kbd "C-j") 'previous-line-end-and-newline-and-indent)
(global-set-key (kbd "C-a") 'back-to-indentation-or-beginning)
(global-set-key (kbd "M-,") 'pop-tag-mark)
(global-set-key (kbd "M-'") 'synelics/helm-rgrep)
(global-set-key (kbd "C-;") 'eval-expression)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "C-M-;") 'highlight-symbol)
(global-set-key (kbd "C-M-'") 'highlight-symbol-occur)
(global-set-key (kbd "C-M-.") 'find-tag)
(global-set-key (kbd "C-x k") (lambda ()
                                (interactive)
                                (kill-buffer)))

;;; Custom
(global-unset-key (kbd "C-x l"))
(global-set-key (kbd "C-x l u") 'Synelics/upcase-char)
(global-set-key (kbd "C-x l d") 'Synelics/downcase-char)
(global-set-key (kbd "C-x l l") 'package-list-packages)
(global-set-key (kbd "C-x l i") 'package-install)
(global-set-key (kbd "C-x l p") 'switch-to-prev-buffer)
(global-set-key (kbd "C-x l n") 'switch-to-next-buffer)
(global-set-key (kbd "C-x l v") 'visit-tags-table)
(global-set-key (kbd "C-x l t") 'Synelics/update-tags-table)
(global-set-key (kbd "C-x l m") 'mc/mark-all-symbols-like-this)
(global-set-key (kbd "C-x l f") (lambda ()
                                  (interactive)
                                  (revert-buffer t t)))
(global-set-key (kbd "C-x l j") 'ace-jump-mode)
(global-set-key (kbd "C-x l e") 'shell)
(global-set-key (kbd "C-x l a") (lambda ()
                                  (interactive)
                                  (ansi-term "/bin/zsh")))
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (local-set-key (kbd "C-x l r")
                           (lambda ()
                             (interactive)
                             (save-buffer)
                             (load-file buffer-file-name)))))

;;; Global settings
(menu-bar-mode -1)
(highlight-symbol-mode -1)
(global-auto-revert-mode 1)
(auto-save-mode nil)

;;; Gc freq
(setq gc-cons-threshold (* 1024 1024 1024))

;;; Window switch
(require-package 'window-numbering)
(window-numbering-mode 1)

;;; Default theme
(require-package 'molokai-theme)
(load-theme 'molokai)

;;; Neo tree
(require-package 'neotree)
(add-hook 'neotree-mode-hook
          (lambda ()
            (local-set-key (kbd "j") 'next-line)
            (local-set-key (kbd "k") 'previous-line)
            (local-set-key (kbd "o") 'neotree-enter)
            (local-set-key (kbd "u") 'neotree-select-up-node)
            (local-set-key (kbd "c") 'neotree-create-node)
            (local-set-key (kbd "d") 'neotree-delete-node)
            (local-set-key (kbd "r") 'neotree-rename-node)
            (local-set-key (kbd "i") 'neotree-change-root)
            (local-set-key (kbd "n") 'neotree-select-next-sibling-node)
            (local-set-key (kbd "p") 'neotree-projectile-action)))

;;; Smooth scrolling
(require-package 'smooth-scrolling)
(smooth-scrolling-mode t)
(setq smooth-scroll-margin 1)

;;; YASnippet
(yas-global-mode t)

;;; Tags table
;; (setq tags-table-list
;;       '("~/webroot/phone-v2"))
(setq tags-file-name "~/webroot/phone-v2/TAGS")
(setq tags-revert-without-query 1)

(defvar ido-cur-item nil)
(defvar ido-default-item nil)
(defvar ido-cur-list nil)
(global-flex-isearch-mode t)

;;; Line number
(global-linum-mode 1)
(set-face-foreground 'linum "#888")
(set-face-background 'linum "#1c1c1c")
(setq linum-format "%3d ")

;;; Paren match
(custom-set-faces
 '(show-paren-match ((t (:background "#fd971f" :foreground "#fff")))))

;;; Display time
(setq display-time-24hr-format t
      display-time-day-and-date t
      display-time-use-mail-icon t
      display-time-interval 10
      display-time-format "%H:%M %a")
(display-time-mode 1)

;;; Revert buffer
(global-auto-revert-mode 1)

:; General function
(defun Synelics/server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server"
  (interactive)
  (kill-emacs))


(defun Synelics/uppest-git-directory ()
  "Get the uppest git directory name."
  (let* ((base-dir (file-name-directory (buffer-file-name)))
         (home-dir (substring base-dir (string-match "\/.*?\/.*?\/" base-dir) (match-end 0))))
    (do ((curr-dir base-dir (substring curr-dir (string-match "\/.*\/" (substring curr-dir 0 -1)) (match-end 0))))
        ((equal nil (string-match "/home/.*?/" curr-dir)))
      (if (> (list-length (directory-files curr-dir nil ".*\.git$")) 0)
          (progn
            (setq home-dir curr-dir)
            (return (list home-dir)))))))

(defun Synelics/update-tags-table ()
  "Update TAGS table."
  (interactive)
  (shell-command (concat "bash " (car (Synelics/uppest-git-directory)) "gen-tags.sh")))

(defun synelics/find-tag ()
  "Find tag without confirm."
  (interactive)
  (let* ((begin-and-end-cons (begin-and-end-point-cons-of-current-string))
         (begin (car begin-and-end-cons))
         (end (cdr begin-and-end-cons)))
    (find-tag (buffer-substring-no-properties begin end))))

(defun Synelics/upcase-char ()
  "Upcase current char and forward."
  (interactive)
  (let ((beg (point))
        end)
    (forward-char 1)
    (setq end (point))
    (upcase-region beg end)))

(defun Synelics/downcase-char ()
  "Downcase current char and forward."
  (interactive)
  (let ((beg (point))
        end)
    (forward-char 1)
    (setq end (point))
    (downcase-region beg end)))


(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))

(defun begin-and-end-point-cons-of-current-string ()
  (let ((cur (point))
        (reg "[^0-9-$a-zA-Z_/]")
        beg
        end)
    (cond ((region-active-p)
           (setq beg (region-beginning) end (region-end)))
          ((not (equal (word-at-point) nil))
           (progn
             (setq beg (+ (re-search-backward reg) 1))
             (forward-char 1)
             (setq end (- (re-search-forward reg) 1))
             (backward-char 1)))
          (t
           (setq beg cur end cur)))
    (progn
      (goto-char cur)
      (cons beg end))))


(defun copy-selection-or-current-string ()
  (interactive)
  (if (region-active-p)
      (setq beg (region-beginning) end (region-end))
    (setq beg (car (begin-and-end-point-cons-of-current-string)) end (cdr (begin-and-end-point-cons-of-current-string))))
  (copy-region-as-kill beg end))

(defun kill-region-or-current-line ()
  (interactive)
  (if (not (region-active-p))
      (progn
        (beginning-of-line)
        (cua-set-mark)
        (end-of-line)
        (next-line)
        (beginning-of-line)))
  (kill-region (region-beginning) (region-end)))

(defun del-current-string-and-paste ()
  (interactive)
  (let* ((begin-and-end-cons (begin-and-end-point-cons-of-current-string))
         (begin (car begin-and-end-cons))
         (end (cdr begin-and-end-cons)))
    (delete-region begin end)
    (yank)))

(defun del-current-string ()
  (interactive)
  (let* ((begin-and-end-cons (begin-and-end-point-cons-of-current-string))
         (begin (car begin-and-end-cons))
         (end (cdr begin-and-end-cons)))
    (delete-region begin end)))

(defun copy-current-line ()
  (interactive)
  (let ((cur (point))
        beg
        end)
    (back-to-indentation)
    (setq beg (point))
    (move-end-of-line 1)
    (setq end (point))
    (copy-region-as-kill beg end)
    (goto-char cur)))

(defun previous-line-end-and-newline-and-indent ()
  "Move curve to previous line end then newline and indent."
  (interactive)
  (back-to-indentation-or-beginning)
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

(defun info-config ()
  (local-set-key (kbd "j") 'scroll-up-line)
  (local-set-key (kbd "k") 'scroll-down-line)
  (local-set-key (kbd "n") 'forward-button)
  (local-set-key (kbd "p") 'backward-button))

(add-hook 'Info-mode-hook 'info-config)
(add-hook 'help-mode-hook 'info-config)

(provide 'init-locales)
