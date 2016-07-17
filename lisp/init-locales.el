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
(defvar sylc/base--key-prefix "")
(defvar sylc/base--key-prefix-local "")

(defmacro sylc/base-exec-for-hook (hook &rest body)
  `(add-hook ,hook
             #'(lambda ()
                 ,@body)
             'local))

;;; Test
;; (sylc/base-exec-for-hook 'css-mode-hook
;;                     (message "TEST ==> %s" 1)
;;                     (message "TEST ==> %s" 2))

(defun sylc/base-set-key-prefix (key-prefix &optional local)
  (if local
      (setf sylc/base--key-prefix-local key-prefix)
    (global-unset-key (kbd key-prefix))
    (setf sylc/base--key-prefix key-prefix)))

(defun sylc/base-add-keybinding (keybinding fn &optional local)
  (let ((set-key-fn 'global-set-key))
    (if local
        (setf set-key-fn 'local-set-key))
    (funcall set-key-fn
             (kbd (concat sylc/base--key-prefix
                          (if local
                              (concat " " sylc/base--key-prefix-local)
                            "")
                          keybinding))
             fn)))

(sylc/base-set-key-prefix "C-l ")
(sylc/base-set-key-prefix "cm" 'local)

(sylc/base-add-keybinding "cu" 'Synelics/upcase-char)
(sylc/base-add-keybinding "cd" 'Synelics/downcase-char)

(sylc/base-add-keybinding "pl" 'package-list-packages)
(sylc/base-add-keybinding "pi" 'package-install)

(sylc/base-add-keybinding "tc" 'sylc/base-generate-tags-table)
(sylc/base-add-keybinding "tv" 'visit-tags-table)
(sylc/base-add-keybinding "tr" 'tags-reset-tags-tables)

(sylc/base-add-keybinding "ma" 'mc/mark-all-symbols-like-this)

(sylc/base-add-keybinding "fr" (lambda ()
                                 (interactive)
                                 (revert-buffer t t)))

(sylc/base-add-keybinding "sa" (lambda ()
                                 (interactive)
                                 (ansi-term "/bin/zsh")))

(sylc/base-add-keybinding "se" 'eshell)
(sylc/base-add-keybinding "ss" 'shell)

(sylc/base-exec-for-hook 'emacs-lisp-mode-hook
                         (sylc/base-add-keybinding "r"
                                                   (lambda ()
                                                     "Update current el config."
                                                     (interactive)
                                                     (save-buffer)
                                                     (load-file buffer-file-name))
                                                   'local))

(sylc/base-exec-for-hook 'paredit-mode-hook
                         (define-key paredit-mode-map (kbd "C-j") 'ace-jump-word-mode)
                         (define-key paredit-mode-map (kbd "C-o") 'previous-line-end-and-newline-and-indent))

;;; Golbal key bindings
(global-set-key (kbd "M-/") 'undo-tree-redo)
(global-set-key (kbd "M-?") 'comment-or-uncomment-region-or-line)
(global-set-key (kbd "M-w") 'copy-selection-or-current-string)
(global-set-key (kbd "C-w") 'kill-region-or-current-line)
(global-set-key (kbd "C-M-w") 'del-current-string-and-paste)
(global-set-key (kbd "C-M-d") 'del-current-string)
(global-set-key (kbd "C-x f") 'find-file-in-project)
(global-set-key (kbd "M-k") 'copy-current-line)
(global-set-key (kbd "C-j") 'ace-jump-word-mode)
(global-set-key (kbd "C-o") 'previous-line-end-and-newline-and-indent)
(global-set-key (kbd "C-M-o") 'next-line-beginning-and-newline-and-indent)
(global-set-key (kbd "C-a") 'back-to-indentation-or-beginning)
(global-set-key (kbd "M-,") 'pop-tag-mark)
(global-set-key (kbd "M-'") 'synelics/helm-rgrep)
(global-set-key (kbd "C-;") 'eval-expression)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "C-M-s") 'paredit-splice-sexp)
(global-set-key (kbd "C-M-;") 'highlight-symbol)
(global-set-key (kbd "C-M-'") 'highlight-symbol-occur)
(global-set-key (kbd "C-M-.") 'find-tag)
(require-package 'evil)
(global-set-key (kbd "C-x C-q") 'evil-mode)
(global-set-key (kbd "C-x l") 'recenter-top-bottom)
(global-set-key (kbd "C-x C-m") 'smex)
(global-set-key (kbd "C-x k") (lambda ()
                                (interactive)
                                (kill-buffer)))

;;; Global settings
(menu-bar-mode -1)
(highlight-symbol-mode -1)
(global-auto-revert-mode 1)
(auto-save-mode nil)

;;; Auto update pkgs
(require-package 'auto-package-update)

;;; Gc freq
(setq gc-cons-threshold (* 512 1024 1024))

;;; Window switch
(require-package 'window-numbering)
(window-numbering-mode 1)

;;; Default theme
(require-package 'molokai-theme)
(load-theme 'molokai)

;;; Neo tree
;; (require-package 'neotree)
;; (add-hook 'neotree-mode-hook
;;           (lambda ()
;;             (local-set-key (kbd "j") 'next-line)
;;             (local-set-key (kbd "k") 'previous-line)
;;             (local-set-key (kbd "o") 'neotree-enter)
;;             (local-set-key (kbd "u") 'neotree-select-up-node)
;;             (local-set-key (kbd "c") 'neotree-create-node)
;;             (local-set-key (kbd "d") 'neotree-delete-node)
;;             (local-set-key (kbd "r") 'neotree-rename-node)
;;             (local-set-key (kbd "i") 'neotree-change-root)
;;             (local-set-key (kbd "n") 'neotree-select-next-sibling-node)
;;             (local-set-key (kbd "p") 'neotree-projectile-action)))

;;; Smooth scrolling
;; (require-package 'smooth-scrolling)
;; (smooth-scrolling-mode t)
;; (setq smooth-scroll-margin 1)

;;; YASnippet
(yas-global-mode t)

;;; Tags table
;; (setq tags-table-list
;;       '("~/webroot/phone-v2"))
;; (setq tags-file-name "~/webroot/phone-v2/TAGS")
;; (setq tags-revert-without-query 1)

;;; Ido mode
;; (defvar ido-cur-item nil)
;; (defvar ido-default-item nil)
;; (defvar ido-cur-list nil)

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

;;; General function
(defun sylc/uppest-git-directory ()
  "Get the uppest git directory name."
  (if buffer-file-name
      (let* ((base-dir (file-name-directory buffer-file-name))
             (home-dir (substring base-dir (string-match "\/.*?\/.*?\/" base-dir) (match-end 0))))
        (do ((curr-dir base-dir (substring curr-dir (string-match "\/.*\/" (substring curr-dir 0 -1)) (match-end 0))))
            ((equal nil (string-match "/home/.*?/" curr-dir)))
          (if (> (list-length (directory-files curr-dir nil ".*\.git$")) 0)
              (progn
                (setq home-dir curr-dir)
                (return home-dir)))))
    (car (cdr (split-string (pwd))))))

(defun sylc/base--project-file (file-name)
  (concat (sylc/uppest-git-directory) file-name))

(defun sylc/base--project-generate-tags-sh ()
  (sylc/base--project-file "gen-tags.sh"))

(defun sylc/base--project-tags-file ()
  (sylc/base--project-file "TAGS"))

(defun sylc/base-generate-tags-table ()
  "update tags table."
  (interactive)
  (shell-command (concat "bash " (sylc/base--project-generate-tags-sh))))

(defun sylc/base--tags-table-exists-p ()
  "Update tags table."
  (file-exists-p (sylc/base--project-tags-file)))

(defun synelics/find-tag ()
  "Find tag without confirm."
  (interactive)
  (let* ((begin-and-end-cons (begin-and-end-point-cons-of-current-string))
         (begin (car begin-and-end-cons))
         (end (cdr begin-and-end-cons)))

    (unless (sylc/base--tags-table-exists-p)
      (sylc/base-generate-tags-table))
    (visit-tags-table (sylc/base--project-tags-file) 'local)

    (find-tag (buffer-substring-no-properties begin end))
    ))

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

(defmacro synelics/remove-from-list (list-var element)
  "Remove element from list."
  `(setq ,list-var (delete ,element ,list-var)))

(provide 'init-locales)
