;;; package -- Summary
;;; Commentary:
;;; Code:

;;; Base config
(require-package 'evil-leader)
(require 'evil)
(require 'evil-leader)

(evil-mode t)
(global-evil-leader-mode)

(setq evil-leader/no-prefix-mode-rx '("magit-.*-mode" "gnus-.*-mode"))

;; (evil-leader/set-key-for-mode 'emacs-lisp-mode "b" 'byte-compile-file)
(define-key evil-normal-state-map (kbd "g / r") (lambda () (evil-ex "%s/")))

(evil-leader/set-key
  "x" 'smex
  "m" 'find-file
  "n" 'neotree-toggle
  "f" 'find-file-in-project
  "b" 'switch-to-buffer
  "c" 'comment-or-uncomment-region-or-line
  "w" 'save-buffer)

;; remap <c-u>
(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
(define-key evil-visual-state-map (kbd "C-u") 'evil-scroll-up)
(define-key evil-insert-state-map (kbd "C-u")
  (lambda ()
    (interactive)
    (evil-delete (point-at-bol) (point))))
(define-key evil-insert-state-map (kbd "C-h") 'paredit-backward-delete)

(define-key evil-normal-state-map (kbd "0") 'back-to-indentation-or-beginning)
(define-key evil-visual-state-map (kbd "0") 'back-to-indentation-or-beginning)

;;; goto
(define-key evil-normal-state-map (kbd "C-i") 'elisp-slime-nav-find-elisp-thing-at-point)

;;; window
;; (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
;; (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)

;;; esc quits
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-inactive-mode-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-inactive-mode-map [escape] 'minibuffer-inactive-mode-syntax-table)

(provide 'ext-evil)
;;; ext-evil.el ends here
