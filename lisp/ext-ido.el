;;; package -- Summary
;;; Commentary:
;;; Code:

(require-package 'ido-vertical-mode)
(require-package 'ido-at-point)
(require 'ido-vertical-mode)
(require 'ido-at-point)

(ido-vertical-mode t)
(ido-at-point-mode t)

;;; Use ido mode to find files in git repo
(require-package 'find-file-in-project)
(require 'find-file-in-project)

(setq ffip-prefer-ido-mode t)

;; ;;; Key bindings for ido-mode
;; (add-hook 'ido-setup-hook
;;           (lambda ()
;;             (define-key ido-completion-map (kbd "TAB") 'ido-next-match)))

;;; Make Ido complete almost anything (except the stuff where it shouldn't)
(defvar ido-enable-replace-completing-read t
  "If t, use ido-completing-read instead of completing-read if possible.

    Set it to nil using let in around-advice for functions where the
    original completing-read is required.  For example, if a function
    foo absolutely must use the original completing-read, define some
    advice like this:

    (defadvice foo (around original-completing-read-only activate)
      (let (ido-enable-replace-completing-read) ad-do-it))")

;; Replace completing-read wherever possible, unless directed otherwise
(defadvice completing-read
    (around use-ido-when-possible activate)
  (if (or (not ido-enable-replace-completing-read) ; Manual override disable ido
          (and (boundp 'ido-cur-list)
               ido-cur-list)) ; Avoid infinite loop from ido calling this
      ad-do-it
    (let ((allcomp (all-completions "" collection predicate)))
      (if allcomp
          (setq ad-return-value
                (ido-completing-read prompt
                                     allcomp
                                     nil require-match initial-input hist def))
        ad-do-it))))

(provide 'ext-ido)
;;; ext-ido.el ends here
