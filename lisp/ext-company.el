;;; package -- Summary
;;; Commentary:
;;; Code:

(require 'company)

;; (require-package 'company-flx)
;; (with-eval-after-load 'company
;;   (company-flx-mode +1))

;;; Never company auto
(setq company-idle-delay nil)

;;; Key bindings
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)

(add-to-list 'completion-at-point-functions 'synelics/ido-company 'append)

(defun synelics/ido-company ()
  "Select `company-complete' candidates by `ido'."
  (interactive)

  ;;; Start complete
  (let (
        ;; Since company will auto complete common part
        (complete-start-pos (point))
        candidate-prefix candidates)

    ;;; Company first
    (unless company-candidates
      (company-complete))

    ;;; Then record states, and quit default frontend
    (when company-point
      (setq candidate-prefix company-prefix)
      (setq candidates company-candidates)
      (company-abort))

    ;;; Show as ido
    (when candidates
      (let ((pos (point)))
        (insert (ido-completing-read "Compeletion: " candidates nil nil candidate-prefix))
        (if (> (length candidate-prefix) 0)
            (delete-region (- complete-start-pos (length candidate-prefix)) pos))))
    ))

;;; Custom company theme
(deftheme molokai-overrides)

(custom-theme-set-faces
 'molokai-overrides

 ;; Additional modes
 ;; Company tweaks.
 `(company-tooltip
   ((t :inherit default
       :background "#403D3D")))

 `(company-scrollbar-bg
   ((t :background "#232526")))

 `(company-scrollbar-fg
   ((t :background "#888888")))

 `(company-tooltip-selection
   ((t :inherit font-lock-function-name-face)))

 `(company-tooltip-common
   ((t :inherit font-lock-constant-face)))

 '(font-lock-comment-face ((t (:foreground "#888888" :slant italic)))))

;;; Bug: pos-tip-hide error
(require 'company-quickhelp)
(remove-hook 'after-init-hook 'company-quickhelp-mode)

(provide 'ext-company)
;;; ext-company.el ends here
