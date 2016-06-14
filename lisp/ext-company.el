;;; package -- Summary
;;; Commentary:
;;; Code:

(require 'company)

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

;;; Never company auto
(setq company-idle-delay nil)

;;; Key bindings
(define-key company-active-map (kbd "C-s") 'company-select-next)
(define-key company-active-map (kbd "C-r") 'company-select-previous)
(define-key company-mode-map (kbd "TAB") 'synelics/tab-indent-or-complete)

(defun synelics/tab-indent-or-complete ()
  "Smart tab, to indent, yas or complete."
  (interactive)
  (defun check-expansion ()
    (save-excursion
      (if (looking-at "\\_>") t
        (backward-char 1)
        (if (looking-at "\\.") t
          (backward-char 1)
          (if (looking-at "->") t nil)))))

  (defun do-yas-expand ()
    (let ((yas/fallback-behavior 'return-nil))
      (yas/expand)))

  (if (minibufferp)
      (minibuffer-complete)
    (if (or (not yas/minor-mode)
            (null (do-yas-expand)))
        (if (check-expansion)
            (synelics/ido-company)
          (indent-for-tab-command)))))

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
        (insert (ido-completing-read "Compeletion: " candidates))
        (delete-region (- complete-start-pos (length candidate-prefix)) pos)))))


;;; Bug: pos-tip-hide error
(require 'company-quickhelp)
(remove-hook 'after-init-hook 'company-quickhelp-mode)

(provide 'ext-company)
;;; ext-company.el ends here
