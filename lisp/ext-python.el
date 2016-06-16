;;; package -- Summary
;;; Commentary:
;;; Code:

(require-package 'anaconda-mode)
(require-package 'company-anaconda)

(require 'company)
(require 'anaconda-mode)
(require 'company-anaconda)

;; (add-hook 'python-mode-hook 'anaconda-eldoc-mode)
;; (add-hook 'python-mode-hook 'anaconda-mode)

(add-hook 'python-mode-hook
          (lambda ()
            ;;; Advanced minor mode
            (anaconda-eldoc-mode)
            (anaconda-mode)

            ;;; Remove default completion
            (remove-hook 'completion-at-point-functions
                         #'python-completion-complete-at-point 'local)

            ;;; Config complete backend
            (add-to-list 'company-backends 'company-anaconda)))

;; (add-hook 'after-change-major-mode-hook
;;           (lambda ()
;;             (print major-mode)
;;             (print (eq major-mode 'python-mode))
;;             ;; (unless (eq major-mode 'python-mode)
;;             ;;   (delete 'company-anaconda company-backends))
;;             ))


;;; Override old key bindings
(substitute-key-definition 'anaconda-mode-go-back 'anaconda-mode-show-doc anaconda-mode-map)
(substitute-key-definition 'anaconda-mode-find-assignments 'synelics/py-go-back anaconda-mode-map)
(substitute-key-definition 'anaconda-mode-find-definitions 'synelics/py-goto-definition anaconda-mode-map)
(substitute-key-definition 'anaconda-mode-show-doc 'comment-or-uncomment-region-or-line anaconda-mode-map)

;;; Custom def
(defun synelics/py-goto-definition ()
  "Find definition first, then assignment, if failed, last use TAGS."
  (interactive)
  (anaconda-mode-call "goto_definitions"
                      (lambda (result)
                        (if result
                            (anaconda-mode-definitions-view result)
                          (anaconda-mode-call "goto_assignments"
                                              (lambda (result)
                                                (if result
                                                    (anaconda-mode-definitions-view result)
                                                  (synelics/find-tag))))))))

(defun synelics/py-go-back ()
  "Go back."
  (interactive)
  (if anaconda-mode-go-back-definitions
      (anaconda-mode-find-file-no-record-definition (pop anaconda-mode-go-back-definitions))
    (pop-tag-mark)))

(provide 'ext-python)
;;; ext-python.el ends here
