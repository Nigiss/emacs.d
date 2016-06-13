;;; package -- Summary
;;; Commentary:
;;; Code:

(require-package 'anaconda-mode)
(require-package 'company-anaconda)

(require 'anaconda-mode)

(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)
(add-hook 'python-mode-hook 'company-mode)

;;; Override old key bindings
(substitute-key-definition 'anaconda-mode-go-back 'anaconda-mode-show-doc anaconda-mode-map)
(substitute-key-definition 'anaconda-mode-find-assignments 'synelics/py-go-back anaconda-mode-map)
(substitute-key-definition 'anaconda-mode-find-definitions 'synelics/py-goto-definition anaconda-mode-map)
(substitute-key-definition 'anaconda-mode-show-doc 'comment-or-uncomment-region-or-line anaconda-mode-map)

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
