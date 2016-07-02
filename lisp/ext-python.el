;;; package -- Summary
;;; Commentary:
;;; Code:

(require-package 'anaconda-mode)
(require-package 'company-anaconda)

(require 'python)
(require 'anaconda-mode)

(add-hook 'python-mode-hook
          (lambda ()
            ;;; Advanced minor mode
            (anaconda-eldoc-mode)
            (anaconda-mode)

            ;;; Remove default completion
            (remove-hook 'completion-at-point-functions
                         #'python-completion-complete-at-point 'local)

            (sylc/py--custom-key-bindings)))

(require 'ext-company)
(require 'company-anaconda)
(synelics/company-add-backend 'python-mode 'company-anaconda)

;;; Override old key bindings
(sylc/base-exec-for-hook 'anaconda-mode-hook
                         (define-key anaconda-mode-map (kbd "M-.") 'synelics/py-goto-definition)
                         (define-key anaconda-mode-map (kbd "M-,") 'synelics/py-go-back)
                         (define-key anaconda-mode-map (kbd "M-?") 'comment-or-uncomment-region-or-line)
                         (define-key anaconda-mode-map (kbd "M-*") 'anaconda-mode-show-doc))

;;; Custom def
(defun sylc/py--custom-key-bindings ()
  "Custom 'python-mode key bindings."
  (cl-loop for (fn key cmd-arg) in '(('sylc/shell-exec-bash-async "r" ""))
           collect
           (sylc/base-add-keybinding key
                                     ;; Lexical binding, like closure in js
                                     `(lambda ()
                                        (interactive)
                                        (funcall ,fn "run.sh" ,cmd-arg))
                                     'local)))

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
