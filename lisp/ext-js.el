;;; package -- Summary
;;; Commentary:
;;; Code:

(require-package 'ac-js2)
(require-package 'js2-refactor)

(require 'js2-mode)
(require 'ac-js2)
(require 'js2-refactor)
(require 'ext-company)

(sylc/base-exec-for-hook 'js2-mode-hook
                         (ac-js2-mode)
                         (synelics/remove-from-list completion-at-point-functions 'ac-js2-completion-function)

                         (js2-refactor-mode)
                         (js2r-add-keybindings-with-prefix "C-c C-m"))


;;; Key bindings overlay
(sylc/base-exec-for-hook 'ac-js2-mode-hook
                         (define-key ac-js2-mode-map (kbd "M-.") 'sylc/js-goto-definition))

(after-load 'js2-mode
  (after-load 'company
    (add-hook 'js2-mode-hook
              (lambda () (sanityinc/local-push-company-backend 'ac-js2-company)))))

(setq-default js2-basic-offset 4)

(defun sylc/js-goto-definition ()
  "Use default first, if failed, then use TAGS."
  (interactive)
  (condition-case nil
      (ac-js2-jump-to-definition)
    (error
     (condition-case nil
         (js2-jump-to-definition)
       (error
        (synelics/find-tag))))))

(provide 'ext-js)
;;; ext-js.el ends here
