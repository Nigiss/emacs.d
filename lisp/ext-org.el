;;; package -- Summary
;;; Commentary:
;;; Code:

(require 'org)
(add-hook 'org-mode-hook 'sylc/org--custom-key-bindings)

(defun sylc/org--custom-key-bindings ()
  "Custom key bindings for org mode."
  (sylc/base-add-keybinding "w"
                            #'(lambda ()
                                (interactive)
                                (shell-command (concat "cp week-template.org "
                                                       (read-string "Please enter week number: ")
                                                       ".org")))
                            'local))

(provide 'ext-org)
;;; ext-org.el ends here
