;;; package -- Summary
;;; Commentary:
;;;     For duokan work.
;;; Code:


(require 'js2-mode)

(defvar work--project-list
  '(("/home/vagrant/duokan/phone-v3/" 'sylc/work--custom-phone-key-bindings)
    ("/home/vagrant/webroot/store_hybrid/" nil)
    ("/home/vagrant/webroot/web/" nil)))

(dolist (hook '(js2-mode-hook))
  (add-hook hook 'sylc/work--custom-phone-key-bindings))

;; Highlight output of karma.
(add-hook 'comint-preoutput-filter-functions
          (lambda (string)
            (cl-loop for (replace-regexp ansi-color-code)
                     in '(("SUCCESS" "47")
                          (".*FAILED$" "208")
                          (".*Error: .*" "197"))
                     do
                     (setf string
                           (sylc/work--karma-color-filter string
                                                          replace-regexp
                                                          ansi-color-code)))
            (replace-regexp-in-string ";40" "" string)))

(defun sylc/work--karma-color-filter (filter-string replace-regexp ansi-color-code)
  "Custom shell filter with STRING."
  (replace-regexp-in-string replace-regexp
                            (lambda (match-string)
                              (concat "[38;5;" ansi-color-code "m" match-string "\033[m"))
                            filter-string
                            'fixedcase))

(defun sylc/work--custom-phone-key-bindings ()
  "docstring"
  (cl-loop for (work-directory custom-key-set-fn) in
           '(("/home/vagrant/duokan/phone-v3/" 'sylc/work--custom-phone-key-bindings)
             ("/home/vagrant/webroot/store_hybrid/" nil)
             ("/home/vagrant/webroot/web/" nil))
           do
           (and (eq work-directory (sylc/uppest-git-directory))
                custom-key-set-fn
                ;; (funcall custom-key-set-fn)
                (print work-directory)
                )))

(defun sylc/work--custom-phone-key-bindings ()
  "Custom key bindings of phone project."
  (cl-loop for (sh-fn cmd cmd-arg key) in
           '(('sylc/shell-exec-bash "cmd" nil "d")
             ('sylc/shell-exec-bash-async "cmd" "sync" "s")
             ('sylc/shell-exec-bash "cmd" "hotfix phone" "h")
             ('sylc/shell-exec-bash-async "cmd" "alpha phone" "a")
             ('sylc/shell-exec-bash-async "cmd" "publish phone" "p")
             ('with-editor-async-shell-command "bash ~/kits/bin/test-phone-v3" nil "t"))
           do
           (sylc/base-add-keybinding key
                                     ;; Lexical binding, like closure in js
                                     `(lambda ()
                                        (interactive)
                                        (funcall ,sh-fn ,cmd ,cmd-arg))
                                     'local)))

;; (defvar sylc/test "\n2                         #%2Ascratch%2A#3658kb6#  [0m[34;40mbrowser-fe[0m  [35;40mkits[0m           test.js\n#%2Ascratch%2A#21497Dc5#  #%2Ascratch%2A#7034X_7#  [34;40mduokan[0m      [34;40mlocal[0m          [34;40mtmp[0m\n#%2Ascratch%2A#24661pUU#  [34;40mbin[0m                      [34;40mfonts[0m       npm-debug.log  [35;40mwebroot[0m\n")

;; (defvar sylc/test-1 "testing \033[38;5;196;48;5;21mCOLOR1\033[38;5;208;48;5;159mCOLOR2\033[m")
;; (defvar sylc/test-2 "Chrome \033[38;5;83mMobile\033[m 48.0.2564 (Android 5.0.0): Executed 1 of 1 (1 FAILED) ERROR (0.122 secs / 0.009 secs)")


;; (defun sylc/shell-filter-color (string color-code)
;;   (replace-regexp-in-string color-code "" string))

;; (insert (xterm-color-filter "[38;5;235m235[m"))

;; (insert (xterm-color-filter
;;          (replace-regexp-in-string "([0-9]+ FAILED) ERROR"
;;                                    #'(lambda (match-string)
;;                                        (replace-regexp-in-string "ERROR"
;;                                                                  "[38;5;197mERROR\033[m"
;;                                                                  match-string
;;                                                                  'fixedcase))
;;                                    sylc/test-2
;;                                    'fixedcase)))


;; (replace-regexp-in-string "([0-9]+ FAILED) ERROR"
;;                           #'(lambda (match-string)
;;                               (replace-regexp-in-string "ERROR"
;;                                                         "[38;5;197mERROR\033[m"
;;                                                         match-string
;;                                                         'fixedcase))
;;                           sylc/test-2
;;                           'fixedcase)

;; (string-match "ERR.R" sylc/test-2)
;; (match-end 0)

;; (insert (xterm-color-filter "Chrome [38;5;83mMobile[m 48.0.2564 (Android 5.0.0): Executed 1 of 1 (1 FAILED) [38;5;228mERROR[m (0.122 secs / 0.009 secs)"))

;; (replace-regexp-in-string "ERROR" "[38;5;228mERROR\033[m" sylc/test-2)


;; ;; (loop for i from 0 to 255
;; ;;       do
;; ;;       (insert (xterm-color-filter (concat "\033[38;5;" (number-to-string i) "m" (number-to-string i) "\033[m   "))))

(provide 'ext-work)
;;; ext-work.el ends here
