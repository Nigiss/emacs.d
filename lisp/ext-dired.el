;;; package -- Summary
;;; Commentary:
;;; Code:

(define-key dired-mode-map (kbd "c") 'syenlics/dired-new-file)

(defun syenlics/dired-new-file ()
  (interactive)
  (let* (
         (n 0)
         lawlist-filename
         (dired-buffer-name (buffer-name)))
    (catch 'done
      (while t
        (setq lawlist-filename (concat "untitled"
                                       (if (= n 0) "" (int-to-string n))
                                       ".txt"))
        (setq n (1+ n))
        (if (not (file-exists-p lawlist-filename))
            (throw 'done nil))))
    (message "[b]uffer + file (maybe) | [f]ile + buffer (maybe)")
    (let ((file-or-buffer (read-char-exclusive)))
      (cond
       ((eq file-or-buffer ?b)
        (switch-to-buffer (get-buffer-create lawlist-filename))
        (text-mode)
        (or (y-or-n-p (format "Save Buffer `%s'? "lawlist-filename))
            (error "Done."))
        (write-file lawlist-filename)
        (with-current-buffer dired-buffer-name
          (revert-buffer)))
       ((eq file-or-buffer ?f)
        (start-process "touch-file" nil "touch" lawlist-filename)
        (revert-buffer)
        (or (y-or-n-p (format "Open `%s'? "lawlist-filename))
            (error "Done."))
        (find-file lawlist-filename)
        (text-mode))
       (t (message "You have exited the function."))))))

(provide 'ext-dired)
;;; ext-dired.el ends here
