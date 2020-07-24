;;; persona-extras.el --- Personal extensions

;;;###autoload
(defun only-current-buffer () 
  (interactive)                                                                   
    (mapc 'kill-buffer (cdr (buffer-list (current-buffer)))))

(provide 'personal-extras)
