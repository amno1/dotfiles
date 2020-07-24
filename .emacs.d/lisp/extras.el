;;; extras.el --- Some shortcut funcitons, usefull scripts etc -*- lexical-binding: t -*-
;;; most of stuff here is found on the www, emacs wiki or sx primiary sources,
;;; some just copy-paste, some modified
;;; some is written by me
(require 'cl-lib)

(defun enlarge-window-vertically (delta)
  "Make selected window DELTA columns wider.
Interactively, if no argument is given, make selected window one
column wider."
  (interactive "p")
  (enlarge-window delta nil))

(defun shrink-window-vertically (delta)
  "Make selected window DELTA columns narrower.
Interactively, if no argument is given, make selected window one
column narrower."
  (interactive "p")
  (shrink-window delta nil))

(defun kill-window-left()
  "Kills window on the left side of current window."
  (interactive)
  (delete-window (window-in-direction 'left)))

(defun kill-window-right()
  "Kills window on the right side of current window."
  (interactive)
  (delete-window (window-in-direction 'right)))

(defun kill-window-above()
  "Kills window above current window."
  (interactive)
  (delete-window (window-in-direction 'above)))

(defun kill-window-below()
  "Kills window below current window."
  (interactive)
  (delete-window (window-in-direction 'below)))

(defun kill-buffer-other-window ()
  "Kills buffer in other window."
  (interactive)
  (other-window 1)
  (kill-buffer)
  (other-window 1))

(defun silence-missing (func-name file-name)
  "Silence missing function warnings"
  (eval-when-compile
    (declare-function func-name (concat file-name ".el"))))

(defun sudo-find-file (file-name)
  "Like find file, but opens the file as root."
  (interactive "FSudo Find File: ")
  (let ((tramp-file-name (concat "/sudo::" (expand-file-name file-name))))
    (find-file tramp-file-name)))

(defun z-swap-windows () ""
       (interactive)
       (ace-swap-window)
       (aw-flip-window))

(defun only-current-buffer ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer
        (delq (current-buffer)
	      (cl-remove-if-not 'buffer-file-name (buffer-list)))))

(defun load-if-exists (f)
  "load the elisp file only if it exists and is readable"
  (if (file-readable-p f)
      (load-file f)))

(defun toggle-letter-case ()
  "Toggle the letter case of current word or text selection.
   Toggles between: “all lower”, “Init Caps”, “ALL CAPS”."
  (interactive)
  (let (p1 p2 (deactivate-mark nil) (case-fold-search nil))
    (if (region-active-p)
        (setq p1 (region-beginning) p2 (region-end))
      (let ((bds (bounds-of-thing-at-point 'word) ) )
        (setq p1 (car bds) p2 (cdr bds)) ) )
    (when (not (eq last-command this-command))
      (save-excursion
        (goto-char p1)
        (cond
         ((looking-at "[[:lower:]][[:lower:]]") (put this-command 'state "all lower"))
         ((looking-at "[[:upper:]][[:upper:]]") (put this-command 'state "all caps") )
         ((looking-at "[[:upper:]][[:lower:]]") (put this-command 'state "init caps") )
         ((looking-at "[[:lower:]]") (put this-command 'state "all lower"))
         ((looking-at "[[:upper:]]") (put this-command 'state "all caps") )
         (t (put this-command 'state "all lower") ) ) ) )
    (cond
     ((string= "all lower" (get this-command 'state))
      (upcase-initials-region p1 p2) (put this-command 'state "init caps"))
     ((string= "init caps" (get this-command 'state))
      (upcase-region p1 p2) (put this-command 'state "all caps"))
     ((string= "all caps" (get this-command 'state))
      (downcase-region p1 p2) (put this-command 'state "all lower")))))

(defun transform-square-brackets-to-round-ones(string-to-transform)
    "Transforms [ into ( and ] into ), other chars left unchanged."
    (concat
    (mapcar #'(lambda (c) (if (equal c ?\[) ?\( (if (equal c ?\]) ?\) c)))
            string-to-transform)))

;; from emacs-wiki @ https://www.emacswiki.org/emacs/RecentFiles
(defun undo-kill-buffer (arg)
  "Re-open the last buffer killed.  With ARG, re-open the nth buffer."
  (interactive "p")
  (let ((recently-killed-list (copy-sequence recentf-list))
	(buffer-files-list
	 (delq nil (mapcar (lambda (buf)
			     (when (buffer-file-name buf)
			       (expand-file-name (buffer-file-name buf)))) (buffer-list)))))
    (mapc
     (lambda (buf-file)
       (setq recently-killed-list
             (delq buf-file recently-killed-list)))
     buffer-files-list)
    (find-file
     (if arg (nth arg recently-killed-list)
       (car recently-killed-list)))))

(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (indent-buffer)
  (untabify-buffer)
  (delete-trailing-whitespace))

(defun my/quick-view-file-at-point ()
  "Preview the file at point then jump back after some idle time.

In order for this to work you need to bind this function to a key combo,
you cannot call it from the minibuffer and let it work.

The reason it works is that by holding the key combo down, you inhibit
idle timers from running so as long as you hold the key combo, the
buffer preview will still display."
  (interactive)
  (setq-local lexical-binding t)
  (let* ((buffer (current-buffer))
         (file (thing-at-point 'filename t))
         (file-buffer-name (format "*preview of %s*" file)))
    (if (and file (file-exists-p file))
        (let ((contents))
          (if (get-buffer file)
              (setq contents
                    (save-excursion
                      (with-current-buffer (get-buffer file)
                        (call-interactively #'font-lock-fontify-buffer)
                        (buffer-substring (point-min) (point-max)))))
            (let ((new-buffer (find-file-noselect file)))
              (with-current-buffer new-buffer
                (font-lock-mode t)
                (call-interactively #'font-lock-fontify-buffer)
                (setq contents (buffer-substring (point-min) (point-max))))
              (kill-buffer new-buffer)))
          (switch-to-buffer (get-buffer-create file-buffer-name))
          (setq-local header-line-format "%60b")
          (delete-region (point-min) (point-max))
          (save-excursion (insert contents))
          (local-set-key (kbd "C-M-v") (lambda () (interactive) (sit-for .2)))
          (run-with-idle-timer
           .7
           nil
           (lambda ()
             (switch-to-buffer buffer)
             (kill-buffer file-buffer-name))))
      (message "no file to preview at point!"))))

;; you can modify that list, to fit your needs
;; from emacs-wiki: https://www.emacswiki.org/emacs/KillingBuffers
(setq not-to-kill-buffer-list '("*scratch*" "#emacs" "*Messages*"))

(defun kill-buffer-but-not-some ()
  (interactive)
  (if (member (buffer-name (current-buffer)) not-to-kill-buffer-list)
      (bury-buffer)
    (kill-buffer (current-buffer))))

(defun signal-restart-server ()
  (interactive)
  (message "Caught event %S" last-input-event)
  (server-mode))

(defun find-duplicate-lines (&optional insertp interp)
  (interactive "i\np")
  (let ((max-pon (line-number-at-pos (point-max)))
        (gather-dups))
    (while (< (line-number-at-pos) max-pon) ;(= (forward-line) 0)
      (let ((this-line (buffer-substring-no-properties (line-beginning-position 1) (line-end-position 1)))
            (next-line (buffer-substring-no-properties (line-beginning-position 2) (line-end-position 2))))
        (when  (equal this-line next-line)  (setq gather-dups (cons this-line gather-dups)))))
    (if (or insertp interp)
        (save-excursion (newline) (princ gather-dups (current-buffer)))
      gather-dups)))

(defun uniquify-all-lines-buffer ()
  "Delete duplicate lines in buffer and keep first occurrence."
  (interactive "*")
  (uniquify-all-lines-region (point-min) (point-max)))

(defun uniquify-all-lines-region (start end)
  "Find duplicate lines in region START to END keeping first occurrence."
  (interactive "*r")
  (save-excursion
    (let ((lines) (end (copy-marker end)))
      (goto-char start)
      (while (and (< (point) (marker-position end))
                  (not (eobp)))
        (let ((line (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position))))
          (if (member line lines)
              (delete-region (point) (progn (forward-line 1) (point)))
            (push line lines)
            (forward-line 1)))))))

(defun remove-elc-on-save ()
  "If you're saving an elisp file, likely the .elc is no longer valid."
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))
            nil
            t))

(add-hook 'emacs-lisp-mode-hook 'remove-elc-on-save)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Automatically compile and save init files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun reload-emacs()
  "Reloads Emacs init config."
  (interactive)
  (load (concat user-emacs-directory "lisp/init.el")))

(defun update-emacs-packages ()
  (interactive)
  (auto-package-update-now)
  (package-quickstart-refresh)
  (reload-emacs))

(defun native-compile-file (filename &optional with-late-load)
  "Compile a FILE of Emacs Lisp code into native code.
This is the interactive entry-point for the Emacs Lisp native compiler.
FILE is a path to an Elisp file. If no FILE is given interactively, the
name of current buffer will be used as a FILE. When WITH-LATE-LOAD non
Nil mark the compilation unit for late load once finished compiling
(internal use only). Return the compilation unit file name."
  (interactive
   (let ((file buffer-file-name)
         (file-dir nil))
     (and file
          (derived-mode-p 'emacs-lisp-mode)
          (setq file-dir (file-name-directory file)))
     (list (read-file-name (if current-prefix-arg
			       "Native compile and load file: "
			     "Native compile file: ")
			   file-dir buffer-file-name nil)
	   current-prefix-arg)))

  ;; Expand now so we get the current buffer's defaults
  (setq filename (expand-file-name filename))
  
  ;; If we're compiling a file that's in a buffer and is modified, offer
  ;; to save it first.
  (or noninteractive
      (let ((b (get-file-buffer filename)))
        (if (and b (buffer-modified-p b)
	         (y-or-n-p (format "Save buffer %s first? " (buffer-name b))))
	    (with-current-buffer b (save-buffer)))))
  (native-compile filename with-late-load))

(defun org-tangle-and-compile-file (file &optional bcompile ncompile)
  "Load Emacs Lisp source code blocks in the Org FILE.
This function exports the source code using `org-babel-tangle'. With
optional prefix argument COMPILE, the tangled Emacs Lisp file is
byte-compiled before it is loaded."
  (interactive "fFile to load: \nP")
  (let* ((tangled-file (concat (file-name-sans-extension file) ".el")))
    ;; Tangle only if the Org file is newer than the Elisp file.
    (unless (org-file-newer-than-p
	     tangled-file
	     (file-attribute-modification-time (file-attributes file)))
      (org-babel-tangle-file file tangled-file "emacs-lisp"))
    (if (or bcompile ncompile)
	(progn
          (if ncompile
	      (native-compile-file tangled-file)
	    (byte-compile-file tangled-file))
          (package-quickstart-refresh)
	  (message "Tangled and compiled %s" tangled-file))
      (message "Tangled %s" tangled-file))))

(add-hook 'after-save-hook
          (function
           (lambda ()
             (if (string= buffer-file-name
                          (file-truename "~/.emacs.d/lisp/init.org"))
                 (org-tangle-and-compile-file "~/.emacs.d/lisp/init.org" t t)))))

(defun byte-compile-init-file (file)
  "Automatically compile FILE."
  (interactive)
  (save-restriction
    (setq byte-compile-warnings
          '(not free-vars obsolete unresolved callargs redefine
                obsolete noruntime cl-warnings interactive-only)))
  (byte-compile-file (expand-file-name file)))

(add-hook 'after-save-hook
          (function
           (lambda ()
             (if (string=  (file-truename "~/.emacs.d/init.el")
                           (file-truename (buffer-file-name)))
                 (byte-compile-init-file (file-truename "~/.emacs.d/init.el"))))))

(provide 'extras)
