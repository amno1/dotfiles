;;; logpad.el --- Simulate Windows Notepad for logging.

;; Copyright (C) 2018 Jens K. Loewe

;; Author: Jens K. Loewe <git@tuxproject.de>
;; URL: https://bitbucket.org/tux_/logpad.el
;; Keywords: files outlines notepad
;; Version: 1.0.0

;; This work is free. You can redistribute it and/or modify it under the
;; terms of the Do What The Fuck You Want To Public License, Version 2,
;; as published by Sam Hocevar. See http://www.wtfpl.net/ for more details.

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; This minor mode transfers the logging mechanisms of Windows Notepad
;; to GNU Emacs. It will be enabled whenever you open a text file which
;; starts with ".LOG".

;;; Code:

;; --------------------
;; Configuration stuff:
;; --------------------


(defgroup logpad nil
  "Customize the Notepad logging simulation."
  :prefix "logpad-"
  :group 'text)


(defcustom logpad-add-newlines nil
  "Non-nil fully simulates Notepad, otherwise, additional newlines will be added for look-and-feel reasons."
  :type 'boolean
  :group 'logpad)


(defcustom logpad-add-to-end t
  "If set to true, new log entries will be added to the end of the file.
Otherwise, they will be added right below the .LOG line."
  :type 'boolean
  :group 'logpad)


;; ------------
;; Actual code:
;; ------------


(defun logpad--get-nth-line (number)
  "Return the NUMBERth line in the current buffer."
  (save-restriction
    (widen)
    (save-excursion
      (with-no-warnings
        (goto-line number))
      (buffer-substring-no-properties (line-beginning-position)
                                      (line-end-position)))))


(defun logpad--insert-date ()
  "Insert the current date/time."
  (insert (format-time-string "%c")))


(defun logpad--newline-with-jump ()
  "Add a new line to the end of wherever we are and go there."
  (end-of-line)
  (open-line 1)
  (forward-line))


(defun logpad--enable-logging ()
  "This is where the magic happens."
  ;; Display the lighter, at least.
  (logpad-mode)

  ;; This will move the user's cursor.
  ;; This is to be expected, after all.
  (with-no-warnings
    (if logpad-add-to-end
        ;; Add a new entry to the end of the file.
        (goto-char (point-max))

      ;; Add a new entry after the first line.
      (goto-line 1))
    (logpad--newline-with-jump)
    (when logpad-add-newlines
      (logpad--newline-with-jump)) ;; Leave one empty line before the new date.
    (logpad--insert-date)
    (logpad--newline-with-jump))

  (message "Logpad applied to '%s'." (buffer-name)))


(defun logpad--check-for-log ()
  "Try to determine whether the current file is a log file."
  (when (string= (logpad--get-nth-line 1) ".LOG")
    ;; Perform the magic.
    (logpad--enable-logging)))


(define-minor-mode logpad-mode
  "Simulate Windows Notepad for logging."
  :lighter " Logpad")


;; Automatically check whether we're in a log file:
(add-hook 'text-mode-hook (lambda () (logpad--check-for-log)))


(provide 'logpad)
;;; logpad.el ends here
