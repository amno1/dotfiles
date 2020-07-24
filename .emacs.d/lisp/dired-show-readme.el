;;; dired-show-readme.el --- Minor mode to show README of current directory  -*- lexical-binding: t; -*-

;; Authors: Kisaragi Hiu <mail@kisaragi-hiu.com>
;; URL: https://kisaragi-hiu.com/projects/dired-show-readme
;; Version: 0.3.1
;; Package-Requires: ((emacs "25.1") (dash "2.10.0") (f "0.18.1"))
;; Keywords: dired files

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A minor mode to display the README of the current directory in
;; Dired, like many code foundries do.

;; Internally converts the README to HTML using pandoc, then renders
;; that with shr.

;;; Code:

(require 'shr)
(require 'dash)
(require 'f)

(defgroup dired-show-readme ()
  "Show README in Dired."
  :group 'dired
  :prefix "dired-show-readme-")

(defcustom dired-show-readme-pandoc-executable (executable-find "pandoc")
  "Path to the pandoc executable.

Set to nil to skip Pandoc altogether."
  :group 'dired-show-readme
  ;; Like Flycheck's flycheck-*-executable options
  :type `(choice
          (const :tag "Default executable" ,(executable-find "pandoc"))
          (string :tag "Name or path")
          (const :tag "Don't use Pandoc" nil)))

(defcustom dired-show-readme-position 'bottom
  ;; logic implemented in `dired-show-readme--make-overlay'.
  "Position of the README display.

Options:
`top': display before file listing (like Nextcloud)
`bottom': display after file listing (like Github)"
  :group 'dired-show-readme
  :type '(choice
          (const top
                 :tag "Display before file listing (like Nextcloud)")
          (const bottom
                 :tag "Display after file listing (like Github)")))

(defcustom dired-show-readme-regexp
  (rx (or (seq bol "README")
          (seq bol "readme")
          (seq bol "README.md")
          (seq bol "readme.md")
          (seq bol "README.org")
          (seq bol "readme.org")))
  "Regular expression used to match README files."
  :group 'dired-show-readme
  :type 'string)

(defvar-local dired-show-readme--overlay nil
  "Internal: overlay that shows the README in current buffer.")

(defun dired-show-readme--find-readme-file ()
  "Find a README file in the current directory, and return its name."
  (-some--> (directory-files "." nil dired-show-readme-regexp t)
    ;; explicitly prefer the shortest name (this function is not
    ;; expected to be run too often)
    (sort it #'string>)
    ;; except a plain "README" is only considered when there isn't
    ;; another README with an extension
    (if (string-match-p (rx ".") (car it))
        it
      (append (cdr it) (list (car it))))
    car))

(defun dired-show-readme--make-overlay (string place separator)
  "Make an overlay to show STRING at PLACE.

Put SEPARATOR between the buffer's content and STRING.

PLACE can be:
`top': Displayed before file listing
`bottom': Displayed after file listing

Returns the overlay created."
  (cond
   ((eq place 'top)
    (let ((ov (make-overlay (point-min) (point-min) nil t))
          (string (concat string separator)))
      (overlay-put ov 'before-string string)
      ov))
   ((eq place 'bottom)
    (let ((ov (make-overlay (point-max) (point-max)
                            ;; `point-max' is actually still 1 when
                            ;; this is run. Use t for REAR-ADVANCE to
                            ;; make sure end of the overlay stays at
                            ;; end of buffer.
                            nil t t))
          (string (concat separator string)))
      (overlay-put ov 'after-string string)
      ov))
   (t nil)))

(defun dired-show-readme--render-pandoc (file)
  "Render FILE with `pandoc' to a string."
  (cl-assert (executable-find dired-show-readme-pandoc-executable))
  ;; Creating a temporary file for `call-process'
  ;; Better option is to use `make-process' and adapt to async code,
  ;; but we can do that later.
  (let ((err-file (make-temp-file "dired-show-readme-pandoc-error")))
    (unwind-protect
        (cl-block nil
          (let* ((dom (with-temp-buffer
                        (let ((result (call-process
                                       dired-show-readme-pandoc-executable
                                       nil `(t ,err-file)
                                       nil
                                       ;; let pandoc figure out the format on
                                       ;; its own
                                       file "--to" "html")))
                          ;; return nil if return code != 0 or if
                          ;; something was sent to stderr
                          (when (or (/= result 0)
                                    (not (f-empty? err-file)))
                            (cl-return nil)))
                        (goto-char (point-max))
                        (libxml-parse-html-region (point-min) (point-max)))))
            (with-temp-buffer
              (shr-insert-document dom)
              (buffer-string))))
      (and (file-exists-p err-file)
           (delete-file err-file)))))

(defun dired-show-readme--render-file (file)
  "Render FILE to (propertized) string.

Selects a suitable backend to do the job, or if there's no
suitable backend, just return the contents of FILE."
  (cond ((and dired-show-readme-pandoc-executable
              (executable-find dired-show-readme-pandoc-executable)
              (fboundp 'libxml-parse-html-region))
         (or (dired-show-readme--render-pandoc file)
             (f-read-text file)))
        (t
         (f-read-text file))))

;;;###autoload
(define-minor-mode dired-show-readme-mode
  "Dired minor mode to preview README in current directory."
  :global nil :lighter " README"
  (when (derived-mode-p 'dired-mode)
    (if dired-show-readme-mode
        (-some--> (dired-show-readme--find-readme-file)
          (dired-show-readme--render-file it)
          (dired-show-readme--make-overlay it dired-show-readme-position (format "\n--- %s ---\n" (dired-show-readme--find-readme-file)))
          (setq dired-show-readme--overlay it))
      (-some--> dired-show-readme--overlay
        (delete-overlay it)))))

(provide 'dired-show-readme)

;;; dired-show-readme.el ends here
