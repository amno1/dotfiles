;; Make startup faster by reducing the frequency of garbage
;; collection.  The default is 800 kilobytes.  Measured in bytes.
(setq old-gc-cons-trsh gc-cons-threshold)
(setq gc-cons-threshold (* 50 1000 1000))

(let ((file-name-handler-alist nil))
  ;; Use a hook so the message doesn't get clobbered by other messages.
  ;; (add-hook 'emacs-startup-hook
  ;;           (lambda ()
  ;;             (message "Emacs ready in %s with %d garbage collections."
  ;;                      (format "%.2f seconds"
  ;;                              (float-time
  ;;                               (time-subtract after-init-time before-init-time)))
  ;;                      gcs-done)))

  (require 'package)

  (setq load-prefer-newer t
        package-enable-at-startup nil)

  (setq  package-archives '(("melpa" . "https://melpa.org/packages/")
                            ;;			  ("melpa-stable" . "https://stable.melpa.org/packages/")
			    ("elpa" . "http://elpa.gnu.org/packages/")
			    ("marmalade"   . "http://marmalade-repo.org/packages/")
                            ;;			  ("gnu"         . "http://elpa.gnu.org/packages/")
			    ("org" . "http://orgmode.org/elpa/")))

  (let ((default-directory  "~/.emacs.d/lisp/"))
    (normal-top-level-add-to-load-path '("."))
    (normal-top-level-add-subdirs-to-load-path))

  ;; (package-initialize)
  ;; (package-refresh-contents)

  (setq package-quickstart t)

  (unless (package-installed-p 'use-package)
    (package-install 'use-package))

  ;;(org-babel-load-file "~/.emacs.d/lisp/init.org")
  (load-file "~/.emacs.d/lisp/init.el"))

;; restore gc
(setq gc-cons-threshold old-gc-cons-trsh)


