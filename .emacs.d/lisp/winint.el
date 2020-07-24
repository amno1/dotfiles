 ;;(set gc-cons-threshold 100000000)
(prefer-coding-system 'utf-8)
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

(setenv "PATH" (concat  (getenv "PATH") ";C:\\msys64\\mingw64\\bin"))
(setenv "PATH" (concat  (getenv "PATH") ";C:\\msys64\\usr\\bin"))

(add-to-list 'exec-path "C:/msys64/mingw64/bin")
(add-to-list 'exec-path "C:/msys64/usr/bin")
(add-to-list 'exec-path "C:/Program Files/mpv")
(add-to-list 'exec-path "C:/Program Files (x86)/hunspell-1.3.2-3/bin")



(require 'speck)
(setq speck-hunspell-library-directory "C:/Program Files (x86)/hunspell-1.3.2-/share/hunspell/")
(setq speck-hunspell-default-dictionary-name "sv_SE")

(require 'auto-complete-clang)
(setq ac-auto-start nil)
(setq ac-quick-help-delay 0.5)
(define-key ac-mode-map  [(control tab)] 'auto-complete)
(defun my-ac-config ()
  (setq-default ac-sources '(ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers))
  (add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
  ;; (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
  (add-hook 'ruby-mode-hook 'ac-ruby-mode-setup)
  (add-hook 'css-mode-hook 'ac-css-mode-setup)
  (add-hook 'auto-complete-mode-hook 'ac-common-setup)
  (global-auto-complete-mode t))
(defun my-ac-cc-mode-setup ()
  (setq ac-sources (append '(ac-source-clang ac-source-yasnippet) ac-sources)))
(add-hook 'c-mode-common-hook 'my-ac-cc-mode-setup)
;; ac-source-gtags
(setq ac-clang-flags
      (mapcar (lambda (item)(concat "-I" item))
              (split-string
               " 
 /c/sdk/boost_1_63_0_mw/boost_1_63_0
 /c/sdk/src
 /usr/local/include
 /usr/lib/gcc/x86_64-w64-mingw32/include
 /usr/mingw64/include
 /usr/include
"
)))
(my-ac-config)
(global-set-key (kbd "C-'") 'ac-complete-clang)

(defvar decor 0)
(defun toggle-frame-decor ()
  (interactive)
  (progn
   (modify-frame-parameters (selected-frame) `((undecorated . ,'decor)))
   (if (= decor 0)
       (setq decor 1)
     (setq decor 0))))
(global-set-key [f9] 'toggle-frame-decor)


(global-set-key (kbd "H-w") 'windmove-up)
(global-set-key (kbd "H-a") 'windmove-left)
(global-set-key (kbd "H-s") 'windmove-down)
(global-set-key (kbd "H-d") 'windmove-right)
