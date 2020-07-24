(provide 'emacs-orgmode-config)
(require 'ox)

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

(setq org-log-done t)
(setq org-fast-tag-selection-single-key t)
(setq org-use-fast-todo-selection t)
(setq org-startup-truncated nil)

(setq org-directory (expand-file-name "~/org"))
(setq org-default-notes-file (concat org-directory "/mygtd.org"))
(setq org-agenda-files '("~/org" "~/www/org" "~/www/_org"))

(setq org-todo-keywords
      '(
        (sequence "IDEA(i)" "TODO(t)" "STARTED(s)" "NEXT(n)" "WAITING(w)" "|" "DONE(d)")
        (sequence "|" "CANCELED(c)" "DELEGATED(l)" "SOMEDAY(f)")
        ))

(setq org-todo-keyword-faces
      '(("IDEA" . (:foreground "GoldenRod" :weight bold))
        ("NEXT" . (:foreground "IndianRed1" :weight bold))
        ("STARTED" . (:foreground "OrangeRed" :weight bold))
        ("WAITING" . (:foreground "coral" :weight bold))
        ("CANCELED" . (:foreground "LimeGreen" :weight bold))
        ("DELEGATED" . (:foreground "LimeGreen" :weight bold))
        ("SOMEDAY" . (:foreground "LimeGreen" :weight bold))
        ))

(setq org-tag-persistent-alist
      '((:startgroup . nil)
        ("HOME" . ?h)
        ("RESEARCH" . ?r)
        ("TEACHING" . ?t)
        (:endgroup . nil)
        (:startgroup . nil)
        ("OS" . ?o)
        ("DEV" . ?d)
        ("WWW" . ?w)
        (:endgroup . nil)
        (:startgroup . nil)
        ("EASY" . ?e)
        ("MEDIUM" . ?m)
        ("HARD" . ?a)
        (:endgroup . nil)
        ("URGENT" . ?u)
        ("KEY" . ?k)
        ("BONUS" . ?b)
        ("noexport" . ?x)
        )
      )

(setq org-tag-faces
      '(
        ("HOME" . (:foreground "GoldenRod" :weight bold))
        ("RESEARCH" . (:foreground "GoldenRod" :weight bold))
        ("TEACHING" . (:foreground "GoldenRod" :weight bold))
        ("OS" . (:foreground "IndianRed1" :weight bold))
        ("DEV" . (:foreground "IndianRed1" :weight bold))
        ("WWW" . (:foreground "IndianRed1" :weight bold))
        ("URGENT" . (:foreground "Red" :weight bold))
        ("KEY" . (:foreground "Red" :weight bold))
        ("EASY" . (:foreground "OrangeRed" :weight bold))
        ("MEDIUM" . (:foreground "OrangeRed" :weight bold))
        ("HARD" . (:foreground "OrangeRed" :weight bold))
        ("BONUS" . (:foreground "GoldenRod" :weight bold))
        ("noexport" . (:foreground "LimeGreen" :weight bold))
        )
      )

(setq org-enable-priority-commands nil)

(setq org-list-demote-modify-bullet (quote (("+" . "-")
                                            ("*" . "-")
                                            ("1." . "-")
                                            ("1)" . "a)"))))


(setq org-agenda-ndays 7)
(setq org-agenda-show-all-dates t)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-start-on-weekday nil)
(setq org-deadline-warning-days 14)
(setq org-agenda-custom-commands
      '(("g" . "GTD contexts")
        ("gh" "Home" tags-todo "HOME")
        ("gu" "Urgent" tags-todo "URGENT")
        ("G" "GTD Block Agenda"
         ((todo "STARTED")
          (tags-todo "URGENT")
          (todo "NEXT"))
         ((org-agenda-prefix-format "[ ] %T: ")
          (org-agenda-with-colors nil)
          (org-agenda-compact-blocks t)
          (org-agenda-remove-tags t)
          (ps-number-of-columns 2)
          (ps-landscape-mode t))
         ;;nil                      ;; i.e., no local settings
         ("~/next-actions.txt"))
        ))
(setq org-reverse-note-order t)

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/org/mygtd.org" "Tasks")
         "* TODO %?\nAdded: %U\n" :prepend t :kill-buffer t)
        ("w" "Web" entry (file+headline "~/www/org/index.org" "Tasks")
         "* TODO %?\nAdded: %U\n" :prepend t :kill-buffer t)
        ("r" "Prog. R" entry (file+headline "~/www/org/teaching/introR.org" "Tasks")
         "* TODO %?\nAdded: %U\n" :prepend t :kill-buffer t)
        ("i" "Idea" entry (file+headline "~/org/mygtd.org" "Someday/Maybe")
         "* IDEA %?\nAdded: %U\n" :prepend t :kill-buffer t)
        ("h" "Home" entry (file+headline "~/org/mygtd.org" "Home")
         "* TODO %?\nAdded: %U\n" :prepend t :kill-buffer t)
        )
      )

(require 'ox-latex)

(add-to-list 'org-latex-packages-alist '(\"\" \"listings\"))
(add-to-list 'org-latex-packages-alist '(\"\" \"color\"))

(setq org-format-latex-options (quote (:foreground default :background default :scale 2.0 :html-foreground "Black" :html-background "Transparent" :html-scale 2.0 :matchers ("begin" "$1" "$" "$$" "\\(" "\\["))))

(setq org-use-speed-commands t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (emacs-lisp . t)
   (org . t)
   (sh . t)
   (C . t)
   (python . t)
   (gnuplot . t)
   (octave . t)
   (R . t)
   (dot . t)
   (awk . t)
   ))

(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)

(defun org-babel-tangle-block()
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively 'org-babel-tangle)
    ))

(add-to-list 'ispell-skip-region-alist '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:"))
(add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_SRC" . "#\\+END_SRC"))
(add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_EXAMPLE" .
                                         "#\\+END_EXAMPLE"))

(setq org-ascii-links-to-notes nil)

(setq org-ascii-headline-spacing (quote (1 . 1)))

(setq org-export-with-smart-quotes t)

(require 'ox-publish)
(setq org-html-coding-system 'utf-8-unix)
(
 setq org-publish-project-alist
 '(
   ("html-static"
    :base-directory "~/www/static_html/"
    :base-extension "html\\|xml\\|css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|zip\\|gz\\|csv\\|m\\|R"
    :include (".htaccess")
    :publishing-directory "~/public_html/"
    :recursive t
    :publishing-function org-publish-attachment
    )

   ("org-notes"
    :base-directory "~/www/org/"
    :base-extension "org"
    :publishing-directory "~/public_html/org/"
    :recursive t
    :exclude ".*-reveal\.org"        ; exclude org-reveal slides
    :publishing-function org-html-publish-to-html
    :headline-levels 2               ; Just the default for this project.
    :auto-sitemap t                  ; Generate sitemap.org automagically...
    :sitemap-filename "sitemap.org"  ; ... call it sitemap.org (it's the default)...
    :sitemap-title "Sitemap"         ; ... with title 'Sitemap'.
    :with-creator nil    ; Disable the inclusion of "Created by Org" in the postamble.
    :with-email nil      ; Disable the inclusion of "(your email)" in the postamble.
    :with-author nil       ; Enable the inclusion of "Author: Your Name" in the postamble.
    :auto-preamble t;         ; Enable auto preamble
    :auto-postamble t         ; Enable auto postamble
    :table-of-contents t        ; Set this to "t" if you want a table of contents, set to "nil" disables TOC.
    :toc-levels 2               ; Just the default for this project.
    :section-numbers nil        ; Set this to "t" if you want headings to have numbers.
    :html-head-include-default-style nil ;Disable the default css style
    :html-head-include-scripts nil ;Disable the default javascript snippet
    :html-head "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n<link rel=\"stylesheet\" type=\"text/css\" href=\"http://www.i3s.unice.fr/~malapert/css/worg.min.css\"/>\n<script type=\"text/javascript\" src=\"http://www.i3s.unice.fr/~malapert/js/ga.min.js\"></script>" ;Enable custom css style and other tags
    :html-link-home "index.html"    ; Just the default for this project.
    :html-link-up "../index.html"    ; Just the default for this project.
    )

   ("org-static"
    :base-directory "~/www/org/"
    :base-extension "html\\|xml\\|css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|zip\\|gz\\|csv\\|m\\|R"
    :publishing-directory "~/public_html/org/"
    :recursive t
    :publishing-function org-publish-attachment
    :exclude "Rplots.pdf"
    )

   ("org"
    :components ("org-notes" "org-static" "html-static")
    )

   ("_org-notes"
    :base-directory "~/www/_org/"
    :base-extension "org"
    :publishing-directory "~/private_html/"
    :recursive t
    :publishing-function org-html-publish-to-html
    :headline-levels 2               ; Just the default for this project.
    :auto-preamble t
    :auto-sitemap nil                  ; Do NOT Generate sitemap.org automagically...
    :with-creator nil    ; Disable the inclusion of "Created by Org" in the postamble.
    :with-email nil      ; Disable the inclusion of "(your email)" in the postamble.
    :with-author nil       ; Enable the inclusion of "Author: Your Name" in the postamble.
    :auto-preamble t;         ; Enable auto preamble
    :auto-postamble t         ; Enable auto postamble
    :table-of-contents t        ; Set this to "t" if you want a table of contents, set to "nil" disables TOC.
    :toc-levels 2               ; Just the default for this project.
    :section-numbers nil        ; Set this to "t" if you want headings to have numbers.
    :html-head-include-default-style nil ;Disable the default css style
    :html-head-include-scripts nil ;Disable the default javascript snippet
    :html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"http://www.i3s.unice.fr/~malapert/css/worg.min.css\"/>" ;Enable custom css style
    )

   ("_org-static"
    :base-directory "~/www/_org/"
    :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|zip\\|gz"
    :publishing-directory "~/private_html/"
    :recursive t
    :publishing-function org-publish-attachment
    :exclude "Rplots.pdf"
    )

   ("_org"
    :components ("_org-notes" "_org-static")
    )
   )
 )

(setq org-html-head-include-default-style nil)
(setq org-html-head-include-scripts nil)

(setq org-html-validation-link nil)

(setf org-html-mathjax-options
      '((path "https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML")
        (scale "100")
        (align "center")
        (indent "2em")
        (mathml nil))
      )
(setf org-html-mathjax-template
      "<script type=\"text/javascript\" src=\"%PATH\"></script>")

(setq org-html-table-default-attributes
      '(:border "0" :cellspacing "0" :cellpadding "6" :rules "none" :frame
                "none"))

(require 'ox-reveal)
