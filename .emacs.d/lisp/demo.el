(require 'elgantt)
(require 'elgantt-interaction)
(setq elgantt-agenda-files
      (concat default-directory "test.org"))


(defun elgantt-demo1()
  (interactive)
  (elgantt--reset-org-ql-cache)
  (setq elgantt-user-set-color-priority-counter 0)
  (elgantt-create-display-rule draw-scheduled-to-deadline
    :parser ((elgantt-color . ((when-let ((colors (org-entry-get (point) "ELGANTT-COLOR")))
                                 (s-split " " colors)))))
    :args (elgantt-scheduled elgantt-color elgantt-org-id)
    :body ((when elgantt-scheduled
             (let ((point1 (point))
                   (point2 (save-excursion
                             (elgantt--goto-date elgantt-scheduled)
                             (point)))
                   (color1 (or (car elgantt-color)
                               "black"))
                   (color2 (or (cadr elgantt-color)
                               "red")))
               (when (/= point1 point2)
                 (elgantt--draw-gradient 
                  color1
                  color2
                  (if (< point1 point2) point1 point2) ;; Since cells are not necessarily linked in 
                  (if (< point1 point2) point2 point1) ;; chronological order, make sure they are sorted
                  nil
                  `(priority ,(setq elgantt-user-set-color-priority-counter
                                    (1- elgantt-user-set-color-priority-counter))
                             ;; Decrease the priority so that earlier entries take
                             ;; precedence over later ones (note: it doesn’t matter if the number is negative)
                             :elgantt-user-overlay ,elgantt-org-id)))))))
  (elgantt-open))

(defun elgantt-demo2 ()
  (interactive)
  (elgantt--reset-org-ql-cache)
  (elgantt--selection-rule
   :name colorize
   :selection-number 2
   :selection-messages ((1 . "Select first cell")
                        (2 . "Select second cell"))
   :execution-functions ((2 . ((elgantt-with-point-at-orig-entry nil
                                   (org-id-get-create))))
                         (1 . ((elgantt-with-point-at-orig-entry nil
                                   (org-set-property "ELGANTT-LINKED-TO" return-val)
                                 (org-set-property "ELGANTT-COLOR" (concat (s-trim (read-color "Select start color:"))
                                                                           " "
                                                                           (s-trim (read-color "Select end color:")))))))))

  ;; You’ll also need to use this to colorize 
  (setq elgantt-user-set-color-priority-counter 0) ;; There must be a counter to ensure that overlapping overlays are handled properly
  (elgantt-create-display-rule user-set-color
    :parser ((elgantt-color . ((when-let ((colors (org-entry-get (point) "ELGANTT-COLOR")))
                                 (s-split " " colors))))
             (elgantt-linked-to . ((org-entry-get (point) "ELGANTT-LINKED-TO"))))
    :args (elgantt-org-id)
    :body ((when elgantt-linked-to
             (save-excursion
               (when-let ((point1 (point))
                          (point2 (let (date) 
                                    ;; Cells can be linked even if they are not 
                                    ;; in the same header in the calendar. Therefore, 
                                    ;; we have to get the date of the linked cell, and then
                                    ;; move to that date in the current header
                                    (save-excursion (elgantt--goto-id elgantt-linked-to)
                                                    (setq date (elgantt-get-date-at-point)))
                                    (elgantt--goto-date date)
                                    (point)))
                          (color1 (car elgantt-color))
                          (color2 (cadr elgantt-color)))
                 (when (/= point1 point2)
                   (elgantt--draw-gradient 
                    color1
                    color2
                    (if (< point1 point2) point1 point2) ;; Since cells are not necessarily linked in 
                    (if (< point1 point2) point2 point1) ;; chronological order, make sure they are sorted
                    nil
                    `(priority ,(setq elgantt-user-set-color-priority-counter
                                      (1- elgantt-user-set-color-priority-counter))
                               ;; Decrease the priority so that earlier entries take
                               ;; precedence over later ones
                               :elgantt-user-overlay ,elgantt-org-id))))))))
  (elgantt-open))
