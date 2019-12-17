(in-package :de.anvi.croatoan)

;; 0 invisible, 1 visible, 2 very visible.
(defun set-cursor-visibility (status)
  (case status
    ((nil :invisible)    (%curs-set 0))
    ((t :visible)      (%curs-set 1))
    (:very-visible (%curs-set 2))
    (otherwise (error "Valid status arguments: nil, t, :very-visible"))))
