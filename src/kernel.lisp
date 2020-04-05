(in-package :de.anvi.croatoan)

;; 0 invisible, 1 visible, 2 very visible.
(defun set-cursor-visibility (status)
  (case status
    ((nil :invisible) (ncurses:curs-set 0))
    ((t :visible)     (ncurses:curs-set 1))
    (:very-visible    (ncurses:curs-set 2))
    (otherwise (error "Valid status arguments: nil, t, :very-visible"))))
