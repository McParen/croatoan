(in-package :de.anvi.croatoan)

(defun resize (window height width)
  (let ((winptr (winptr window)))
    (ncurses:wresize winptr height width)))
