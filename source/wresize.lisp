(in-package :de.anvi.croatoan)

(defun resize (window height width)
  (let ((winptr (.winptr window)))
    (%wresize winptr height width)))
