(in-package :de.anvi.ansi-escape)

;;; Common functions based on the core ANSI control functions.

(defun home ()
  (cursor-position))

(defun clear ()
  (erase)
  (home))
