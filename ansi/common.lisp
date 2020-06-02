(in-package :de.anvi.ansi-escape)

;;; Common functions based on the core ANSI control functions.

(defun home ()
  "Move the cursor to the home position, the top left corner."
  (cursor-position))

(defun clear ()
  "Erase the whole screen, then move the cursor to the home position."
  (erase)
  (home))
