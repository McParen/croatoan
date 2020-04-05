(in-package :de.anvi.croatoan)

(defun delete-char (window &key y x)
  "Delete the character under the cursor. 

All characters to the right of the cursor on the same line are moved
to the left one position and the last character on the line is filled
with a blank. The cursor position does not change after moving
to (y,x), if specified."
  (let ((winptr (winptr window)))
    (cond ((and y x)
           (ncurses:mvwdelch winptr y x))
          (t
           (ncurses:wdelch winptr)))))
