(in-package :de.anvi.croatoan)

(defun delete-line (window &key (n 1))
  "Delete n lines starting with the one under the cursor.

The remaining lines are moved up. The bottom n lines are cleared.

The current cursor position does not change."
  (ncurses:winsdelln (winptr window) (- n)))

(defun insert-line (window &key (n 1))
  "Insert n lines above the current line. 

The current line and the lines below are moved down. The n bottom
lines are lost. 

The current cursor position does not change."
  (ncurses:winsdelln (winptr window) n))
