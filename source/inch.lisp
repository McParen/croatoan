(in-package :de.anvi.croatoan)

(defun extract-char (window &key y x position)
  "Extract and return the single-byte complex char from the window.

If the position coordinates y (row) and x (column) are given, move the
cursor to the position first and then add the character.

The position can also be passed in form of a two-element list."
  (when (and y x) (move window y x))
  (when position (apply #'move window position))
  (let* ((winptr (.winptr window))
         (chtype (%winch winptr)))
    (chtype2xchar chtype)))
