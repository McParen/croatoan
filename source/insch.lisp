(in-package :de.anvi.croatoan)

(defun insert-char (window char &key attributes color-pair y x position n)
  "Insert char into window before the character currently under the cursor.

Chars right of the cursor are moved one position to the right.
The rightmost character on the line may be lost. The position of the
cursor is not changed.

If the position coordinates y (row) and x (column) are given, move the
cursor to the position first and then add the object.

The position can also be passed in form of a two-element list.

If n is given, insert n chars."
  (when (and y x) (move window y x))
  (when position (apply #'move window position))
  (let ((winptr (winptr window))
        (chtype (make-chtype char attributes color-pair))
        (count (if n n 1)))
    (loop repeat count do (%winsch winptr chtype))))
