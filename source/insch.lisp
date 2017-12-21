(in-package :de.anvi.croatoan)

(defun insert-char (window char &key attributes color-pair y x)
  "Insert char into window before the character currently under the cursor.

Chars right of the cursor are moved one position to the right.
The rightmost character on the line may be lost. The position of the
cursor is not changed.

If the destination coordinates y and x are given, move the cursor
there first."
  (when (and y x) (move window y x))
  (let ((winptr (.winptr window))
        (chtype (make-chtype char attributes color-pair)))
    (%winsch winptr chtype)))
