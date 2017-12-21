(in-package :de.anvi.croatoan)

(defun insert-wide-char (window char &key attributes color-pair y x n)
  "Insert char into window before the character currently under the cursor.

Chars right of the cursor are moved one position to the right.
The rightmost character on the line may be lost. The position of the
cursor is not changed.

If the destination coordinates y and x are given, move the cursor
there first.

If n is given, insert n chars."
  (when (and y x) (move window y x))
  (let ((count (if n n 1)))
    (funcall-make-cchar_t #'%wins-wch window char attributes color-pair count)))
