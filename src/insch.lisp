(in-package :de.anvi.croatoan)

(defun insert-char (window char &key attributes fgcolor bgcolor color-pair style y x position n)
  "Insert char into window before the character currently under the cursor.

Chars right of the cursor are moved one position to the right.
The rightmost character on the line may be lost. The position of the
cursor is not changed.

char can be a simple character or a complex-char with attributes and colors.

If the position coordinates y (row) and x (column) are given, move the
cursor to the position first and then add the object.

The position can also be passed in form of a two-element list.

If n is given, insert n chars."
  (when (and y x) (move window y x))
  (when position (apply #'move window position))
  (let ((attributes (if style
                        (getf style :attributes)
                        attributes))
        (color-pair (cond (style
                           (list (getf style :fgcolor) (getf style :bgcolor)))
                          ((or fgcolor bgcolor)
                           (list fgcolor bgcolor))
                          (t color-pair))))
    (funcall-make-chtype #'%winsch window char attributes color-pair n)))
