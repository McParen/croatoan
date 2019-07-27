(in-package :de.anvi.croatoan)

(defun insert-wide-char (window char &key attributes color-pair style y x position n)
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
        (color-pair (if style
                        (list (getf style :foreground) (getf style :background))
                        color-pair)))
    (funcall-make-cchar_t #'%wins-wch window char attributes color-pair n)))

;; TODO: (defmethod insert (obj character))
;;       (defmethod insert (obj string)) etc.
;; the same for echo (only chars) and add.

;; :x t :y t => keep the current row or column

(defun insert (window object &rest keys &key &allow-other-keys)
  "Insert char or string into window before the char currently under the cursor.

Currently supported text objects are characters (simple and complex),
characters given by integer codes or keywords, and strings 
(simple and complex).

If the position coordinates y (row) and x (column) are given, move the
cursor to the position first and then insert the object.

The position can also be passed in form of a two-element list.

If n is given for a char, insert n chars.

If n is given for a string, add at most n chars from the string."
  (let ((fn (typecase object
              ((or string complex-string)
               #'insert-string)
              ((or integer keyword character complex-char)
               #'insert-wide-char))))   
     (apply fn window object keys)))
