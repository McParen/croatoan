(in-package :de.anvi.croatoan)

(defun insert-char (window char &key attributes color-pair y x)
  "Insert char into window before the character currently under the cursor.

Chars right of the cursor are moved one position to the right.
The rightmost character on the line may be lost. The position of the
cursor is not changed.

If the destination coordinates y and x are given, move the cursor
there first."
  (let ((winptr (.winptr window))
        (chtype (logior (typecase char
                          ;; if the char is already an integer chtype
                          (integer char)
                          ;; alternative chars are given as keywords
                          (keyword (acs char))
                          ;; if it is a lisp char, convert it to an integer first
                          (character (char-code char))
                          (t 0))
                        ;; convert the pair to an integer, then bit shift it by 8
                        (if color-pair (ash (pair->number color-pair) 8) 0)
                        ;; the attribute bitmasks already are bitshifted to the
                        ;; correct position in the chtype
                        (if attributes (attrs2chtype attributes) 0))))
    (cond ((and y x)
           (%mvwinsch winptr y x chtype))
          (t
           (%winsch winptr chtype)))))
