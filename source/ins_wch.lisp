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
  (let ((winptr (.winptr window))
        (attr (if attributes (attrs2chtype attributes) 0))
        (color-pair-number (if color-pair (pair->number color-pair) 0))
        (count (if n n 1)))
    (typecase char
      ;; if we have a lisp char or an integer, use the attributes and colors passed as arguments.
      (integer      (funcall-with-cchar_t #'%wins-wch winptr             char attr color-pair-number count))
      (character    (funcall-with-cchar_t #'%wins-wch winptr (char-code char) attr color-pair-number count))
      ;; if we have a complex char, use its own attributes and colors.
      (complex-char (funcall-with-cchar_t #'%wins-wch
                                          winptr
                                          (char-code (.simple-char char))
                                          (if (.attributes char) (attrs2chtype (.attributes char)) 0)
                                          (if (.color-pair char) (pair->number (.color-pair char)) 0)
                                          count)))))
