(in-package :de.anvi.croatoan)

(defun insert-string (window string &key y x n)
  "Insert string at the current position in window.

Chars right of the cursor are moved to the right. The rightmost chars
on the line may be lost. The cursor position is not changed.

If n is given, insert n chars. If (<= n 0) insert the whole string.

If the destination coordinates y and x are given, move the cursor
there first."
  (let ((winptr (.winptr window)))
    (cond ((and y x n)
           (%mvwinsnstr winptr y x string n))
          ((and y x)
           (%mvwinsstr winptr y x string))
          (n
           (%winsnstr winptr string n))
          (t
           (%winsstr winptr string)))))
