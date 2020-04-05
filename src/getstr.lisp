(in-package :de.anvi.croatoan)

(defun get-string (window n &key y x)
  "Read a string from the keyboard and return it.

Reading is performed until a newline or carriage return is received.
The terminating character is not included in the returned string.

If n is given, read at most n chars, to prevent a possible input
buffer overflow.

If the destination coordinates y and x are given, move the cursor
there first."
  (let ((winptr (winptr window)))
    (cffi:with-foreign-pointer-as-string (string n)
      (cond ((and y x)
             (ncurses:mvwgetnstr winptr y x string n))
            (t
             (ncurses:wgetnstr winptr string n))))))
