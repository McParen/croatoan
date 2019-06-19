(in-package :de.anvi.croatoan)

(defun extract-string (window &key y x position n)
  "Extract and return a string from window.

Any attributes are stripped from the characters before the string is returned.

Start at the current cursor position and end at the right margin of window. 

If the position coordinates y (row) and x (column) are given, move the
cursor to the position first and then add the character.

The position can also be passed in form of a two-element list.

If n is given, read at most n chars."
  (when (and y x) (move window y x))
  (when position (apply #'move window position))
  (let ((len (if n n (distance-to-eol window))))
    (with-foreign-pointer (string len)
      ;; zero the allocated foreign string first.
      (setf (mem-ref string :char (1- len)) 0)
      ;; populate the foreign string with chars.
      ;; the c routines return ERR (-1) or the number of chars extracted.
      (let ((retval (%winnstr (winptr window) string len)))
        (if (= retval -1)
            nil
            ;; convert the char pointer to a lisp string.
            (foreign-string-to-lisp string))))))
