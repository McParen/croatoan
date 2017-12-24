(in-package :de.anvi.croatoan)

(defun extract-string (window &key y x n)
  "Extract and return a string from window.

Any attributes are stripped from the characters before the string is
returned.

Start at the current cursor position and end at the right margin of
window. If n is given, read at most n chars. 

If the destination coordinates y and x are given, move the cursor to
the destination first."
  (when (and y x) (move window y x))
  (let ((len (if n n (distance-to-eol window))))
    (with-foreign-pointer (string len)
      ;; zero the allocated foreign string first.
      (setf (mem-ref string :char (1- len)) 0)
      ;; populate the foreign string with chars.
      ;; the c routines return ERR (-1) or the number of chars extracted.
      (let ((retval (%winnstr (.winptr window) string len)))
        (if (= retval -1)
            nil
            ;; convert the char pointer to a lisp string.
            (foreign-string-to-lisp string))))))
