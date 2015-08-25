(in-package :de.anvi.croatoan)

(defun extract-string (window &key y x n)
  "Extract and return a string from window.

Any attributes are stripped from the characters before the string is
returned.

Start at the current cursor position and end at the right margin of
window. If n is given, read at most n chars. 

If the destination coordinates y and x are given, move the cursor to
the destination first."
  (with-foreign-pointer (string 200 len)
    ;; zero the result string.
    (setf (mem-ref string :char (1- len)) 0)
    ;; populate the foreign string with chars.
    ;; the c routines return ERR (-1) or the number of chars extracted.
    (let ((retval (cond ((and y x n)
                         (%mvwinnstr (.winptr window) y x string n))
                        ((and y x)
                         (%mvwinstr (.winptr window) y x string))
                        (n
                         (%winnstr (.winptr window) string n))
                        (t
                         (%winstr (.winptr window) string)))))
      (if (= retval -1)
          nil
          ;; convert the char pointer to a lisp string.
          (foreign-string-to-lisp string)))))

;;; NOTES

;;; TODOs

;; [ ] Reimplement completely in Lisp, using extract-char.
