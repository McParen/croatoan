(in-package :de.anvi.croatoan)

(defun add-string (window string &key y x n)
  "Add the unrendered string to the window.

If n is given, write at most n chars from the string. If n is -1, as
many chars will be added that will fit on the line.

If the coordinates y and x are given, move to the destination first
and then add the string."
  (let ((winptr (.winptr window)))
    (cond ((and y x n)
           (%mvwaddnstr winptr y x string n))
          ((and y x)
           (%mvwaddstr winptr y x string))
          (n
           (%waddnstr winptr string n))
          (t
           (%waddstr winptr string)))))

;;; TODOs

;; A common function to display both strings and chars.
;;(defun display (window object &key y x n)
;;  (if (typep object 'chtype)

;; rewrite all the addstr and print functions in a way that they use format internally and
;; can accept chars like ~% instead of C's \n.
;;(add-string scr (format nil " upper left corner~%~%~%"))
