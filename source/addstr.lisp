(in-package :de.anvi.croatoan)

;; first attempt to create add-string with attributes and colors
;; without adding a complex-string type first.
(defun add-string (window string &key attributes color-pair y x n)
  "Add the unrendered string to the window.

If n is given, write at most n chars from the string. If n is -1, as
many chars will be added that will fit on the line.

If the coordinates y and x are given, move to the destination first
and then add the string."
  (when (and y x) (move window y x))
  (typecase string
    (string
     (progn
       (let ((winptr (.winptr window))
             (pos1 (.cursor-position window)))
         (if n
             (%waddnstr winptr string n)
             (%waddstr winptr string))
         (when (or attributes color-pair)
           (let ((pos2 (.cursor-position window))
                 (count (if n n (length string))))
             (move window (car pos1) (cadr pos1))
             (change-attributes window count attributes color-pair)
             (move window (car pos2) (cadr pos2)))))))
    (complex-string
     (if n
         (loop
            repeat (if (= n -1)
                       (- (.width window) (cadr (.cursor-position window)))
                       n)
            for ch across (.complex-char-array string)
            do (add-char window ch))
         (loop
            for ch across (.complex-char-array string)
            do (add-char window ch))))))


;;; TODOs

;; A common function to display both strings and chars.
;;(defun display (window object &key y x n)
;;  (if (typep object 'chtype)

;; rewrite all the addstr and print functions in a way that they use format internally and
;; can accept chars like ~% instead of C's \n.
;;(add-string scr (format nil " upper left corner~%~%~%"))
