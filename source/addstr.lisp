(in-package :de.anvi.croatoan)

(defun add-string (window string &key attributes color-pair y x n)
  "Add the unrendered string to the window.

If n is given, write at most n chars from the string. If n is -1, as
many chars will be added that will fit on the line.

If the coordinates y and x are given, move to the destination first
and then add the string."
  (when (and y x) (move window y x))
  (let ((count (if n
                   (if (= n -1)
                       (- (.width window) (cadr (.cursor-position window)))
                       n)
                   (length string))))
    (typecase string
      (string
       (if (or attributes color-pair)
           ;; lisp string combined with attributes and colors
           (loop
              repeat count
              for ch across string
              do (add-char window ch :attributes attributes :color-pair color-pair))
           ;; simple lisp string, no attributes or colors
           (if n
               (%waddnstr (.winptr window) string n)
               (%waddstr  (.winptr window) string))))
      (complex-string
       (loop
          repeat count
          for ch across (.complex-char-array string)
          do (add-char window ch))))))
