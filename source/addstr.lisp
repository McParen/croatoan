(in-package :de.anvi.croatoan)

(defun add-string (window string &key attributes color-pair y x position n)
  "Add the unrendered string to the window.

If n is given, write at most n chars from the string. If n is -1, as
many chars will be added that will fit on the line.

If the position coordinates y (row) and x (column) are given, move the
cursor to the position first and then add the object.

The position can also be passed in form of a two-element list.
"
  (when (and y x) (move window y x))
  (when position (apply #'move window position))
  (let ((count (if n
                   (if (= n -1)
                       (- (width window) (cadr (cursor-position window)))
                       n)
                   ;; we cant use length to determine the length of a complex string
                   ;; because it is not a sequence.
                   (typecase string
                     (string (length string))
                     (complex-string (length (complex-char-array string)))))))
    (typecase string
      (string
       (if (or attributes color-pair)
           ;; lisp string combined with attributes and colors
           (loop
              repeat count
              for ch across string
              do (add-wide-char window ch :attributes attributes :color-pair color-pair))
           ;; simple lisp string, no attributes or colors
           (if n
               (%waddnstr (winptr window) string n)
               (%waddstr  (winptr window) string))))
      (complex-string
       (loop
          repeat count
          for ch across (complex-char-array string)
          do (add-wide-char window ch))))))
