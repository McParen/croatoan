(in-package :de.anvi.croatoan)

(defun insert-string (window string &key attributes fgcolor bgcolor color-pair style y x position n)
  "Insert string before the current position in window.

Chars right of the cursor are moved to the right. The rightmost chars
on the line may be lost. The cursor position is not changed.

If n is given, insert at most n chars from the string.

If the position coordinates y (row) and x (column) are given, move the
cursor to the position first and then add the object.

The position can also be passed in form of a two-element list."
  (when (and y x) (move window y x))
  (when position (apply #'move window position))
  (let ((count (if n
                   n
                   ;; we cant use length to determine the length of a complex string
                   ;; because it is not a sequence.
                   (typecase string
                     (string (length string))
                     (complex-string (length (complex-char-array string)))))))
    (typecase string
      (string
       ;;(if (or attributes fgcolor bgcolor color-pair style)
           ;; lisp string combined with attributes and colors
           (loop
              repeat count
              for ch across (reverse string)
              do (insert-wide-char window ch :attributes attributes :fgcolor fgcolor :bgcolor bgcolor
                                   :color-pair color-pair :style style)) )
           ;; simple lisp string, no attributes or colors
           ;; TODO 190826 we dont want to use this because we want to force color-set and bkgd to use separate colors
           ;;(if n
           ;;    (%winsnstr (winptr window) string n)
           ;;    (%winsstr  (winptr window) string))))
      (complex-string
       (loop
          repeat count
          for ch across (reverse (complex-char-array string))
          do (insert-wide-char window ch))))))
