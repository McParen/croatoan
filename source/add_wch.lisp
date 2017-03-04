(in-package :de.anvi.croatoan)

(defun add-wide-char (window char &key attributes color-pair y x n)
  "Add the wide (multi-byte) char to the window, then advance the cursor.

If the destination coordinates y and x are given, move the cursor to the
destination first and then add the character.

If n is given, write n chars. If n is -1, as many chars will be added
as will fit on the line."
  (when (and y x) (move window y x))
  (let ((count  (if n
                   (if (= n -1)
                       (- (.width window) (cadr (.cursor-position window)))
                       n)
                   1))
        (code-point (typecase char
                      (integer char)
                      (character (char-code char)))))
    (typecase char
      (complex-char
       ;; if we have a complex char, use its attributes and colors.
       (loop repeat count do
            (mapc #'(lambda (ch) (add-char window ch :attributes (.attributes char) :color-pair (.color-pair char)))
                  (unicode-to-utf-8 (char-code (.simple-char char))))))
      ;; if we have a lisp char or an integer, use the attributes and colors passed as arguments.
      (t
       (loop repeat count do
            (mapc #'(lambda (ch) (add-char window ch :attributes attributes :color-pair color-pair))
                  (unicode-to-utf-8 code-point)))))))
