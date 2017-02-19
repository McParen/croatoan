(in-package :de.anvi.croatoan)

(defun add-wide-char (window char &key attributes y x)
  "Add the wide (multi-byte) char to the window, then advance the cursor.

If the destination coordinates y and x are given, move the cursor to the
destination first and then add the character.

If n is given, write n chars. If n is -1, as many chars will be added
as will fit on the line."
  (when (and y x) (move window y x))
  (let ((winptr (.winptr window))
        (code (char-code char))
        ;; I absolutely can not get the color argument to work with wadd-wch.
        ;; but attributes work.
        (attr (if attributes (attrs2chtype attributes) 0)))
    (with-foreign-object (ptr '(:struct cchar_t))
      (setf ptr (convert-to-foreign (list 'cchar-attr attr 'cchar-chars code) ; Lisp plist to C struct.
                                    '(:struct cchar)))
      (%wadd-wch winptr ptr))))

;; TODO: color-pair, n, see add-char
;;       proper type translation cchar_t->xchar
