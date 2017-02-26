(in-package :de.anvi.croatoan)

(defun add-wide-char (window char &key attributes color-pair y x)
  "Add the wide (multi-byte) char to the window, then advance the cursor.

If the destination coordinates y and x are given, move the cursor to the
destination first and then add the character.

If n is given, write n chars. If n is -1, as many chars will be added
as will fit on the line."
  (when (and y x) (move window y x))
  (let ((winptr (.winptr window))
        ;; this does work as expected for %waddch but doesnt work for %wadd-wch.
        (attr (char2chtype 0 attributes color-pair)))
    (with-foreign-object (ptr '(:struct cchar_t))
      (setf ptr (convert-to-foreign (list 'cchar-attr attr 'cchar-chars (char-code char)) ; Lisp plist to C struct.
                                    '(:struct cchar)))
      (%wadd-wch winptr ptr))))

;; TODO: color-pair, n, see add-char
;;       proper type translation cchar_t->xchar