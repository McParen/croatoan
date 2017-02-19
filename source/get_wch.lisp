(in-package :de.anvi.croatoan)

(defun get-wide-char (window &key y x)
  "Read in a wide C wchar_t (multi-byte) from the keyboard and return it.

If the destination coordinates y (row) and x (column) are given, move
the cursor to the destination first and then read a multi-byte char.

The window from which the char is read is automatically refreshed."
  (when (and y x) (move window y x))

  (with-foreign-object (ptr 'wint_t)
    ;; #define KEY_CODE_YES    0400            /* A wchar_t contains a key code */
    ;; if the char is a function key, return t as a second value, otherwise nil.
    (if (= 256 (%wget-wch (.winptr window) ptr))
        (values (mem-ref ptr 'wint_t) t)
        (values (mem-ref ptr 'wint_t) nil))))
