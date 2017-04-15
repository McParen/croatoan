(in-package :de.anvi.croatoan)

(defun extract-wide-char (window &key y x)
  "Extract and return a single (complex) character from the window.

This includes wide characters (code > 255), and requires the ncursesw library.

If the destination coordinates y and x are given, move the cursor first."
  (when (and y x) (move window y x))
  (with-foreign-object (ptr '(:struct cchar_t))
    ;; read a struct cchar_t into the space allocated with ptr
    (%win-wch (.winptr window) ptr)
    ;; the slot cchar-chars is a a pointer to the wchar_t array.
    (let* ((char (mem-aref (foreign-slot-pointer ptr '(:struct cchar_t) 'cchar-chars) 'wchar_t 0))
           ;; the color pair is not placed into the cchar_t slot ext_color, but ORed into the attribute int.
           ;;(col (foreign-slot-value ptr '(:struct cchar_t) 'cchar-colors))
           (attr (foreign-slot-value ptr '(:struct cchar_t) 'cchar-attr)))
      (make-instance 'complex-char
                     :simple-char (code-char char)
                     :attributes (chtype2attrs attr)
                     :color-pair (chtype2colors attr) ))))
