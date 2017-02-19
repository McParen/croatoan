(in-package :de.anvi.croatoan)

(defun extract-wide-char (window &key y x)
  "Extract and return a wide character (code > 255) from the window.

If the destination coordinates y and x are given, move the cursor
to the destination first."
  (when (and y x) (move window y x))

  (with-foreign-object (ptr '(:struct cchar_t)) ; we absolutely need cchar_t here, cchar crashes sbcl.
    ;; read a struct cchar_t into the space allocated with ptr
    (%win-wch (.winptr window) ptr)
    ;; for testing purposes, print the struct cchar_t pointed to by ptr
    ;;(%wadd-wch (.winptr window) ptr)))
    ;; when we use cchar, the int is dereferenced, cchar_t returns a pointer to the wchar_t array.
    ;; convert-from-foreign returns a plist, then we read the char from the plist using getf
    (getf (convert-from-foreign ptr '(:struct cchar)) 'cchar-chars) ))

#|

;;TODO: extract the attributes, then construct and return a complex-char
;;(getf (convert-from-foreign ptr '(:struct cchar)) 'cchar-attr) ))
(make-instance 'complex-char
               :simple-char __char__
               :attributes (loop for i in *valid-attributes* if (logtest __attr__ (get-bitmask i)) collect i)
               ;; first get the color attribute bits by log-AND-ing them with ch.
               ;; then right shift them by 8 to extract the color int from them.
               ;; then get the color pair (:white :black) associated with that number.
               :color-pair (number->pair (ash (logand __attr__ (get-bitmask :color)) -8)))

|#
