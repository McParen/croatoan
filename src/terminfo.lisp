(in-package :de.anvi.croatoan)

(defun tigetstr (capname)
  "Take a terminfo capability name, return its string value.

Error return values:

-1  if capname is not a string capability,
 0  if it is canceled or absent from the terminal description.

This function is just a thin convenience wrapper around
ncurses:tigetstr, so we dont have to deal with C pointers."
  (let ((ptr (ncurses:tigetstr capname)))
    (cond ((ncurses:invalid-pointer-p ptr)
           ;; not a string capability
           -1)
          ;; canceled or absent from terminfo
          ((cffi:null-pointer-p ptr)
           0)
          (t
           ;; valid string value
           (cffi:foreign-string-to-lisp ptr)))))
