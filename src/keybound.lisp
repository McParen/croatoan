(in-package :de.anvi.croatoan)

;; keybound
;; get definition of curses keycode
;; https://invisible-island.net/ncurses/man/keybound.3x.html

(defun function-key-definition (code)
  "Take a function key code, return its definition as a string.

The key definition is a escape sequence of characters that is
generated by the keyboard and recognized by ncurses as a single
function key.

Also see: function-key-code."
  (let* ((ptr (cffi:foreign-funcall "keybound" :int code :int 0 :pointer))
         (str (cffi:foreign-string-to-lisp ptr)))
    ;; keybound returns a C string which must be freed.
    ;; ncurses-6.4/ncurses/base/keybound.c
    (cffi:foreign-free ptr)
    str))
