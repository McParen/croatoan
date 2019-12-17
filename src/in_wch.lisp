(in-package :de.anvi.croatoan)

(defun extract-wide-char (window &key y x position)
  "Extract and return a single wide (complex) character from the window.

This includes wide characters (code > 255), and requires the ncursesw library.

If the position coordinates y (row) and x (column) are given, move the
cursor to the position first and then add the character.

The position can also be passed in form of a two-element list."
  (when (and y x) (move window y x))
  (when position (apply #'move window position))
  (funcall-get-cchar_t #'%win-wch window))
