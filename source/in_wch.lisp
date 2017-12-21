(in-package :de.anvi.croatoan)

(defun extract-wide-char (window &key y x)
  "Extract and return a single wide (complex) character from the window.

This includes wide characters (code > 255), and requires the ncursesw library.

If the destination coordinates y and x are given, move the cursor first."
  (when (and y x) (move window y x))
  (funcall-get-cchar_t #'%win-wch window))
