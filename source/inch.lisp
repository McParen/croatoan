(in-package :de.anvi.croatoan)

(defun extract-char (window &key y x)
  "Extract and return the single-byte complex char from the window.

If the destination coordinates y and x are given, move the cursor
there first."
  (when (and y x) (move window y x))
  (let* ((winptr (.winptr window))
         (chtype (%winch winptr)))
    (chtype2xchar chtype)))
