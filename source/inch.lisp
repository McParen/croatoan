(in-package :de.anvi.croatoan)

(defun extract-complex-char (window &key y x)
  "Extract and return the single-byte complex char from the window.

If the destination coordinates y and x are given, move the cursor
there first."
  (let* ((winptr (.winptr window))
         (chtype (cond ((and y x)
                        (%mvwinch winptr y x))
                       (t
                        (%winch winptr)))))
    (c2x chtype)))
