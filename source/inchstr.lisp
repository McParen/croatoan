(in-package :de.anvi.croatoan)

(defun extract-complex-string (window &key y x n)
  "Extract and return a complex string from the window.

Start at the current cursor position and end at the right margin of window. 

If n is given explicitely, read at most n chars.

If the destination coordinates y and x are given, move the cursor first."
  (when (and y x) (move window y x))
  (let* ((count (if n n (- (.width window) (cadr (.cursor-position window)))))
         (complex-string (make-instance 'complex-string)))
    (loop for i from 0 to (1- count) do
         (vector-push-extend (extract-char window) (.complex-char-array complex-string))
         (move-to window :right))
    complex-string))
