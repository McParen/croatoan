(in-package :de.anvi.croatoan)

(defun extract-complex-string (window &key y x position n)
  "Extract and return a complex string from the window.

Start at the current cursor position and end at the right margin of window. 

If the position coordinates y (row) and x (column) are given, move the
cursor to the position first and then add the character.

The position can also be passed in form of a two-element list.

If n is given, read at most n chars."
  (when (and y x) (move window y x))
  (when position (apply #'move window position))
  (let* ((count (if n n (- (.width window) (cadr (.cursor-position window)))))
         (complex-string (make-instance 'complex-string)))
    (loop for i from 0 to (1- count) do
         (vector-push-extend (extract-wide-char window) (.complex-char-array complex-string))
         (move-to window :right))
    complex-string))
