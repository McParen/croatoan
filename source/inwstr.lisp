(in-package :de.anvi.croatoan)

(defun extract-wide-string (window &key y x position n)
  "Extract and return a string from window.

Any attributes are stripped from the characters before the string is returned.

Start at the current cursor position and end at the right margin of window. 

If the position coordinates y (row) and x (column) are given, move the
cursor to the position first and then add the character.

The position can also be passed in form of a two-element list.

If n is given, read at most n chars."
  (when (and y x) (move window y x))
  (when position (apply #'move window position))
  (let ((count (if n n (distance-to-eol window)))
        ;; start with an empty string as buffer
        (str (make-array '(0) :element-type 'character :fill-pointer 0 :adjustable t)))
    (loop for i from 0 to (1- count) do
         (vector-push-extend (simple-char (extract-wide-char window)) str)
         (move-direction window :right))
    ;; return string buffer containing i chars
    str))
