(in-package :de.anvi.croatoan)

(defun extract-wide-string (window &key y x n)
  "Extract and return a string from window.

Any attributes are stripped from the characters before the string is returned.

Start at the current cursor position and end at the right margin of window. 

If n is given explicitely, read at most n chars.

If the destination coordinates y and x are given, move the cursor
to the destination first."
  (when (and y x) (move window y x))
  (let ((count (if n n (distance-to-eol window)))
        ;; start with an empty string as buffer
        (str (make-array '(0) :element-type 'character :fill-pointer 0 :adjustable t)))
    (loop for i from 0 to (1- count) do
         (vector-push-extend (.simple-char (extract-wide-char window)) str)
         (move-to window :right))
    ;; return string buffer containing i chars
    str))
