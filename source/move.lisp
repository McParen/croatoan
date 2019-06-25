(in-package :de.anvi.croatoan)

(defun move (window y x &key relative)
  "Move cursor to the position given by row y and column x.

If relative is t, move the cursor by y rows and x columns."
  (let ((winptr (winptr window)))
    (if relative
        (let ((pos-y (car  (cursor-position window)))
              (pos-x (cadr (cursor-position window))))
          (%wmove winptr (+ pos-y y) (+ pos-x x)))
        (%wmove winptr y x))))

(defun move-direction (window direction &optional (n 1))
  "Move cursor in the given direction by n cells."
  (case direction
    (:left  (move window        0 (* n -1) :relative t))
    (:right (move window        0 (* n  1) :relative t))
    (:up    (move window (* n -1)        0 :relative t))
    (:down  (move window (* n  1)        0 :relative t))
    (otherwise (error "Valid cursor movement directions: :left, :right, :up, :down"))))

(defun move-window (window y x &key relative)
  "Move top left corner of the window to row y and column x.

If relative is t, move the window by y rows and x columns."
  (let ((winptr (winptr window)))
    (if relative
        (let ((pos-y (car  (location window)))
              (pos-x (cadr (location window))))
          (%mvwin winptr (+ pos-y y) (+ pos-x x)))
        (%mvwin winptr y x))))
