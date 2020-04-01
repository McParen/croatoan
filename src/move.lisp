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

(defun goto (win &rest positions)
  "Move cursor to the window position given as a two-element list (y x).

If more than one position is given, add their coordinates before moving the cursor.

This allows to address element-relative coordinates or cursor movement to a
different position."
  (if positions
      (if (cdr positions)
          ;; if there is more than one position given, add them before moving.
          (apply #'move win (apply #'mapcar #'+ positions))
          ;; if only one position is given, move the cursor to it.
          (apply #'move win (car positions)))
      ;; if no position is given, move to the home position, the top left corner.
      (move win 0 0)))

(defun get-direction (direction-name)
  "Take a keyword name of a direction, get a direction given as a two-element list."
  (case direction-name
    (:up-left    '(-1 -1))
    (:up         '(-1  0))
    (:up-right   '(-1  1))
    (:left       '( 0 -1))
    (:right      '( 0  1))
    (:down-left  '( 1 -1))
    (:down       '( 1  0))
    (:down-right '( 1  1))))

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
