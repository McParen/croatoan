(in-package :de.anvi.croatoan)

(defun move (window y x &key relative)
  "Move cursor to the position given by row y and column x.

If relative is t, move the cursor by y rows and x columns relative to the
current cursor position."
  (with-accessors ((pos cursor-position)) window
    (let* ((pos-y (car pos))
           (pos-x (cadr pos))
           (new-pos (if relative
                        (list (+ pos-y y) (+ pos-x x))
                        (list y x))))
      (setf pos new-pos))))

(defun goto (window &rest positions)
  "Move cursor to the window position given as a two-element list (y x).

If more than one position is given, add their coordinates before moving the cursor.

This allows to address element-relative coordinates or cursor movement to a
different position."
  (with-accessors ((pos cursor-position)) window
    (if positions
        (if (cdr positions)
            ;; if there is more than one position given, add them before moving.
            (setf pos (apply #'mapcar #'+ positions))
            ;; if only one position is given, move the cursor to it.
            (setf pos (car positions)))
        ;; if no position is given, move to the home position, the top left corner.
        (setf pos '(0 0)))))

(defun get-direction (direction-name)
  "Take a keyword name of a direction, return a direction given as a two-element list.

The two-element list is given as (y x), where y is the vertical
direction up (-1) or down (+1) and x is the horizontal direction
left (-1) or right (+1).

Calculated that way, we have 8 possible directions:

-1,-1    -1,0    -1,1
 0,-1             0,1
 1,-1     1,0     1,1

The direction is a 2D increment (dy dx) that can be added to a
position (y x) to get a new position (y+dy x+dx)."
  (case direction-name
    (:up-left         '(-1 -1))
    (:up              '(-1  0))
    (:up-right        '(-1  1))
    (:left            '( 0 -1))
    (:right           '( 0  1))
    (:down-left       '( 1 -1))
    (:down            '( 1  0))
    (:down-right      '( 1  1))))

(defun move-direction (window direction &optional (n 1))
  "Move cursor in the given direction by n cells."
  (flet ((multiply (x) (* n x)))
    (let* ((dir (get-direction direction))
           (offset (if (> n 1)
                       (mapcar #'multiply dir)
                       dir)))
      (move window (car offset) (cadr offset) :relative t))))

;;; TODO 210628 rename to move-widget
(defun move-window (window y x &key relative)
  "Move top left corner of the window to row y and column x.

If relative is t, move the window by y rows and x columns relative to its current position."
  (with-accessors ((pos widget-position)) window
    (let* ((pos-y (car pos))
           (pos-x (cadr pos))
           (new-pos (if relative
                        (list (+ pos-y y) (+ pos-x x))
                        (list y x))))
      (setf pos new-pos))))
