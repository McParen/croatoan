(in-package :de.anvi.croatoan)

;; shapes
;; curses extension for plotting shapes
;; author: Daniel Vedder <daniel@terranostra.one>

(defclass shape ()
  (;; TODO: merge -x and -y into one location in form of a list to correspond to the position/location slot of other elements.
   (origin-x
    :initform       0
    :initarg        :x0
    :type           integer
    :accessor       origin-x
    :documentation "The x coordinate of this shape's point of origin.")
		
   (origin-y
    :initform      0
    :initarg       :y0
    :type          integer
    :accessor      origin-y
    :documentation "The y coordinate of this shape's point of origin.")

   ;; Coordinates are stored as '(y x) pairs (note the order!)
   (coordinates
    :initform      nil
    :type          (or null cons)
    :accessor      coordinates
    :documentation "A list of coordinates relative to the origin that form this shape.")

   (plot-char
    :initform      (make-instance 'complex-char :simple-char #\X :color-pair '(:white :black) :attributes nil)
    :initarg       :char
    :type          (or null character keyword complex-char)
    :accessor      plot-char
    :documentation "The character to use for plotting."))

  (:documentation "A shape is a list of coordinates, relative to an origin, that can be plotted in a window."))

;;; General shape methods

(defun shape-extent (shape)
  "Return min-y, min-x, max-y, and max-x of a shape's coordinates as multiple values."
  (let ((y-vals (mapcar #'first (coordinates shape)))
        (x-vals (mapcar #'second (coordinates shape))))
    (values (apply #'min y-vals) (apply #'min x-vals)
            (apply #'max y-vals) (apply #'max x-vals))))

(defun fill-shape (shape)
  "Take a shape that only shows the borders and 'color it out'."
  ;; Every point inside a shape has, on the same axis, a point larger and one smaller than itself.
  (flet ((inside-p (pt shape)
           (and (member pt (coordinates shape)
                        :test #'(lambda (p c) (and (= (first p) (first c))
                                              (> (second p) (second c)))))
                (member pt (coordinates shape)
                        :test #'(lambda (p c) (and (= (first p) (first c))
                                              (< (second p) (second c)))))
                (member pt (coordinates shape)
                        :test #'(lambda (p c) (and (= (second p) (second c))
                                              (> (first p) (first c)))))
                (member pt (coordinates shape)
                        :test #'(lambda (p c) (and (= (second p) (second c))
                                              (< (first p) (first c))))))))
    ;; Iterate over the rectangle that encloses the shape, adding any
    ;; points inside the shape's borders to its coordinates
    (do* ((extent (multiple-value-list (shape-extent shape)))
          (min-y (first extent)) (min-x (second extent))
          (max-y (third extent)) (max-x (fourth extent))
          (y min-y (1+ y)))
         ((> y max-y) shape)
      (do ((x min-x (1+ x)))
          ((> x max-x))
        (when (inside-p (list y x) shape)
          (setf (coordinates shape)
                (append (coordinates shape) (list (list y x)))))))))

(defun merge-shapes (&rest shapes)
  "Create a new shape object by merging the coordinates of a given list of shapes."
  ;; This keeps the first shape's point of origin and plot-char.
  ;; A completely new object is created and new lists consed up.
  (let ((shp (make-instance 'shape
                            :char (plot-char (first shapes))
                            :y0 (origin-y (first shapes))
                            :x0 (origin-x (first shapes)))))
    (dolist (s shapes shp)
      (dolist (c (coordinates s))
        (unless (member c (coordinates shp) :test #'equal)
          (setf (coordinates shp)
                (append (coordinates shp)
                        (list (list (first c) (second c))))))))))

;;; Create various basic shapes

(defun line (y0 x0 y1 x1 &key char)
  "Return a straight line between two points"
  ;;make sure we're moving from left to right
  (let (zx zy)
    (when (or (> x0 x1) (and (= x0 x1) (> y0 y1)))
      (setf zx x1 zy y1)
      (setf x1 x0 y1 y0)
      (setf x0 zx y0 zy)))
  ;;increment x from x0 to x1, building a list of coordinates as we go
  (do* ((l (make-instance 'shape)) (coords nil)
        (slope (if (= x0 x1) (abs (- y0 y1)) ;;prevent division-by-zero
                   (/ (- y1 y0) (- x1 x0))))
        (x 0 (1+ x)) (y (round (* x slope)) (round (* x slope))))
       ((< x1 (+ x0 x)) ;;finalise the shape object and return it
        (setf (coordinates l) coords)
        (when char (setf (plot-char l) char))
        l)
    ;;for each x value, figure out how many characters we need to print
    ;; in the y direction (depends on the slope gradient)
    (do* ((next-x (+ x0 x)) (dy 0 (1+ dy))
          (next-y (+ y0 y (if (plusp slope) dy (* -1 dy)))
                  (+ y0 y (if (plusp slope) dy (* -1 dy)))))
         ;;stop when we have stacked sufficient vertical coordinates
         ((or (and (>= 1 (abs slope)) (= dy 1)) ;;shallow slopes
              (and (< 1 (abs slope)) (= dy (ceiling (abs slope)))) ;;steep
              (if (plusp slope) ;;don't overshoot the end
                  (or (> next-y y1) (> next-x x1))
                  (or (< next-y y1) (> next-x x1)))))
      ;;append the next pair of coordinates
      (setf coords (append coords (list (list next-y next-x)))))))

(defun angle-line (y0 x0 theta length &key char)
  "Draw a line of the given length in the bearing theta from the origin."
  ;; theta = 0 -> vertically up; theta = 90 -> horizontally right
  (let* ((radians (* pi (/ (- theta 90) 180.0)))
         (y1 (+ y0 (* length (sin radians))))
         (x1 (+ x0 (* length (cos radians)))))
    (line y0 x0 (round y1) (round x1) :char char)))

(defun polygon (corners &key filled char)
  "Return a polygon along a list of corners, optionally filled"
  (do* ((pol (make-instance 'shape))
        (i 0 (1+ i)) (j (1+ i) (1+ i)))
       ((= i (length corners))
        (when char (setf (plot-char pol) char))
        (if filled (fill-shape pol) pol))
    (when (= j (length corners)) (setf j 0))
    (setf pol (merge-shapes pol
                            (line (first (nth i corners)) (second (nth i corners))
                                  (first (nth j corners)) (second (nth j corners))
                                  :char char)))))

(defun triangle (y0 x0 y1 x1 y2 x2 &key filled char)
  "Return a triangle (utility wrapper around `polygon')."
  (polygon (list (list y0 x0) (list y1 x1) (list y2 x2))
           :filled filled :char char))

(defun quadrilateral (y0 x0 y1 x1 y2 x2 y3 x3 &key filled char)
  "Return a quadrilateral (utility wrapper around `polygon')."
  (polygon (list (list y0 x0) (list y1 x1) (list y2 x2) (list y3 x3))
           :filled filled :char char))

(defun rectangle (y0 x0 height width &key filled char)
  "Return a rectangle (utility wrapper around `polygon')."
  (polygon (list (list y0 x0) (list y0 (+ x0 (1- width)))
                 (list (+ y0 (1- height)) (+ x0 (1- width)))
				 (list (+ y0 (1- height)) x0))
           :filled filled :char char))

(defun circle (y0 x0 radius &key filled char)
  "Return a circle with a given radius, optionally filled."
  (do* ((shp (make-instance 'shape)) (coords nil) (deg 0 (1+ deg))
        (radians (* pi (/ deg 180.0)) (* pi (/ deg 180.0)))
        (y (+ y0 (round (* radius (sin radians))))
           (+ y0 (round (* radius (sin radians)))))
        (x (+ x0 (round (* radius (cos radians))))
           (+ x0 (round (* radius (cos radians)))))
			 (last-coord nil coord) (coord (list y x) (list y x)))
       ((= deg 360)
        (setf (coordinates shp) coords)
        (when char (setf (plot-char shp) char))
        (if filled (fill-shape shp) shp))
    (unless (equal coord last-coord)
      (setf coords (append coords (list coord))))))

;;; Integrate shapes with the rest of croatoan

(defun draw-shape (window shape &optional squarify)
  "Draw a shape in the given window."
  ;; If squarify is on, draw-shape doubles the width of the shape to compensate
  ;; for the fact that terminal fonts are higher than they are wide
  (do* ((y0 (origin-y shape)) (x0 (origin-x shape)) (c (plot-char shape))
        (coords (coordinates shape) (cdr coords))
        (y (first (car coords)) (first (car coords)))
        (x (second (car coords)) (second (car coords))))
       ((null coords))
    (setf y (+ y0 y) x (+ x0 x))
    (when squarify (setf x (* 2 x)))
    (unless (or (minusp y) (minusp x) (>= y (height window)) (>= x (width window)))
      (add window (simple-char c) :y y :x x :attributes (attributes c) :color-pair (color-pair c)))))
