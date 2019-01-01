(in-package :de.anvi.croatoan)

;; shapes
;; curses extension for plotting shapes

(defclass shape ()
	((x-origin
		 :initform  0
		 :initarg :x0
		 :type (or null integer)
		 :accessor .x-origin
		 :documentation "The x coordinate of this shape's point of origin")
		
		(y-origin
			:initform  0
			:initarg :y0
			:type (or null integer)
			:accessor .y-origin
			:documentation "The y coordinate of this shape's point of origin")
		
		(coordinates
			;;Coordinates are stored as '(y x) pairs (note the order!)
			:initform  nil
			:type (or null cons)
			:accessor .coordinates
			:documentation "A list of coordinates relative to the origin that form this shape")
		
		(plot-char
			:initform (make-instance 'complex-char
						  :simple-char #\X
						  :color-pair '(:white :black)
						  :attributes nil)
			:initarg :char
			:type (or null complex-char)
			:accessor .plot-char
			:documentation "The complex character to use for plotting"))
	(:documentation "A shape is a list of coordinates, relative to an origin, that can be plotted in a window."))

(defmethod shape-extent ((shape shape))
	"Return min-y, min-x, max-y, and max-x of a shape's coordinates"
	;;TODO
	)

(defmethod draw-shape ((shape shape) (win window) &optional (char complex-char))
	(let ((y0 (.y-origin shape)) (x0 (.x-origin shape))
			 (c (if char char (.plot-char shape))))
		(dolist (coord (.coordinates shape))
			(add-char win (.simple-char c)
				:y (+ y0 (first coord)) :x (+ x0 (second coord))
				:attributes (.attributes c) :color-pair (.color-pair c)))))

(defmethod delete-shape ((shape shape) (win window) &optional (bg-col :black))
	 "A utility function to delete a shape by drawing over it in black"
	 (draw-shape shape window
		  (make-instance 'complex-char
				:simple-char #\space :color-pair (list bg-col bg-col))))

(defmethod fill-shape ((shape shape))
	"Take a shape that only shows the borders and 'color it out'"
	;;TODO
	)

(defun merge-shapes (shapes)
	"Create a new shape by merging the coordinates of a given list of shapes"
	;; This keeps the first shapes point of origin and plot-char.
	;; A completely new object is created and new lists consed up.
	;;TODO
	)

;;; The following functions return a shape object that can be passed to draw-shape

;;XXX This function is almost indecipherable - convert it to simple trig!
;; (It's a direct port of the following Python code)
;;
;; def diagonal_line(x1, y1, x2, y2):
;;     if x2 < x1:
;;         x1,x2 = x2, x1
;;         y1,y2 = y2, y1
;;     shape = []
;;     slope = (x2-x1)/(y2-y1)
;;     # XXX a bit ugly, but it works
;;     if abs(slope) < 1: # steep lines
;;         for y in range(y1, y2+1):
;;             x = int(round(x2 - ((y2-y)*slope)))
;;             shape.append((x,y))
;;     else: # shallow lines
;;         for x in range(x1, x2+1):
;;             y = int(round(y2 - ((x2-x)/slope)))
;;             shape.append((x,y))
;;     return shape

(defun line (y0 x0 y1 x1 &key char)
	"Return a straight line between two points"
	(when (> x0 x1)
		;;reverse the positions to ensure a positive slope
		;;XXX is this really necessary?
		(setf zx x0 zy y0)
		(setf x1 x0 y1 y0 x0 zx y0 zy))
	(do* ((l (make-instance 'shape :char char))
			 (coords nil)
			 (slope (/ (- y1 y0) (- x1 x0)))
			 (steep-p (> (abs slope) 1))
			 (p (if steep-p y0 x0) (1+ p))
			 (q (round (- (if steep-p x1 y1)
						   (* slope (- (if steep-p y1 x1) p))))))
		((= p (1+ (if steep-p y1 x1)))
			(setf (.coordinates l) coords)
			l)
		(setf coords (append coords (if steep-p (list q p) (list p q))))))

(defun polygon (corners &key filled char)
	"Return a polygon along a list of corners, optionally filled"
	(do* ((pol (make-instance 'shape :char char))
			 (i 0 (1+ i)) (j (1+ i) (1+ i)))
		((= i (length corners)) (if filled (fill-shape pol) pol))
		(when (= j (length corners)) (setf j 0))
		(merge-shapes pol (line (first (nth i corners)) (second (nth i corners))
							  (first (nth j corners)) (second (nth j corners)) char))))

(defun triangle (y0 x0 y1 x1 y2 x2 &key filled char)
	"Return a triangle (utility wrapper around `polygon')"
	(polygon (list y0 x0 y1 x1 y2 x2) :filled filled :char char))

(defun rectangle (y0 x0 y1 x1 y2 x2 y3 x3 &key filled char)
	"Return a rectangle (utility wrapper around `polygon')"
	(polygon (list y0 x0 y1 x1 y2 x2 y3 x3) :filled filled :char char))

(defun circle (y0 x0 radius &key filled char)
	"Return a circle with a given radius, optionally filled"
	;;TODO
	)

