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
	(let ((y-vals (mapcar #'first (.coordinates shape)))
			 (x-vals (mapcar #'second (.coordinates shape))))
		(values (apply #'min y-vals) (apply #'min x-vals)
			(apply #'max y-vals) (apply #'max x-vals))))

(defun inbounds-p (coordinate win)
	"Test whether the given coordinate is within the given window"
	(not (or (minusp (first coordinate))
			 (minusp (second coordinate))
			 (>= (first coordinate) (.height win))
			 (>= (second coordinate) (.width win)))))

;;TODO add `squarify` option to print extra blank space between chars
(defmethod draw-shape ((shape shape) (win window))
	(with-accessors ((y0 .y-origin) (x0 .x-origin) (c .plot-char)) shape
		(dolist (coord (.coordinates shape))
			(when (inbounds-p coord win) ;;xxx needs adjusted coordinates!
				(add-char win (.simple-char c)
					:y (+ y0 (first coord)) :x (+ x0 (second coord))
					:attributes (.attributes c) :color-pair (.color-pair c))))))

(defmethod delete-shape ((shape shape) (win window) &optional (bg-col :black))
	 "A utility function to delete a shape by drawing over it in black"
	 (draw-shape shape window
		  (make-instance 'complex-char
				:simple-char #\space :color-pair (list bg-col bg-col))))

(defmethod fill-shape ((shape shape))
	"Take a shape that only shows the borders and 'color it out'"
	shape
	;;TODO
	)

(defun merge-shapes (&rest shapes)
	"Create a new shape object by merging the coordinates of a given list of shapes"
	;; This keeps the first shape's point of origin and plot-char.
	;; A completely new object is created and new lists consed up.
	(let ((shp (make-instance 'shape :char (.plot-char (first shapes))
				   :y0 (.y-origin (first shapes))
				   :x0 (.x-origin (first shapes)))))
		(dolist (s shapes shp)
			(dolist (c (.coordinates s))
				(unless (member c (.coordinates shp) :test #'equal)
					(setf (.coordinates shp)
						(append (.coordinates shp)
							(list (list (first c) (second c))))))))))

;;; The following functions return a shape object that can be passed to draw-shape

(defun line (y0 x0 y1 x1 &optional char)
	"Return a straight line between two points"
	(when (or (> x0 x1) (and (= x0 x1) (> y0 y1)))
		;;make sure we're moving from left to right
		(setf zx x1 zy y1)
		(setf x1 x0 y1 y0)
		(setf x0 zx y0 zy))
	(do* ((l (make-instance 'shape)) (coords nil)
			 (slope (if (= x0 x1) (abs (- y0 y1)) ;;prevent division-by-zero
						(/ (- y1 y0) (- x1 x0))))
			 (x 0 (1+ x)) (y (round (* x slope)) (round (* x slope))))
		((< x1 (+ x0 x))
			(setf (.coordinates l) coords)
			(when char (setf (.plot-char l) char))
			l)
		(if (< slope 1) ;;shallow slope
			(setf coords (append coords (list (list (+ y0 y) (+ x0 x)))))
			(dotimes (dy (round slope)) ;;steep slope
				(setf coords (append coords
								 (list (list (+ y0 y dy) (+ x0 x)))))))))

(defun polygon (corners &key filled char)
	"Return a polygon along a list of corners, optionally filled"
	(do* ((pol (make-instance 'shape))
			 (i 0 (1+ i)) (j (1+ i) (1+ i)))
		((= i (length corners))
			(when char (setf (.plot-char pol) char))
			(if filled (fill-shape pol) pol))
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

;;; Development function - move to test suite later?

(defun display-shape (shp)
	"Open a screen display and draw the shape."
	(with-screen (scr :input-blocking T :enable-colors t :input-echoing nil
					 :cursor-visibility nil :input-reading :unbuffered)
		(clear scr)
		(draw-shape shp scr)
		(event-case (scr event)
			(otherwise (return-from event-case)))))
