(in-package :de.anvi.croatoan)

;; shapes
;; curses extension for plotting shapes

(defclass shape ()
	 ((x-origin
			:initform  0
			:type (or null integer)
			:accessor .x-origin
			:documentation "The x coordinate of this shape's point of origin")
		  
		  (y-origin
				:initform  0
				:type (or null integer)
				:accessor .y-origin
				:documentation "The y coordinate of this shape's point of origin")

		  (coordinates
				:initform  nil
				:type (or null cons)
				:accessor .coordinates
				:documentation "A list of coordinates relative to the origin that form this shape")

		  (plot-char
				:initform (make-instance 'complex-char
								  :simple-char #\X
								  :color-pair '(:white :black)
								  :attributes nil)
				:type (or null complex-char)
				:accessor .plot-char
				:documentation "The complex character to use for plotting"))
	 (:documentation "A shape is a list of coordinates, relative to an origin, that can be plotted in a window."))
		  
(defmethod draw-shape ((shape shape) (win window) &optional (char complex-char))
	 ;;TODO
	 )

(defmethod delete-shape ((shape shape) (win window))
	 "A utility function to delete a shape by drawing over it in black"
	 (draw-shape shape window
		  (make-instance 'complex-char
				:simple-char #\space :color-pair '(:black :black))))

;;; XXX invert x and y positions? (ncurses tradition)

(defun line (x0 y0 x1 y1 &key char)
	 "Return a straight line between two points"
	 ;;TODO
	 )

(defun polygon (corners &key filled char)
	 "Return a polygon along a list of corners, keyly filled"
	 ;;TODO
	 )

(defun triangle (x0 y0 x1 y1 x2 y2 &key filled char)
	 "Return a triangle (utility wrapper around `polygon')"
	 ;;TODO
	 )

(defun rectangle (x0 y0 x1 y1 x2 y2 x3 y3 &key filled char)
	 "Return a rectangle (utility wrapper around `polygon')"
	 ;;TODO
	 )

(defun circle (x0 y0 radius &key filled char)
	 "Return a circle with a given radius, optionally filled"
	 ;;TODO
	 )
