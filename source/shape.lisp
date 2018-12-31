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
		  
