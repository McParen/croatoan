(in-package :de.anvi.croatoan)

(defclass grid ()
  ((grid-position-y
    :initarg       :grid-y
    :initform      0
    :type          (or null integer))
   (grid-position-x
    :initarg       :grid-x
    :initform      0
    :type          (or null integer))
   (grid-height
    :initarg       :grid-height
    :initform      nil
    :type          (or null integer))
   (grid-width
    :initarg       :grid-width
    :initform      nil
    :type          (or null integer))
   (cyclicp
    :initarg       :cyclic
    :initform      nil
    :type          boolean)
   (scrolling-enabled-p
    :initarg       :enable-scrolling
    :initform      nil
    :type          boolean)
   (region-position-y
    :initarg       :region-y
    :initform      nil
    :type          (or null integer))
   (region-position-x
    :initarg       :region-x
    :initform      nil
    :type          (or null integer))
   (region-width
    :initarg       :region-width
    :initform      nil
    :type          (or null integer))
   (region-height
    :initarg       :region-height
    :initform      nil
    :type          (or null integer)))

  (:documentation "Utility to track the display of items in a scrollable mxn grid, like a layout or a menu."))

(defmethod initialize-instance :after ((obj grid) &key grid-position grid-dimensions grid-geometry region-dimensions)
  (with-slots ((y grid-position-y) (x grid-position-x) (h grid-height) (w grid-width) (rh region-height) (rw region-width)) obj
    ;; the keyword position overrides the keywords y and x
    (when grid-position
      (setf y (car  grid-position)
            x (cadr grid-position)))
    ;; the keyword dimensions overrides width and height
    (when grid-dimensions
      (setf h (car  grid-dimensions)
            w (cadr grid-dimensions)))
    ;; geometry overrides y, x, width and height
    (when grid-geometry
      (setf y (nth 0 grid-geometry)
            x (nth 1 grid-geometry)
            h (nth 2 grid-geometry)
            w (nth 3 grid-geometry)))
    (when region-dimensions
      (setf rh (car  region-dimensions)
            rw (cadr region-dimensions)))))

(defun visible-grid-height (grid)
  (with-slots (scrolling-enabled-p region-height grid-height) grid
    (if scrolling-enabled-p region-height grid-height)))

(defun visible-grid-width (grid)
  (with-slots (scrolling-enabled-p region-width grid-width) grid
    (if scrolling-enabled-p region-width grid-width)))

(defgeneric move-left (obj)
  (:documentation "Move the current grid position one cell to the left."))

(defgeneric move-right (obj)
  (:documentation "Move the current grid position one cell to the right."))

(defgeneric move-up (obj)
  (:documentation "Move the current grid position one cell upwards."))

(defgeneric move-down (obj)
  (:documentation "Move the current grid position one cell downwards."))

(defmethod move-left ((grid grid))
  (with-slots (scrolling-enabled-p
               cyclicp
               (m grid-height)
               (n grid-width)
               (i grid-position-y)
               (j grid-position-x)
               (m0 region-position-y)
               (n0 region-position-x)
               (m1 region-height)
               (n1 region-width)) grid
    (if scrolling-enabled-p
        (if cyclicp
            ;; first scroll to the end of the grid, then cycle
            (if (> j 0)
                (progn
                  (decf j)
                  (when (< j n0) (decf n0)))
                (progn
                  (setf j (mod (1- j) n)
                        n0 (- n n1))))
            ;; only scroll the region, no cycling
            (progn
              (when (> j 0) (decf j))     ; when not in first column, move one column left
              (when (< j n0) (decf n0)))) ; when left of region, move region one column left
        (if cyclicp
            ;; no scrolling, cycle
            (setf j (mod (1- j) n))
            ;; no scrolling, no cycle
            (setf j (max (1- j) 0))))))

(defmethod move-right ((grid grid))
  (with-slots (scrolling-enabled-p
               cyclicp
               (m grid-height)
               (n grid-width)
               (i grid-position-y)
               (j grid-position-x)
               (m0 region-position-y)
               (n0 region-position-x)
               (m1 region-height)
               (n1 region-width)) grid
    (if scrolling-enabled-p
        (if cyclicp
            ;; first scroll to the end of the grid, then cycle
            (if (< j (1- n))
                (progn
                  (incf j)
                  (when (>= j (+ n0 n1)) (incf n0)))
                (progn
                  (setf j (mod (1+ j) n)
                        n0 0)))
            ;; only scroll the region, no cycling
            (progn
              (when(< j (1- n)) (incf j))         ; when not in last column, move one column right
              (when (>= j (+ n0 n1)) (incf n0)))) ; when right of region, move region one column right
        (if cyclicp
            ;; no scrolling, cycle
            (setf j (mod (1+ j) n))
            ;; no scrolling, no cycle
            (setf j (min (1+ j) (1- n)))))))

(defmethod move-up ((grid grid))
  (with-slots (scrolling-enabled-p
               cyclicp
               (n grid-width)
               (m grid-height)
               (i grid-position-y)
               (j grid-position-x)
               (m0 region-position-y)
               (n0 region-position-x)
               (m1 region-height)
               (n1 region-width)) grid
    (if scrolling-enabled-p
        (if cyclicp
            ;; first scroll to the end of the grid, then cycle
            (if (> i 0)
                (progn
                  (decf i)
                  (when (< i m0) (decf m0)))
                (progn
                  (setf i (mod (1- i) m)
                        m0 (- m m1))))
            ;; only scroll the region, no cycling
            (progn
              (when (> i 0) (decf i))     ; when not in first row, move one row up
              (when (< i m0) (decf m0)))) ; when above region, move region one row up
        (if cyclicp
            ;; no scrolling, cycle
            (setf i (mod (1- i) m))
            ;; no scrolling, no cycle
            (setf i (max (1- i) 0))))))

(defmethod move-down ((grid grid))
  (with-slots (scrolling-enabled-p
               cyclicp
               (n grid-width)
               (m grid-height)
               (i grid-position-y)
               (j grid-position-x)
               (m0 region-position-y)
               (n0 region-position-x)
               (m1 region-height)
               (n1 region-width)) grid
    (if scrolling-enabled-p
        (if cyclicp
            ;; first scroll to the end of the grid, then cycle
            (if (< i (1- m))
                (progn (incf i)
                       (when (>= i (+ m0 m1)) (incf m0)))
                (progn (setf i (mod (1+ i) m)
                             m0 0)))
            ;; only scroll the region, no cycling
            (progn
              (when (< i (1- m)) (incf i))        ; when not in last row, move one row down
              (when (>= i (+ m0 m1)) (incf m0)))) ; when below region, move region one row down
        (if cyclicp
            ;; no scrolling, cycle
            (setf i (mod (1+ i) m))
            ;; no scrolling, no cycle
            (setf i (min (1+ i) (1- m)))))))

(defclass layout (widget grid)
  ((padding-top
    :initarg       :padding-top
    :initform      0
    :type          integer
    :documentation "Padding on the top of every element in the layout.")
   (padding-bottom
    :initarg       :padding-bottom
    :initform      0
    :type          integer
    :documentation "Number of spaces added to the bottom of every element in the layout.")
   (padding-left
    :initarg       :padding-left
    :initform      0
    :type          integer
    :documentation "The padding added to the left of every element in the layout.")
   (padding-right
    :initarg       :padding-right
    :initform      0
    :type          integer
    :documentation "The padding added to the right of every element in the layout.")
   (elements
    :initarg       :elements
    :initform      nil
    :type          (or null cons)
    :documentation "A list of elements (including other layouts)."))

  (:documentation "A layout is a container widget containing elements positioned in a grid. Layouts can be nested."))

(defmethod initialize-instance :after ((obj layout) &key padding)
  (with-slots (padding-top padding-bottom padding-left padding-right) obj
    ;; the padding initarg (top bottom left right) overrides the individual slot initargs.
    (when padding
      (destructuring-bind (top bottom left right) padding
        (setf padding-top    top
              padding-bottom bottom
              padding-left   left
              padding-right  right)))))

(defmethod width ((obj layout))
  "The width of a layout consists of the max widths of the columns and the left and right padding."
  (with-slots ((n grid-width)
               (m grid-height)
               (y grid-position-y)
               (pl padding-left)
               (pr padding-right)
               elements) obj
    (apply #'+ (* n pl) (* n pr) (column-widths elements (list m n)))))

(defmethod height ((obj layout))
  "The height of a layout consists of the max heights of the rows and the top and bottom padding."
  (with-slots ((n grid-width)
               (m grid-height)
               (pt padding-top)
               (pb padding-bottom)
               elements) obj
    (apply #'+ (* m pt) (* m pb) (row-heights elements (list m n)))))

(defun nth2d (position dimensions list)
  "Return the position (y x) of list by assuming grid dimensions (m n) and row major order."
  (nth (sub2rmi dimensions position) list))

(defun column-widths (list dimensions)
  "Take a list of strings, return a list of max widths of every column."
  (destructuring-bind (m n) dimensions
    (loop for j from 0 below n collect
      (loop for i from 0 below m maximize
        (let ((item (nth2d (list i j) dimensions list)))
          (width item))))))

(defun row-heights (list dimensions)
  "Take a list of strings, return a list of max heights of every row."
  (destructuring-bind (m n) dimensions
    (loop for i from 0 below m collect
      (loop for j from 0 below n maximize
        (let ((item (nth2d (list i j) dimensions list)))
          (height item))))))

(defun calculate-positions (layout)
  "Recursively set the position (y x) of each of the layout's children."
  (with-slots ((m grid-height) (n grid-width) (y position-y) (x position-x)
               (pt padding-top) (pb padding-bottom) (pl padding-left) (pr padding-right) elements) layout
    (let* ((widths (column-widths elements (list m n)))
           (heights (row-heights elements (list m n)))
           (widths2 (loop for i from 0 to (length widths) collect (reduce #'+ (subseq widths 0 i))))
           (heights2 (loop for i from 0 to (length heights) collect (reduce #'+ (subseq heights 0 i)))))
      (loop for el from 0 below (length elements) do
        (when (nth el elements)
          (let* ((i (car (rmi2sub (list m n) el)))
                 (j (cadr (rmi2sub (list m n) el))))
            (setf (widget-position (nth el elements))
                  (list (+ y (+ (* pt (1+ i))
                                (* pb i)
                                (nth i heights2)))
                        (+ x (+ (* pl (1+ j))
                                (* pr j)
                                (nth j widths2))))))
          ;; after setting the position, check if the element is a layout object,
          ;; then recursively set the positions of its child elements.
          (when (typep (nth el elements) 'layout)
            (calculate-positions (nth el elements))))))))

(defun flatten-elements (layout)
  "Take a layout object, return a flattened list of its elements to be passed to a form.

Nested layouts are spliced in, nils removed and strings/symbols/numbers converted to labels."
    (let (elements)
      (labels ((flatten (list)
                 (dolist (i list)
                   (cond ((typep i 'layout)
                          ;; push elements of a nested layout recursively
                          (flatten (slot-value i 'elements)))
                         ((typep i 'element)
                          (push i elements))
                         ((null i)
                          ;; ignore nil layout placeholders
                          nil)
                         ((atom i)
                          ;; strings, numbers, symbols
                          (push (make-instance 'label :title (princ-to-string i)) elements))))))
        (flatten (slot-value layout 'elements))
        (nreverse elements))))
