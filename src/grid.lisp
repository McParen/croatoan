(in-package :de.anvi.croatoan)

(defclass grid ()
  ((grid-row
    :initarg       :grid-row
    :initform      0
    :type          integer)
   (grid-column
    :initarg       :grid-column
    :initform      0
    :type          integer)
   (grid-rows
    :initarg       :grid-rows
    :initform      nil
    :type          (or null integer))
   (grid-columns
    :initarg       :grid-columns
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
   (region-start-row
    :initarg       :region-start-row
    :initform      nil
    :type          (or null integer))
   (region-start-column
    :initarg       :region-start-column
    :initform      nil
    :type          (or null integer))
   (region-rows
    :initarg       :region-rows
    :initform      nil
    :type          (or null integer))
   (region-columns
    :initarg       :region-columns
    :initform      nil
    :type          (or null integer)))

  (:documentation "Utility to track the display of items in a scrollable mxn grid, like a layout or a menu."))

(defmethod initialize-instance :after ((obj grid) &key grid-position grid-dimensions grid-geometry region-dimensions)
  (with-slots (grid-row grid-column grid-rows grid-columns region-rows region-columns) obj
    ;; the keyword position overrides the keywords row and column
    (when grid-position
      (setf grid-row    (car  grid-position)
            grid-column (cadr grid-position)))
    ;; the keyword dimensions overrides rows and columns
    (when grid-dimensions
      (setf grid-rows    (car  grid-dimensions)
            grid-columns (cadr grid-dimensions)))
    ;; geometry overrides row, column, rows, columns
    (when grid-geometry
      (setf grid-row     (nth 0 grid-geometry)
            grid-column  (nth 1 grid-geometry)
            grid-rows    (nth 2 grid-geometry)
            grid-columns (nth 3 grid-geometry)))
    (when region-dimensions
      (setf region-rows    (car  region-dimensions)
            region-columns (cadr region-dimensions)))))

(defun visible-grid-rows (grid)
  (with-slots (scrolling-enabled-p region-rows grid-rows) grid
    (if scrolling-enabled-p region-rows grid-rows)))

(defun visible-grid-columns (grid)
  (with-slots (scrolling-enabled-p region-columns grid-columns) grid
    (if scrolling-enabled-p region-columns grid-columns)))

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
               (m grid-rows)
               (n grid-columns)
               (i grid-row)
               (j grid-column)
               (m0 region-start-row)
               (n0 region-start-column)
               (m1 region-rows)
               (n1 region-columns)) grid
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
               (m grid-rows)
               (n grid-columns)
               (i grid-row)
               (j grid-column)
               (m0 region-start-row)
               (n0 region-start-column)
               (m1 region-rows)
               (n1 region-columns)) grid
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
               (n grid-columns)
               (m grid-rows)
               (i grid-row)
               (j grid-column)
               (m0 region-start-row)
               (n0 region-start-column)
               (m1 region-rows)
               (n1 region-columns)) grid
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
               (n grid-columns)
               (m grid-rows)
               (i grid-row)
               (j grid-column)
               (m0 region-start-row)
               (n0 region-start-column)
               (m1 region-rows)
               (n1 region-columns)) grid
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

(defclass layout (widget grid collection)
  ((grid-row-gap
    :initarg       :grid-row-gap
    :initform      0
    :type          integer
    :documentation "Gap between rows in a grid.")
   (grid-column-gap
    :initarg       :grid-column-gap
    :initform      0
    :type          integer
    :documentation "Gap between columns in a grid."))

  (:documentation "A layout is a container widget containing items positioned in a grid. Layouts can be nested."))

(defmethod initialize-instance :after ((obj layout) &key grid-gap)
  (with-slots (grid-row-gap grid-column-gap
               (gr grid-rows) (gc grid-columns) children) obj
    ;; the grid-gap initarg (gap or (row column)) overrides the individual slot initargs.
    (when grid-gap
      (if (listp grid-gap)
          (destructuring-bind (row column) grid-gap
            (setf grid-row-gap    row
                  grid-column-gap column))
          (setf grid-row-gap    grid-gap
                grid-column-gap grid-gap)))
    ;; if the grid height is not given, deduce it from the width and the items length
    (cond ((and (null gc) gr)
           (setf gc (ceiling (length children) gr)))
          ((and (null gr) gc)
           (setf gr (ceiling (length children) gc))))))

(defclass column-layout (layout)
  ()
  (:default-initargs :grid-columns 1)
  (:documentation "A container of widgets organized in one column."))

(defclass row-layout (layout)
  ()
  (:default-initargs :grid-rows 1)
  (:documentation "A container of widgets organized in one column."))

(defmethod current-item ((obj layout))
  "If n is the current item number, return the nth leaf of a layout tree."
  (with-accessors ((n current-item-number)) obj
    (nth n (leaves obj))))

(defmethod select-next-item ((obj layout))
  (with-accessors ((n current-item-number)) obj
    (setf n (mod (1+ n) (length (leaves obj))))))

(defmethod select-previous-item ((obj layout))
  (with-accessors ((n current-item-number)) obj
    (setf n (mod (1- n) (length (leaves obj))))))

(defmethod width ((obj layout))
  "The width of a layout consists of the max widths of the columns and gaps between them."
  (with-slots ((n grid-columns)
               (m grid-rows)
               (cg grid-column-gap)
               children) obj
    (apply #'+ (* (1- n) cg) (column-widths children (list m n)))))

(defmethod height ((obj layout))
  "The height of a layout consists of the max heights of the rows and the gaps between them."
  (with-slots ((n grid-columns)
               (m grid-rows)
               (rg grid-row-gap)
               children) obj
    (apply #'+ (* (1- m) rg) (row-heights children (list m n)))))

(defun nth2d (position dimensions list)
  "Return the position (y x) of list by assuming grid dimensions (m n) and row major order."
  (nth (sub2rmi dimensions position) list))

(defun column-widths (list dimensions)
  "Take a list of objects, return a list of max widths of every column."
  (destructuring-bind (m n) dimensions
    (loop for j from 0 below n collect
      (loop for i from 0 below m maximize
        (let ((item (nth2d (list i j) dimensions list)))
          (if (typep item 'element)
              (external-width item)
              (width item)))))))

(defun row-heights (list dimensions)
  "Take a list of objects, return a list of max heights of every row."
  (destructuring-bind (m n) dimensions
    (loop for i from 0 below m collect
      (loop for j from 0 below n maximize
        (let ((item (nth2d (list i j) dimensions list)))
          (if (typep item 'element)
              (external-height item)
              (height item)))))))

;; used in initialize-instance form to layout form elements, tested in t16j2
(defun calculate-positions (layout)
  "Recursively set the position (y x) of each of the layout's children."
  (with-slots ((m grid-rows) (n grid-columns) (y position-y) (x position-x)
               (rg grid-row-gap) (cg grid-column-gap) children) layout
    (let* ((widths (column-widths children (list m n)))
           (heights (row-heights children (list m n)))
           (xs (cumsum-predecessors widths))
           (ys (cumsum-predecessors heights)))
      (dotimes (el (length children))
        ;; layouts can contain nil, so check for a nil element
        (when (nth el children)
          (let* ((child (nth el children))
                 (i (car (rmi2sub (list m n) el)))
                 (j (cadr (rmi2sub (list m n) el))))
            (setf (position-y child) (+ y (+ (* rg i) (nth i ys)))
                  (position-x child) (+ x (+ (* cg j) (nth j xs))))
            ;; after setting the position, check if the element is a layout object,
            ;; then recursively set the positions of its child elements.
            (when (typep child 'layout)
              (calculate-positions child))))))))

(defun flatten-items (layout)
  "Take a layout object, return a flattened list of elements to be passed to a form.

Nested layouts are spliced in, nils removed and strings/symbols/numbers converted to labels."
    (let (items)
      (labels ((flatten (list)
                 (dolist (i list)
                   (cond ((typep i 'layout)
                          ;; push items of a nested layout recursively
                          (flatten (slot-value i 'children)))
                         ((typep i 'element)
                          (push i items))
                         ((null i)
                          ;; ignore nil layout placeholders
                          nil)
                         ((atom i)
                          ;; strings, numbers, symbols
                          (push (make-instance 'label :title (princ-to-string i)) items))))))
        (flatten (slot-value layout 'children))
        (nreverse items))))

;; (split-size '(2 nil nil 2) 10) => (2 3 3 2)
(defun split-size (hints parent-size)
  "Take a list of size hints of one dimension, return a complete list.

The missing elements are denoted by nil, their size is evenly split."
  (let* ((n (count-if #'null hints))              ; _N_umber of children without a size
         (g (loop for i in hints when i sum i))   ; sum of already _G_iven sizes
         (s (- parent-size g))                    ; _S_pace left for all free children
         (c (truncate s n))                       ; _C_alculated space for every child
         (l (- s (* n c)))                        ; space _L_eft after truncation
         (firstp t))                              ; flag for the first child
    (loop for i in hints
          collect (if i
                      ;; if there is a size hint, take it
                      i
                      (if firstp
                          ;; give the truncated space to the first child
                          (progn (setq firstp nil) (+ c l))
                          c)))))

;; (2 4 3 6) => (0 2 6 9)
(defun cumsum-predecessors (nums)
  "Take a list, return a cumulative sum of previous elements.

For every number, sum the numbers preceding it in the list.

For example, for the list (2 4 3 6) we will get (0 2 6 9).

For example, if (2 4 3 6) is a list of column widths in a table,
then (0 2 6 9) is a list of x positions of each column."
  (loop for i from 0 below (length nums)
        collect (reduce #'+ (subseq nums 0 i))))

(defun collect-width-hints (nodes)
  "Take a list of plists or objects, return a list of their widths.

If the width is nil for a node, it will be calculated by split-size."
  (loop for i in nodes collect
    (if (listp i)
        (getf (cdr i) :width)
        (slot-value i 'initial-dimension-hint))))

(defun collect-height-hints (nodes)
  (loop for i in nodes collect
    (if (listp i)
        (getf (cdr i) :height)
        (slot-value i 'initial-dimension-hint))))

(defgeneric calculate-layout (node)
  (:documentation "Recursively calculate the missing geometry parameters for the node's children."))

(defmethod calculate-layout ((node row-layout))
  (let* ((y (if (position-y node) (position-y node) (position-y (parent node))))
         (x (if (position-x node) (position-x node) (position-x (parent node))))
         ;; we cant use the h/w accessors because they try to calc the h/w of the layout
         ;; from the h/w of its children, which have not been initialized yet.
         (h (if (slot-value node 'height) (slot-value node 'height) (height (parent node))))
         (w (if (slot-value node 'width)  (slot-value node 'width)  (width  (parent node))))
         (children (children node))
         (n (length children))
         (hints (collect-width-hints children))
         (ws (split-size hints w))
         (hs (make-list n :initial-element h))
         (ys (make-list n :initial-element y))
         (xs (mapcar (lambda (i) (+ i x)) (cumsum-predecessors ws))))
    (dotimes (i n)
      (when (listp (nth i children))
        ;; if there is a width and no hint, save the width as the initial hint
        ;; the width and the hint in the plist are then passed to the object
        (when (and (getf (cdr (nth i (children node))) :width)
                   (null (getf (cdr (nth i (children node))) :initial-dimension-hint)))
          (setf (getf (cdr (nth i (children node))) :initial-dimension-hint)
                (getf (cdr (nth i (children node))) :width)))))
    ;; address every child as (nth i children), so we can setf it if its a plist.
    (dotimes (i n)
      ;; first set its newly calculated geometry
      (if (listp (nth i children))
          ;; if the child is a plist for a window
          (setf (cdr (nth i (children node)))
                (set-geometry-plist (cdr (nth i children))
                                    (list (nth i ys)
                                          (nth i xs)
                                          (nth i hs)
                                          (nth i ws))))
          (progn
            ;; set the new geometry to the current node
            (setf (geometry (nth i children))
                  (list (nth i ys)
                        (nth i xs)
                        (nth i hs)
                        (nth i ws)))
            ;; if the node is a layout, recursively recalculate its children.
            (when (and (typep (nth i children) 'layout)
                       (children (nth i children)))
              (calculate-layout (nth i children))))))))

(defmethod calculate-layout ((node column-layout))
  (let* ((y (if (position-y node) (position-y node) (position-y (parent node))))
         (x (if (position-x node) (position-x node) (position-x (parent node))))
         (h (if (slot-value node 'height) (slot-value node 'height) (height (parent node))))
         (w (if (slot-value node 'width)  (slot-value node 'width)  (width  (parent node))))
         (children (children node))
         (n (length children))
         (hints (collect-height-hints children))
         (hs (split-size hints h))
         (ws (make-list n :initial-element w))
         (xs (make-list n :initial-element x))
         (ys (mapcar (lambda (i) (+ i y)) (cumsum-predecessors hs))))
    (dotimes (i n)
      (when (listp (nth i children))
        (when (and (getf (cdr (nth i (children node))) :height)
                   (null (getf (cdr (nth i (children node))) :initial-dimension-hint)))
          (setf (getf (cdr (nth i (children node))) :initial-dimension-hint)
                (getf (cdr (nth i (children node))) :height)))))
    (dotimes (i n)
      (if (listp (nth i children))
          (setf (cdr (nth i (children node)))
                (set-geometry-plist (cdr (nth i children))
                                    (list (nth i ys)
                                          (nth i xs)
                                          (nth i hs)
                                          (nth i ws))))
          (progn
            (setf (geometry (nth i children))
                  (list (nth i ys)
                        (nth i xs)
                        (nth i hs)
                        (nth i ws)))
            (when (and (typep (nth i children) 'layout)
                       (children (nth i children)))
              (calculate-layout (nth i children))))))))

(defun initialize-leaves (node)
  "Walk the layout tree given by node and initialize the leaf objects that are given as plists."
  (when (and (typep node 'layout)
             (children node))
    (setf (children node)
          ;; convert the plist children to clos objects.
          (mapcar (lambda (i)
                    (if (listp i) (apply #'make-instance i) i))
                  (children node)))
    ;; Recursively init the leaves of the sub-layouts.
    (dolist (c (children node))
      (when (typep c 'layout)
        (initialize-leaves c)))))

(defun leaves (root)
  "Walk the layout tree given by the root node, return a list of leaves."
  (let (stack
        leaves)
    (push root stack)
    (loop
      (if stack
          (let ((node (pop stack)))
            (if (typep node 'layout)
                ;; queue means FIFO, so we can enqueue the children
                ;; in the normal (left to right) order.
                (when (children node)
                  (dolist (c (reverse (children node)))
                    (push c stack)))
                ;; every object other than a layout is a leaf, so return it.
                (push node leaves)))
          (return (reverse leaves))))))
