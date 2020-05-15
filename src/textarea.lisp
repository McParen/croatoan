(in-package :de.anvi.croatoan)

(defclass textarea (element)
  ((width
    :initarg       :width
    :initform      nil
    :type          (or null integer)
    :accessor      width
    :documentation
    "The width (number of columns) of the textarea. The area can not be scrolled horizontally.
    Lines exceeding the width are soft-wrapped to the next line.
    (This corresponds to the emacs behavior when truncate-lines is set to nil.)")

   (height
    :initarg       :height
    :initform      nil
    :type          (or null integer)
    :accessor      height
    :documentation
    "The height (number of visible lines) of the textarea. The area can be scrolled vertically.")
   
   (bindings
    :initarg       :bindings
    :initform      nil
    :type          (or null cons)
    :accessor      bindings
    :documentation
    "Alist of events (characters, keywords or integers) as keys and handler functions as values. 
    Used by the run-event-loop function.")

   (keymap
    :initarg       :keymap
    :initform      'textarea-map
    :type          (or null symbol keyword keymap)
    :accessor      keymap
    :documentation "Keymap containing the key bindings to be used by run-event-loop instead of the object's own bindings.")

   ;; we use one large continous buffer instead of a list of lists/strings.
   ;; reason: easier to implement, we also can use emacs concepts of point and mark more easily
   ;; inefficient, but sufficient for our small textarea sizes.
   (buffer
    :initarg       :buffer ; only for the development and debugging, remove later.
    :initform      nil
    :type          (or null list)
    :accessor      buffer
    :documentation "List containing the characters of the textarea.")

   (display-pointer
    :initform      0
    :type          (or null integer)
    :accessor      display-pointer
    :documentation
    "When the area contains more lines than can be shown on screen because they exceed the 
    given height, this points to the first line that is displayed.")

   (input-pointer
    :initform      0
    :type          (or null integer)
    :accessor      input-pointer
    :documentation
    "The row-major index in the input buffer to which the next character will be written.
    Can not be greater than the row-major length of the current buffer content.")

   (cursor-position-y
    :initform      0
    :type          (or null integer)
    :accessor      cursor-position-y
    :documentation "Y position (row) in the textarea where the next character will be added.")

   (cursor-position-x
    :initform      0
    :type          (or null integer)
    :accessor      cursor-position-x
    :documentation "X position (column) in the textarea where the next character will be added."))

  (:documentation
   "A textarea is a multiline field for display and editing of texts including newlines.
   For now, control characters other than newline are not interpreted."))

(defmethod value ((area textarea))
  "If the buffer is empty, return nil, otherwise return the buffer as a string."
  (when (slot-value area 'buffer)
    (coerce (slot-value area 'buffer) 'string)))

(defmethod (setf value) (new-value (area textarea))
  (setf (slot-value area 'buffer) (coerce new-value 'list)))

(defmethod clear ((area textarea) &key)
  "Clear the textarea by overwriting it with the background char.

The default background char is #\space."
  (with-accessors ((pos element-position) (width width) (height height) (win window)) area
    (setf (cursor-position win) pos)
    ;; return to the start of the area after the clearing
    (save-excursion win
      (loop for i from 0 to (1- height) do
           (loop for j from 0 to (1- width) do
                (apply #'move win (mapcar #'+ pos (list i j)))
                (add win #\space))))))

(defmethod update-cursor-position ((area textarea))
  (with-accessors ((pos element-position) (win window) (dptr display-pointer)
                   (y cursor-position-y) (x cursor-position-x)) area
    (apply #'move win (mapcar #'+ pos (list (- y dptr) x)))
    (refresh win)))

(defmethod draw ((area textarea))
  (with-accessors ((pos element-position) (width width) (height height)
                   (inbuf buffer) (dptr display-pointer) (win window)) area
    (clear area)
    (let ((y 0)
          (x 0))
      ;; start at (0 0)
      (goto win pos (list y x))
      (loop for i from 0 to (1- (length inbuf)) do
        (if (char= (nth i inbuf) #\newline)
            (progn
              ;; when we encounter the newline char, go to the beginning of the next line.
              (setq y (1+ y) x 0))
            (progn
              ;; all other (graphic) chars
              (when (>= y dptr)
                (add-char win (nth i inbuf)))
              (if (= x (1- width))
                  ;; if we're at the last column, set the next x to 0 and move downward.
                  (setq y (1+ y) x 0)
                  ;; otherwise just move to the right
                  (setq x (1+ x)))))
        (goto win pos (list (- y dptr) x))
        (when (>= (- y dptr) height) (return)) ))
    (update-cursor-position area)))

;; (= (1- inptr) width)
(defun multiplep (a b)
  (zerop (mod a b)))

(defun previous-char= (area char)
  "Return t if char is the previous char in the buffer of textarea."
  (with-accessors ((inbuf buffer) (inptr input-pointer)) area
    (char= (nth (1- inptr) inbuf) char)))

(defmethod move-previous-char ((area textarea) event &rest args)
  "Move the cursor to the previous char in the textarea."
  (with-accessors ((width width) (inbuf buffer) (inptr input-pointer) (dptr display-pointer)
                   (win window) (y cursor-position-y) (x cursor-position-x)) area
    (when (> inptr 0)
      (cond
        ;; beginning of line, after a newline, half full previous line
        ((and (previous-char= area #\newline)
              (= x 0))
         (decf inptr)
         (setf y (1- y)
               ;; how to decide where to put x when we go to the previous row?
               ;; we need to know how many chars are on the previous line
               x (calc-pos area))
         (when (< y dptr)
           (decf dptr)))
        ;; beginning of line, full previous line
        ;; a line is full when the distance to the previous newline is a multiple of the width
        ((and (multiplep (calc-pos area) width)
              (= x 0))
         (decf inptr)
         (setf y (1- y) x (1- width))
         (when (< y dptr)
           (decf dptr)))
         ;; when inptr is on a normal char, just move the cursor to the left        
        (t
         (decf inptr)
         (decf x)))))
  (draw area))

(defun calc-pos (area)
  "Take an input pointer (integer) and a textarea buffer (list of characters),

return the number of chars between the pointer and the first newline before the pointer."
  (with-accessors ((width width) (inbuf buffer) (inptr input-pointer) (x cursor-position-x) (win window)) area
    (let* (;; position of the newline from inptr, or inptr if there is no newline
           (pos1 (loop for i from (1- inptr) downto 0 if (char= (nth i inbuf) #\newline) return i))
           ;; if a newline is found
           (pos2 (if pos1
                     ;; return the distance of point and the newline position
                     (- (1- inptr) pos1)
                     ;; if there is no newline, return the point
                     inptr)))
      ;; if the distance to the previous newline is > width, move x to width
      (if (> pos2 width)
          (progn
            (mod pos2 width))
          (progn
            (mod pos2 width))))))

(defmethod move-next-char ((area textarea) event &rest args)
  "Move the cursor to the next char in the textarea."
  (with-accessors ((width width) (height height) (inbuf buffer) (inptr input-pointer) (dptr display-pointer)
                   (win window) (y cursor-position-y) (x cursor-position-x)) area
    (when (< inptr (length inbuf))
      (if (or (char= (nth inptr inbuf) #\newline)
              (= x (1- width)))
          ;; when inptr is on a newline or on the last char on a line
          (progn
            (incf inptr)
            (setf y (1+ y) x 0)
            (when (> (- y dptr) (1- height)) (incf dptr)))
          ;; when inptr is on a normal char, just move the cursor to the right
          (progn
            (incf inptr)
            (incf x)))))
  (draw area))

(defun textarea-add-char (area char &rest args)
  (with-accessors ((inbuf buffer) (inptr input-pointer) (width width) (height height) (dptr display-pointer)
                   (y cursor-position-y) (x cursor-position-x)) area
    (when (characterp char)
      (if (or (char= char #\newline)
              (= x (1- width)))
          (progn
            (setf y (1+ y) x 0)
            (when (> (- y dptr) (1- height)) (incf dptr)))
          (incf x))
      (setf inbuf (replace-nth inptr char inbuf))
      (incf inptr)))
  (draw area))

(define-keymap textarea-map
  (:left 'move-previous-char)
  (:right 'move-next-char)
  (#\soh 'accept)
  (#\can 'cancel)
  (t 'textarea-add-char))
