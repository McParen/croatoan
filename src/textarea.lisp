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

   (insert-mode-p
    :initarg       :insert-mode
    :initform      t
    :type          boolean
    :accessor      insert-mode-p
    :documentation
    "Printing a new char will insert (t, default) it before the character under the cursor
    instead of overwriting it (nil).")

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
    :documentation
    "Y position (row) in the textarea window where the next character will be added.")

   (cursor-position-x
    :initform      0
    :type          (or null integer)
    :accessor      cursor-position-x
    :documentation
    "X position (column) in the textarea window where the next character will be added."))

  (:default-initargs :keymap 'textarea-map)

  (:documentation
   "A textarea is a multiline field for display and editing of texts including newlines.
   For now, control characters other than newline are not interpreted.
   The main difference to a window is the ability to scroll the buffer."))

(defmethod initialize-instance :after ((area textarea) &key dimensions)
  (with-slots (width height) area
    ;; the keyword dimensions overrides width and height
    (when dimensions
      (setf height (car dimensions)
            width (cadr dimensions)))))

(defmethod value ((area textarea))
  "If the buffer is empty, return nil, otherwise return the buffer as a string."
  (when (slot-value area 'buffer)
    (coerce (slot-value area 'buffer) 'string)))

(defmethod (setf value) (new-value (area textarea))
  "Set the buffer of the area to the string new-value."
  (setf (slot-value area 'buffer) (coerce new-value 'list)))

(defmethod clear ((obj textarea) &key)
  "Clear the area by overwriting the underlying window area with the background char.

The default background char is #\space.

The char can be set by setting the :background and :selected-background style.

If the underlying window has a background char, that will be used to
clear the window instead of #\space."
  (with-accessors ((pos widget-position) (width width) (height height)
                   (selected selectedp) (win window) (style style)) obj
    (let* ((bg-style (if selected (getf style :selected-background) (getf style :background)))
           (bg-char  (if (getf bg-style :simple-char) (getf bg-style :simple-char) #\space)))
      (goto win pos)
      ;; return to the start of the area after the clearing
      (save-excursion win
        (dogrid ((i 0 height)
                 (j 0 width))
          (goto win pos (list i j))
          ;; clearing with a simple space inherits the attributes and colors
          ;; from the background char of the window.
          (add win bg-char :style bg-style))))))

(defmethod reset ((area textarea))
  "Clear the textarea and reset its internal buffers and pointers."
  (with-accessors ((inbuf buffer) (inptr input-pointer) (dptr display-pointer) (y cursor-position-y) (x cursor-position-x)) area
    (clear area)
    (setf inbuf nil inptr 0 dptr 0 y 0 x 0)))

(defmethod update-cursor-position ((area textarea))
  (with-accessors ((pos widget-position) (win window) (dptr display-pointer)
                   (y cursor-position-y) (x cursor-position-x)) area
    (apply #'move win (mapcar #'+ pos (list (- y dptr) x)))
    (refresh win)))

(defmethod draw ((area textarea))
  (with-accessors ((pos widget-position) (w width) (h height) (inbuf buffer)
                   (selected selectedp) (dptr display-pointer) (win window) (style style)) area
    (clear area)
    (let ((fg-style (if selected (getf style :selected-foreground) (getf style :foreground)))
          (y 0)
          (x 0))
      ;; start at (0 0)
      (goto win pos (list y x))
      (dotimes (i (length inbuf))
        (let ((ch (nth i inbuf)))
          (if (char= ch #\newline)
              (progn
                ;; when we encounter the newline char, go to the beginning of the next line.
                (setq y (1+ y)
                      x 0))
              (progn
                ;; all other (graphic) chars
                (when (>= y dptr)
                  (add-char win ch :style fg-style))
                (if (= x (1- w))
                    ;; if we're at the last column, set the next x to 0 and move downward.
                    (setq y (1+ y) x 0)
                    ;; otherwise just move to the right
                    (setq x (1+ x)))))
          (goto win pos (list (- y dptr) x))
          ;; when h lines have been displayed, exit the loop.
          (when (>= (- y dptr) h)
            (return))))
      (update-cursor-position area))))

;; (= (1- inptr) width)
(defun multiplep (a b)
  (zerop (mod a b)))

(defun previous-char= (area char)
  "Return t if char is the previous char in the buffer of textarea."
  (with-accessors ((inbuf buffer) (inptr input-pointer)) area
    (char= (nth (1- inptr) inbuf) char)))

(defmethod delete-next-char ((area textarea))
  (with-accessors ((inbuf buffer) (inptr input-pointer) (dptr display-pointer)
                   (y cursor-position-y) (x cursor-position-x) (w width)) area
    (when (> (length inbuf) inptr)
      (setf inbuf (remove-nth inptr inbuf))
      (draw area))))

(defmethod delete-previous-char ((area textarea))
  (with-accessors ((inbuf buffer) (inptr input-pointer) (dptr display-pointer)
                   (y cursor-position-y) (x cursor-position-x) (w width)) area
    (when (> inptr 0)
      (cond
        ((and (previous-char= area #\newline)
              (= x 0))
         (decf inptr)
         (setf y (1- y)
               x (input-pointer-x area))
         (when (< y dptr)
           (decf dptr)))
        ((and (= x 0)
              (multiplep (input-pointer-x area) w))
         (decf inptr)
         (setf y (1- y)
               x (1- w))
         (when (< y dptr)
           (decf dptr)))
        (t
         (decf inptr)
         (decf x)))
      (setf inbuf (remove-nth inptr inbuf))
      (draw area))))

(defmethod move-previous-char ((area textarea))
  "Move the cursor to the previous char in the textarea."
  (with-accessors ((width width) (inbuf buffer) (inptr input-pointer) (dptr display-pointer)
                   (win window) (y cursor-position-y) (x cursor-position-x)) area
    (when (> inptr 0)
      (cond
        ;; beginning of line after a newline, half full previous line
        ((and (previous-char= area #\newline)
              (= x 0))
         (decf inptr)
         (setf y (1- y)
               ;; how to decide where to put x when we go to the previous row?
               ;; we need to know how many chars are on the previous line
               x (input-pointer-x area))
         (when (< y dptr)
           (decf dptr)))
        ;; beginning of line, full previous line
        ;; a line is full when the distance to the previous newline is a multiple of the width
        ((and (= x 0)
              (multiplep (input-pointer-x area) width))
         (decf inptr)
         (setf y (1- y)
               x (1- width)) ;; put the cursor on the last column of the previous line
         (when (< y dptr)
           (decf dptr)))
         ;; when inptr is on a normal char, just move the cursor to the left
        (t
         (decf inptr)
         (decf x)))))
  (draw area))

(defun input-pointer-x (area)
  "Take an input pointer (integer) and a textarea buffer (list of characters),

return the number of columns between the pointer and the first newline before the pointer."
  (with-accessors ((width width) (inbuf buffer) (inptr input-pointer)) area
    (let* (;; position of the newline left from inptr, or nil if there is no previous newline
           (pos1 (loop for i from (1- inptr) downto 0
                       if (char= (nth i inbuf) #\newline) return i))
           ;; if a newline is found
           (pos2 (if pos1
                     ;; return the distance of the pointer and the newline position
                     (- (1- inptr) pos1)
                     ;; if there is no newline, return the pointer
                     inptr)))
      ;; if the distance to the previous newline is > width
      (if (> pos2 width)
          ;; return the x position of the last char after the line has been wrapped to width
          (mod pos2 width)
          (mod pos2 width)))))

(defmethod move-next-char ((area textarea))
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

(defun textarea-add-char (area event)
  (with-accessors ((char event-key)) event
    (with-accessors ((inbuf buffer) (inptr input-pointer) (width width) (height height)
                     (dptr display-pointer) (y cursor-position-y) (x cursor-position-x)) area
      (when (characterp char)
        (cond ((or (char= char #\newline)
                   (= x (1- width)))
               (setf y (1+ y)
                     x 0)
               (when (> (- y dptr) (1- height))
                 (incf dptr)))
              (t
               (incf x)))
        (if (insert-mode-p area)
            (setf inbuf (insert-nth inptr char inbuf))
            (setf inbuf (replace-nth inptr char inbuf)))
        (incf inptr)))
    (draw area)))

(define-keymap textarea-map
  (:left 'move-previous-char)
  (:right 'move-next-char)
  (:ic  (lambda (area)
          (toggle-insert-mode area)))

  ;; C-r = reset = DC2 = #\dc2
  ;; clear and reset the textarea
  (#\dc2 'reset)

  (:backspace 'delete-previous-char)
  (:dc 'delete-next-char)

  (#\soh 'accept) ; C-a
  (#\can 'cancel) ; C-x
  (t 'textarea-add-char))
