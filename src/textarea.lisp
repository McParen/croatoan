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

(defmethod value ((area textarea))
  "If the buffer is empty, return nil, otherwise return the buffer as a string."
  (when (slot-value area 'buffer)
    (coerce (slot-value area 'buffer) 'string)))

(defmethod (setf value) (new-value (area textarea))
  "Set the buffer of the area to the string new-value."
  (setf (slot-value area 'buffer) (coerce new-value 'list)))

(defmethod reset ((area textarea))
  "Clear the textarea and reset its internal buffers and pointers."
  (with-accessors ((inbuf buffer) (inptr input-pointer) (dptr display-pointer) (y cursor-position-y) (x cursor-position-x)) area
    (clear area)
    (setf inbuf nil inptr 0 dptr 0 y 0 x 0)))

(defmethod update-cursor-position ((area textarea))
  (with-accessors ((pos content-position) (win window) (dptr display-pointer)
                   (y cursor-position-y) (x cursor-position-x)) area
    (goto win pos (list (- y dptr) x))
    (refresh win)))

(defmethod draw ((area textarea))
  (with-accessors ((pos content-position) (w width) (h height) (inbuf buffer)
                   (selectedp selectedp) (dptr display-pointer) (win window)
                   (style style) (borderp borderp)) area
    (let ((fg-style (if selectedp (getf style :selected-foreground) (getf style :foreground)))
          (border-style (if selectedp (getf style :selected-border) (getf style :border)))
          (y 0)
          (x 0))
      (clear area)

      (when borderp
        (draw-rectangle win (position-y area) (position-x area) (external-height area) (external-width area) :style border-style))

      ;; start at (0 0)
      (goto win pos (list y x))
      (dotimes (i (length inbuf))
        (let ((ch (nth i inbuf)))
          (if (char= ch #\newline)
              (progn
                ;; when we encounter the newline char, go to the beginning of the next line,
                ;; but only if the previous line isn't full.
                (unless (and (= x 0)
                             (> i 0)
                             (char/= (nth (1- i) inbuf) #\newline))
                  (setq y (1+ y)
                        x 0)))
              (progn
                ;; all other (graphic) chars
                (when (>= y dptr)
                  (add-wide-char win ch :style fg-style))
                ;; if we're at the last column
                (if (= x (1- w))
                    ;; set x to 0 and move downward.
                    (setq y (1+ y) x 0)
                    ;; otherwise just move to the right
                    (setq x (1+ x)))))
          (goto win pos (list (- y dptr) x))
          ;; when h lines have been displayed, exit the loop.
          (when (>= (- y dptr) h)
            (return))))
      (update-cursor-position area))))

(defun previous-char= (area char)
  "Return t if char is the previous char in the buffer of textarea."
  (with-accessors ((inbuf buffer) (inptr input-pointer)) area
    (char= (nth (1- inptr) inbuf) char)))

(defun multiplep (a b)
  (and (> a 0)
       (zerop (mod a b))))

(defun debug-textarea (yy xx area)
  (with-accessors ((inbuf buffer) (inptr input-pointer) (dptr display-pointer)
                   (y cursor-position-y) (x cursor-position-x) (w width) (win window)) area
      (move win yy xx)
      (format win "                                                         ")
      (move win yy xx)
      (format win "~2A ~2A ~2A ~2A ~2A" y x inptr (input-pointer-x area) (previous-line-length inbuf inptr))))

(defun previous-line-length (buf ptr)
  (let* (;; if the last char is a \n, start counting at the second last
         (start (if (and buf
                         (> (length buf) 1)
                         ;;(char= (car (last buf)) #\newline))
                         (if (> ptr 0)
                             (char= (car (last (subseq buf 0 ptr))) #\newline)
                             (char= (car (last buf)) #\newline)))
                    2
                    1))
         (pos1 (loop for i from (- ptr start) downto 0
                     if (char= (nth i buf) #\newline) return i))
         (pos2 (if pos1
                   (- (- ptr start) pos1)
                   (- ptr (- start 1)))))
    ;;(format t "~A ~A ~A" start pos1 pos2)
    (if (minusp pos2)
        0
        pos2)))

(defun distance-to-newline (area)
  (with-accessors ((inbuf buffer) (inptr input-pointer)) area
    (let* (;; position of the newline left from inptr, or nil if there is no previous newline
           (pos1 (loop for i from (1- inptr) downto 0
                       if (char= (nth i inbuf) #\newline) return i))
           ;; if a newline is found
           (pos2 (if pos1
                     ;; return the distance of the pointer and the newline position
                     (- (1- inptr) pos1)
                     ;; if there is no newline, return the pointer
                     inptr)))
      pos2)))

(defun input-pointer-x (area)
  "Take an input pointer (integer) and a textarea buffer (list of characters),

return the number of columns between the pointer and the first newline before the pointer."
  (with-accessors ((width width)) area
    (mod (distance-to-newline area) width)))

(defmethod delete-previous-char ((area textarea))
  (with-accessors ((inbuf buffer) (inptr input-pointer) (dptr display-pointer)
                   (y cursor-position-y) (x cursor-position-x) (width width)) area
    (when (> inptr 0)
      (cond
        ;; beginning of a line after a newline char with a full previous line
        ((and (= x 0)
              (previous-char= area #\newline)
              (multiplep (previous-line-length inbuf inptr) width))
         ;; delete the newline char without moving the cursor
         (decf inptr))

        ;; beginning of line after a newline, half full previous line
        ((and (= x 0)
              (previous-char= area #\newline))
         (decf inptr)
         (setf y (1- y)
               x (input-pointer-x area))
         (when (< y dptr)
           (decf dptr)))

        ;; beginning of line, full previous line, but no newline
        ((and (= x 0)
              (multiplep (previous-line-length inbuf inptr) width))
         (decf inptr)
         (setf y (1- y)
               x (1- width))
         (when (< y dptr)
           (decf dptr)))

        (t
         (decf inptr)
         (decf x)))
      (setf inbuf (remove-nth inptr inbuf)))
    (draw area)))

(defmethod delete-next-char ((area textarea))
  (with-accessors ((inbuf buffer) (inptr input-pointer) (dptr display-pointer)) area
    (when (> (length inbuf) inptr)
      (setf inbuf (remove-nth inptr inbuf))
      (draw area))))

(defmethod move-previous-char ((area textarea))
  "Move the cursor to the previous char in the textarea."
  (with-accessors ((width width) (inbuf buffer) (inptr input-pointer) (dptr display-pointer)
                   (y cursor-position-y) (x cursor-position-x)) area
    (when (> inptr 0)
      (cond
        ;; beginning of a line after a newline char with a full previous line
        ((and (= x 0)
              (previous-char= area #\newline)
              (multiplep (previous-line-length inbuf inptr) width))
         ;; jump over the newline char, do not make an extra step for it
         (decf inptr 2)
         (setf y (1- y)
               x (1- width))
         (when (< y dptr)
           (decf dptr)))

        ;; beginning of line after a newline, half full previous line
        ((and (= x 0)
              (previous-char= area #\newline))
         (decf inptr)
         (setf y (1- y)
               ;; how to decide where to put x when we go to the previous row?
               ;; we need to know how many chars are on the previous line
               x (input-pointer-x area))
         (when (< y dptr)
           (decf dptr)))

        ;; beginning of line, full previous line, but no newline
        ;; a line is full when the distance to the previous newline is a multiple of the width
        ((and (= x 0)
              (multiplep (previous-line-length inbuf inptr) width))
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

(defmethod move-next-char ((area textarea))
  "Move the cursor to the next char in the textarea."
  (with-accessors ((width width) (height height) (inbuf buffer) (inptr input-pointer) (dptr display-pointer)
                   (y cursor-position-y) (x cursor-position-x)) area
    (when (< inptr (length inbuf))
      (cond
        ;; newline at the beginning of a line
        ((and (= x 0)
              (char= (nth inptr inbuf) #\newline))
         (incf inptr)
         (setf y (1+ y)
               x 0)
         (when (> (- y dptr) (1- height))
           (incf dptr)))

        ;; newline in the middle of a line
        ((char= (nth inptr inbuf) #\newline)
         (incf inptr)
         (setf y (1+ y)
               x 0)
         (when (> (- y dptr) (1- height))
           (incf dptr)))

        ;; last column in a line
        ((= x (1- width))
         (if (and (nth (1+ inptr) inbuf)
                  (char= (nth (1+ inptr) inbuf) #\newline))
             ;; if a newline follows the line wrap, jump over the newline
             (incf inptr 2)
             (incf inptr 1))
         (setf y (1+ y)
               x 0)
         (when (> (- y dptr) (1- height))
           (incf dptr)))

        (t
         (incf inptr)
         (incf x)))))
  (draw area))

(defun textarea-add-char (area event)
  "Add a character to the current cursor position in textarea, then move the cursor forward.

Currently only graphic characters and newline are supported."
  (with-accessors ((char event-key)) event
    (with-accessors ((inbuf buffer) (inptr input-pointer) (width width) (height height)
                     (dptr display-pointer) (y cursor-position-y) (x cursor-position-x)) area
      (when (and (characterp char)
                 (or (graphic-char-p char)
                     (char= char #\newline)))
        (cond ((or (char= char #\newline)
                   (= x (1- width)))
               (unless (and (= x 0)
                            (> inptr 0)
                            (char/= (nth (1- inptr) inbuf) #\newline))
                 (setq y (1+ y)
                       x 0)
                 (when (> (- y dptr) (1- height))
                   (incf dptr))))
              (t
               (incf x)))
        (if (insert-mode-p area)
            (setf inbuf (insert-nth inptr char inbuf))
            (setf inbuf (replace-nth inptr char inbuf)))
        (incf inptr))))
  (draw area))

(define-keymap textarea-map
  (:key-arrow-left  'move-previous-char)
  (:key-arrow-right 'move-next-char)
  (:key-insert-char  (lambda (area)
                       (toggle-insert-mode area)))

  ;; C-r = reset = DC2 = #\dc2
  ;; clear and reset the textarea
  (#\dc2 'reset)

  (:key-backspace   'delete-previous-char)
  (:key-delete-char 'delete-next-char)

  (#\soh 'accept) ; C-a
  (#\can 'cancel) ; C-x
  (t 'textarea-add-char))
