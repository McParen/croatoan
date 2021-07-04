(in-package :de.anvi.croatoan)

(defclass field (element)
  ((width
    :initarg       :width
    :initform      nil
    :type          (or null integer)
    :accessor      width
    :documentation "The width of the field. The default buffer length is equal the width.")

   (insert-mode-p
    :initarg       :insert-mode
    :initform      nil
    :type          boolean
    :accessor      insert-mode-p
    :documentation
    "Printing a new char will insert (t) it before the character under the cursor
    instead of overwriting it (nil, default).")

   (buffer
    :initform      nil
    :type          (or null list)
    :accessor      buffer
    :documentation "List containing the characters in the field.")

   (max-buffer-length
    :initarg       :max-buffer-length
    :initform      nil
    :type          (or null integer)
    :accessor      max-buffer-length
    :documentation
    "Max length of the field buffer. If nil, it will be initialized to field width. 
    Horizontal scrolling is then disabled.")

   (display-pointer
    :initform      0
    :type          (or null integer)
    :accessor      display-pointer
    :documentation
    "Position in the input buffer from which n=width characters are displayed.
    When max-buffer-length is greater than width, display-pointer can be greater than zero.
    Horizontal scrolling is then enabled.")

   (input-pointer
    :initform      0
    :type          integer
    :accessor      input-pointer
    :documentation "The position in the input buffer to which the next character will be written."))

  (:default-initargs :keymap 'field-map)

  (:documentation "A field is an editable part of the screen for user input. Can be part of a form."))

(defmethod initialize-instance :after ((field field) &key)
  (with-slots (max-buffer-length width bindings keymap) field
    ;; If unspecified, the default max-buffer-length should be equal to the visible field width.
    (unless max-buffer-length
      (setf max-buffer-length width))))

;; TODO: make value return numbers instead of strings
;; TODO: make value return nil if it contains an empty buffer
;; coercing an empty string to a list properly returns nil.
;; TODO: every element should have a value slot so we can have one function that returns
(defmethod value ((field field))
  "If the field buffer is empty, return nil, otherwise return the buffer as a string."
  (when (slot-value field 'buffer)
    (coerce (reverse (slot-value field 'buffer)) 'string)))

;; TODO: any value that gets set to a field has to be converted to a string first.
;; TODO: check that a value in not longer than max-buffer-length
(defmethod (setf value) (new-value (field field))
  (setf (slot-value field 'buffer) (reverse (coerce new-value 'list))))


(defun remove-nth (n list)
  "Remove element at nth place from the list, decreasing the length of the list.

Example: (remove-nth 3 '(a b c d e)) => (A B C E)"
  (declare
    (type (integer 0) n)
    (type list list))
  (assert (>= n 0))
  (assert (> (length list) n))
  (if (or (zerop n) (null list))
    (cdr list)
    (cons (car list) (remove-nth (1- n) (cdr list)))))

(defun insert-nth (n element list)
  "Insert element into list at nth place, increasing the length of the list.

Example: (insert-nth 3 'x '(a b c d e)) => (A B C X D E)"
  (declare
    (type (integer 0) n)
    (type list list))
  (assert (>= n 0))
  (assert (>= (length list) n))
  (if (or (zerop n) (null list))
      (cons element list)
      (cons (car list) (insert-nth (1- n) element (cdr list)))))

(defun replace-nth (n element list)
  "Replaces element of list at nth place, not increasing the length of the list.

Example: (replace-nth 3 'x '(a b c d e)) => (A B C X E)"
  (declare
    (type (integer 0) n)
    (type list list))
  (assert (>= n 0))
  (assert (>= (length list) n))
  (if (or (zerop n) (null list))
      (cons element (cdr list))
      (cons (car list) (replace-nth (1- n) element (cdr list)))))


;; this is the only place we set the background style for the field
;; TODO: how to access the default fg and bg of a form,
;; if the field is not part of a form? by having a form slot in the field.
;; TODO: this doesnt really clear the field contents, it just overwrites the field display with the background style
;; TODO: rename to (setf (background field) ...)
(defmethod clear ((field field) &key)
  "Clear the field by overwriting it with the background char.

The default background char is #\space."
  (with-accessors ((pos widget-position) (width width) (selected selectedp) (win window) (style style)) field
    (let* ((bg-style (if selected (getf style :selected-background) (getf style :background)))
           (bg-char  (if (getf bg-style :simple-char) (getf bg-style :simple-char) #\space)))
      (setf (cursor-position win) pos)
      (add win bg-char :style bg-style :n width)
      (setf (cursor-position win) pos))))


(defmethod update-cursor-position ((field field))
  "Update the cursor position of a field."
  (with-accessors ((pos widget-position) (inptr input-pointer) (dptr display-pointer) (win window)) field
    (move win
          ;; TODO: assumes a single-line field.
          (car pos)
          (+ (cadr pos)           ; beginning of the field
             (- inptr dptr) ))    ; position in the field starting with dptr

    ;; TODO: why do we have to refresh here?
    ;; because this is the latest movement when drawing

    ;; TODO: replace refresh with update+noutrefresh so we can do a series of refreshes before and end the refresh here.
    (refresh win) ))

;; TODO: use draw-field with a window argument, and draw for the object with an assocated window.
;; TODO: apply this also to menu functions, where it is the other way around.
;; TODO: rewrite clear-field in terms of draw field. simply draw an empty string.
(defmethod draw ((field field))
  "Clear and redraw the field and its contents and background."
  (with-accessors ((pos widget-position) (width width) (inbuf buffer) (inptr input-pointer) (dptr display-pointer)
                   (selected selectedp) (win window) (title title) (style style)) field
    (let* ((fg-style (if selected (getf style :selected-foreground) (getf style :foreground)))
           (len (length inbuf))
           (val (value field))
           (str (if (< len width)
                    ;; if the buffer is shorter than the field, just display it.
                    val
                    ;; otherwise display a substring starting with dptr.
                    ;; display only max width chars starting from dptr
                    (subseq val dptr (if (< width (- len dptr))
                                         ;; if the remaining substring is longer than width, display just width chars.
                                         (+ dptr width)
                                         ;; if the remaining substring is shorter than width, just display it.
                                         len) ))))
      (clear field)
      (apply #'move win pos)
      (add-string win str :style fg-style)
      (update-cursor-position field))))

;; TODO 190308 add layout accessor so we dont have to use height and width separately
;; rename "layout" to "dimensions" to mirror array-dimensions
;; TODO 190313 add support for height. right now, draw ignores height. the default behavior should be to scroll.

;; 1. horizontal scrolling of single-line fields
;; (setq truncate-lines t) emacs
;; after we added the chars, we have to draw the rest of the field.


(defun move-start-of-line (field event &rest args)
  "Move the cursor to the first char in the field."
  (declare (ignore event))
  (with-accessors ((inptr input-pointer) (dptr display-pointer) (win window)) field
    (setf inptr 0 dptr 0)
    (draw field)))

;; TODO: C-e
(defun move-end-of-line (field event &rest args)
  "Move the cursor to the last char in the field."
  (declare (ignore event))
  (with-accessors ((width width) (inptr input-pointer) (inbuf buffer) (dptr display-pointer) (win window)) field
    (cond ((< (length inbuf) width)
           (setf inptr (length inbuf))
           (setf dptr 0))

          ((= (length inbuf) width)
           (setf inptr (1- (length inbuf)))
           (setf dptr 0))
          
          ((> (length inbuf) width)
           (setf inptr (length inbuf))
           (setf dptr (+ 1 (- (length inbuf) width))) )))
  
  (draw field))


(defmethod move-previous-char ((field field) event &rest args)
  "Move the cursor to the previous char in the field."
  (with-accessors ((inptr input-pointer) (dptr display-pointer) (win window)) field
    (when (> inptr 0)
      (decf inptr))
    ;; when the inptr moves left past the dptr, simultaneously decf the dptr.
    (when (< inptr dptr)
      (decf dptr))
    (draw field)))

(defmethod move-next-char ((field field) event &rest args)
  "Move the cursor to the next char in the field."
  (with-accessors ((width width) (inbuf buffer) (inptr input-pointer) (dptr display-pointer) (mlen max-buffer-length)
                   (win window)) field
    (when (and (< inptr (length inbuf))
               (not (= (1+ inptr) mlen width)))
      (incf inptr))
    ;; when the inptr moves right past the width, simultaneously incf the dptr.
    (when (and (>= inptr (+ dptr width))
               (not (= inptr mlen width)))
      (incf dptr))
    (draw field)))

(defun delete-previous-char (field event &rest args)
  "Delete the previous char in the field, moving the cursor to the left."
  (with-accessors ((inbuf buffer) (inptr input-pointer) (dptr display-pointer) (win window)) field
    (when (> inptr 0)
      (decf inptr)
      (when (> dptr 0)
        (decf dptr))
      (setf inbuf (remove-nth (- (length inbuf) 1 inptr) inbuf)))
    ;; we dont have to redraw the complete form, just the changed field.
    (draw field)))

(defun delete-next-char (field event &rest args)
  "Delete the next char (char under the cursor) in the field, not moving the cursor."
  (with-accessors ((inbuf buffer) (inptr input-pointer) (dptr display-pointer) (win window)) field
    ;; we can only delete to the right if the inptr is not at the end of the inbuf.
    (when (> (length inbuf) inptr)
      (when (> dptr 0)
        ;; when a part of the string is hidden on the left side, shift it to the right.
        (decf dptr))
      (setf inbuf (remove-nth (- (length inbuf) (1+ inptr)) inbuf)))
    (draw field)))

;; TODO: rename to add-char, make it a method
(defun field-add-char (field char &rest args)
  "Add char to the current cursor position in the field, then move the cursor forward.

The buffer can be longer than the displayed field width, horizontal scrolling is enabled."
  (if (and (characterp char) (graphic-char-p char))
      (progn
        (with-accessors ((width width) (inbuf buffer) (mlen max-buffer-length) (inptr input-pointer)
                         (dptr display-pointer) (win window)) field
          (let ((len (length inbuf)))
            (if (insert-mode-p field)

                ;; insert mode
                (progn
                  ;; only add new chars until we've reached the max-buffer-length
                  (unless (>= len mlen)
                    ;; if we're at the end of the inbuf
                    (if (= inptr len)
                        ;; just add another char to the inbuf
                        (setf inbuf (cons char inbuf))
                        ;; if we're in the middle of the buffer, either insert or replace
                        (setf inbuf (insert-nth (- len inptr) char inbuf)) )
                    ;; we need special cases when mlen is exactly equal to width.
                    (if (= mlen width)
                        ;; advance the cursor if it is not already at the end
                        ;; if scrolling is disabled, do not move past the last char in the field.
                        (unless (>= inptr (- mlen 1))
                          (incf inptr))
                        (unless (> inptr (- mlen 1))
                          (incf inptr))))
                  ;; after updating the fill-pointer, update the display-pointer
                  (if (< inptr dptr) (decf dptr))
                  (if (> inptr (+ dptr (1- width))) (incf dptr)))

                ;; default overwrite mode
                (progn
                  ;; only add new chars until we've reached the max-buffer-length then only overwrite.
                  (if (>= len mlen)
                      (if (< inptr mlen)
                          ;; even when the inbuf is full, when inptr is not at the end, overwrite.
                          (setf inbuf (replace-nth (- len (1+ inptr)) char inbuf))
                          nil)
                      ;; if we're at the end of the inbuf
                      (if (= inptr len)
                          ;; just add another char to the inbuf
                          (setf inbuf (cons char inbuf))
                          ;; if we're in the middle of the buffer, either insert or replace
                          (setf inbuf (replace-nth (- len (1+ inptr)) char inbuf))))
                  ;; we need special cases when mlen is exactly equal to width.
                  (if (= mlen width)
                      ;; advance the cursor if it is not already at the end
                      ;; if scrolling is disabled, do not move past the last char in the field.
                      (unless (>= inptr (- mlen 1))
                        (incf inptr))
                      (unless (> inptr (- mlen 1))
                        (incf inptr)))
                  ;; after updating the fill-pointer, update the display-pointer
                  (when (< inptr dptr) (decf dptr))
                  (if (<= mlen width)
                      ;; if scrolling is disabled, do not move past the last char in the field.
                      (when (> inptr (+ dptr width))
                        (incf dptr))
                      
                      (when (> inptr (+ dptr (1- width)))
                        (incf dptr))))))) ;accessors
        (draw field)) ;progn
      
      ;; if the char isnt graphic, do nothing.
      ;; TODO: this doesnt work with acs chars, which are keywords.
      nil))

;; TODO: remove form case
(defun debug-print-field-buffer (object event &rest args)
  (declare (ignore event))
  (typecase object
    (field
     (with-accessors ((inbuf buffer) (inptr input-pointer) (dptr display-pointer) (win window)) object
       (when (> (length inbuf) 0)
         (clear win)
         (format win "~A ~%" (value object))

         ;; this is clear field, move this to a separate function to be used to reset fields.
         (setf inbuf nil inptr 0 dptr 0))))
    ;; when we want to debug the whole form.
    (form
     ;; TODO: this doesnt work when we have elements that are not fields?
     (debug-print-field-buffer (current-element object) event)))
  (draw object))
  
;; TODO 191110: if there is an initial value of the field, do not reset it to nil, but to the initial value
(defun reset-field (field event &rest args)
  "Clear the field and reset its internal buffers and pointers."
  (with-accessors ((inbuf buffer) (inptr input-pointer) (dptr display-pointer) (win window)) field
    ;; TODO 191216: only reset if inbuf is not nil, see girc process user input.
    (clear field)
    (setf inbuf nil inptr 0 dptr 0)))

(define-keymap field-map
  ;; C-a = ^A = #\soh = 1 = start of heading
  ;; exit the edit loop, return t
  ;; we need this in the field keymap in case a field is used alone outside of a form.
  (#\soh 'accept)
  ;; C-x = cancel = CAN = #\can
  ;; exit the edit loop, return nil
  (#\can 'cancel)

  ;; TODO: use C-a to get to the start of the field
  ;;(#\l 'move-start-of-line)
  ;;(#\k 'move-end-of-line)
  
  ;; C-r = reset = DC2 = #\dc2
  ;; reset the field
  (#\dc2 'reset-field)

  (:left 'move-previous-char)
  (:right 'move-next-char)
  (:backspace 'delete-previous-char)
  (:dc 'delete-next-char)
  (:ic  (lambda (field event &rest args)
          (toggle-insert-mode field)))
  (t 'field-add-char))
