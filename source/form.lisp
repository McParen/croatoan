(in-package :de.anvi.croatoan)

;; form
;; curses extension for programming forms
;; https://invisible-island.net/ncurses/man/form.3x.html

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
(defmethod clear ((field field) &key)
  "Clear the field by overwriting it with the background char.

The default background char is #\space."
  (with-accessors ((pos .position) (width .width) (style .style) (selected .selected) (win .window)) field
    (let* ((bg  (getf style :background))
           (sbg (getf style :selected-background))
           (bgchar (if selected
                       (if sbg sbg #\space)
                       (if bg bg #\space))))
      (setf (.cursor-position win) pos)
      (add win bgchar :n width)
      (setf (.cursor-position win) pos))))

(defgeneric update-cursor-position (object)
  (:documentation "Update the cursor position of the element of a form.")
  (:method (object)
    "The default method puts the cursor at the start position of the element."
    (setf (.cursor-position (.window object)) (.position object))))

(defmethod update-cursor-position ((field field))
  "Update the cursor position of a field."
  (with-accessors ((pos .position) (inptr .fill-pointer) (dptr .display-pointer) (win .window)) field
    (move win
          ;; TODO: assumes a single-line field.
          (car pos)
          (+ (cadr pos)           ; beginning of the field
             (- inptr dptr) ))))  ; position in the field starting with dptr

(defmethod update-cursor-position ((form form))
  "Move the cursor to the correct position in current element of the form."
  (update-cursor-position (.current-element form)))

(defgeneric draw (object)
  (:documentation "Draw objects (form, field, menu) to their associated window."))

(defmethod draw ((button button))
  (with-accessors ((pos .position) (name .name) (win .window) (selected .selected) (style .style)) button
    (move win (car pos) (cadr pos))
    (let ((fg  (getf style :foreground))
          (sfg (getf style :selected-foreground)))
      (add-string win
                  (format nil "<~A>" name)
                  :attributes (if selected (if sfg (.attributes sfg) nil) (if fg (.attributes fg) nil))
                  :color-pair (if selected (if sfg (.color-pair sfg) nil) (if fg (.color-pair fg) nil))))))

(defmethod draw ((field field))
  "Clear and redraw the field and its contents and background."
  (with-accessors ((pos .position) (width .width) (inbuf .buffer) (inptr .fill-pointer) (dptr .display-pointer)
                   (style .style) (selected .selected) (win .window)) field
    (let* ((fg  (getf style :foreground))
           (sfg (getf style :selected-foreground))
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
      (add-string win str
                  ;; TODO: find a more elegant way to highlight the selected field.
                  :attributes (if selected (if sfg (.attributes sfg) nil) (if fg (.attributes fg) nil))
                  :color-pair (if selected (if sfg (.color-pair sfg) nil) (if fg (.color-pair fg) nil)))
      (update-cursor-position field))))

(defmethod draw ((form form))
  "Draw the form by drawing the elements, then moving the cursor to the current element."
  (with-accessors ((elements .elements) (window .window)) form
    (loop for element in elements do
      (draw element))
    ;; after drawing the elements, reposition the cursor to the current element
    (update-cursor-position form)
    (refresh window)))

;; previous-element and next-element are the only two elements where the current-element-number is changed.
;; here also current-element and selected has to be set.
(defun select-previous-element (form event)
  "Select the previous element in a form's element list."
  ;;(declare (special form))
  (with-accessors ((elements .elements) (current-element-number .current-element-number) (current-element .current-element) (win .window)) form
    (setf (.selected current-element) nil)
    ;; use mod to cycle the element list.
    (setf current-element-number (mod (- current-element-number 1) (length elements)))
    (setf current-element (nth current-element-number elements))
    (setf (.selected current-element) t)
    ;; after we switched the element number, we also have to move the cursor.
    (update-cursor-position form)
    (draw form)
    ;; TODO: why do we need to refresh here, draw form already refreshes.
    (refresh win)))

(defun select-next-element (form event)
  "Select the next element in a form's element list."
  ;;(declare (special form))
  (with-accessors ((elements .elements) (current-element-number .current-element-number) (current-element .current-element) (win .window)) form
    (setf (.selected current-element) nil)
    ;; use mod to cycle the element list.
    (setf current-element-number (mod (+ current-element-number 1) (length elements)))
    (setf current-element (nth current-element-number elements))
    (setf (.selected current-element) t)
    ;; after we switched the element number, we also have to move the cursor.
    (update-cursor-position form)
    ;; TODO: if the selected and unselected styles are different, we have to redraw all elements on every selection change
    (draw form)
    (refresh win)))

(defgeneric move-previous-char (object event)
  (:documentation "Move to the previous char in a field or a form."))

(defmethod move-previous-char ((field field) event)
  "Move the cursor to the previous char in the field."
  (with-accessors ((inptr .fill-pointer) (dptr .display-pointer) (win .window)) field
    (when (> inptr 0)
      (decf inptr))
    ;; when the inptr moves left past the dptr, simultaneously decf the dptr.
    (when (< inptr dptr)
      (decf dptr))
    (update-cursor-position field)
    (draw field) ;; we have to redraw in the case of horizontal scrolling
    (refresh win)))

(defgeneric move-next-char (object event)
  (:documentation "Move to the next char in a field or a form."))

(defmethod move-next-char ((field field) event)
  "Move the cursor to the next char in the field."
  (with-accessors ((width .width) (inbuf .buffer) (inptr .fill-pointer) (dptr .display-pointer) (mlen .max-buffer-length)
                   (win .window)) field
    (when (and (< inptr (length inbuf))
               (not (= (1+ inptr) mlen width)))
      (incf inptr))
    ;; when the inptr moves right past the width, simultaneously incf the dptr.
    (when (and (>= inptr (+ dptr width))
               (not (= inptr mlen width)))
      (incf dptr))
    (update-cursor-position field)
    (draw field) ;; we have to redraw in the case of horizontal scrolling
    (refresh win)))

(defgeneric delete-previous-char (object event)
  (:documentation "Delete the previous char in the field or form, moving the cursor to the left."))

(defmethod delete-previous-char ((field field) event)
  "Delete the previous char in the field, moving the cursor to the left."
  (with-accessors ((inbuf .buffer) (inptr .fill-pointer) (dptr .display-pointer) (win .window)) field
    (when (> inptr 0)
      (decf inptr)
      (when (> dptr 0)
        (decf dptr))
      (setf inbuf (remove-nth (- (length inbuf) 1 inptr) inbuf)))
    ;; we dont have to redraw the complete form, just the changed field.
    (draw field)
    (refresh win)))

(defgeneric delete-next-char (object event)
  (:documentation "Delete the next char (character under the cursor) in the field or form, not moving the cursor."))

(defmethod delete-next-char ((field field) event)
  "Delete the next char (char under the cursor) in the field, not moving the cursor."
  (with-accessors ((inbuf .buffer) (inptr .fill-pointer) (dptr .display-pointer) (win .window)) field
    ;; we can only delete to the right if the inptr is not at the end of the inbuf.
    (when (> (length inbuf) inptr)
      (when (> dptr 0)
        ;; when a part of the string is hidden on the left side, shift it to the right.
        (decf dptr))
      (setf inbuf (remove-nth (- (length inbuf) (1+ inptr)) inbuf)))
    (draw field)
    (refresh win)))

(defun field-add-char (field char)
  "Add char to the current cursor position in the field.

The buffer can be longer than the displayed field width, horizontal scrolling is enabled."
  (if (and (characterp char) (graphic-char-p char))
      (progn
        (with-accessors ((width .width) (inbuf .buffer) (mlen .max-buffer-length) (inptr .fill-pointer)
                         (dptr .display-pointer) (win .window)) field
          (let ((len (length inbuf)))
            (if (.insert-mode win)

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
                        (incf dptr))) ))))
        (draw field))
      ;; if the char isnt graphic, do nothing.
      ;; TODO: this doesnt work with acs chars, which are keywords.
      nil))

(defun debug-print-field-buffer (object event)
  (declare (ignore event))
  (typecase object
    (field
     (with-accessors ((inbuf .buffer) (inptr .fill-pointer) (dptr .display-pointer) (win .window)) object
       (when (> (length inbuf) 0)
         (clear win)
         (format win "~A ~%" (value object))
         (setf inbuf nil inptr 0 dptr 0))))
    ;; when we want to debug the whole form.
    (form
     (debug-print-field-buffer (.current-element object) event)))
  (draw object))

(add-keymap :form-default-keymap
  (make-keymap
    ;; Use C-a ^A #\soh 1 to exit the edit loop.
    ;; TODO: what should the exit return?
    #\soh      'exit-event-loop
    :btab      'select-previous-element
    :up        'select-previous-element
    #\tab      'select-next-element
    :down      'select-next-element))

(add-keymap :field-default-keymap
  (make-keymap
    #\soh      'exit-event-loop  ; we need this in case a field is used alone outside of a form.
    :left      'move-previous-char
    :right     'move-next-char
    :backspace 'delete-previous-char
    :dc        'delete-next-char
    :ic        (lambda (field event)
                 (setf (.insert-mode (.window field)) (not (.insert-mode (.window field)))))
    :default   'field-add-char))

;; TODO: should we pass the event to the button function?
(defun call-button-function (button event)
  (declare (ignore event))
  (funcall (.function button)))

;; How to automatically bind a hotkey to every button?
;; that hotkey would have to be added to the form keymap, not to that of a button.
;; that would be like a global keymap, in contrast to an elements local keymap.
(add-keymap :button-default-keymap
  (make-keymap
   #\space     'call-button-function
   #\newline   'call-button-function))

;; TODO: we want edit to return the edited form.
;; exit-event-loop just returns the keyword :exit-event-loop
(defun edit (object)
  (draw object)
  ;; since we have no further args passed to run-event-loop, all handler functions have to accept
  ;; at most two arguments, object and event.
  (run-event-loop object))
