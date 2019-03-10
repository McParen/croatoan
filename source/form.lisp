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
  (with-accessors ((pos .position) (width .width) (style .style) (selected .selected) (win .window) (form .form)) field
    (let* ((window (if win win (.window form)))
           (bg (nth 1 style))
           (sbg (nth 3 style))
           (bgchar (if selected
                       (if sbg sbg #\space)
                       (if bg bg #\space))))
      (setf (.cursor-position window) pos)
      (add window bgchar :n width)
      (setf (.cursor-position window) pos))))

(defgeneric update-cursor-position (object)
  (:documentation "Update the cursor position of a form or a field."))

(defmethod update-cursor-position ((field field))
  "Update the cursor position of a field."
  (with-accessors ((pos .position) (inptr .fill-pointer) (dptr .display-pointer) (win .window) (form .form)) field
    (move (if win win (.window form))
          ;; TODO: assumes a single-line field.
          (car pos)
          (+ (cadr pos)           ; beginning of the field
             (- inptr dptr) ))))  ; position in the field starting with dptr

(defmethod update-cursor-position ((form form))
  "Update the cursor position of the current field of the form."
  (update-cursor-position (.current-field form)))

(defgeneric draw (object)
  (:documentation "Draw objects (form, field, menu) to their associated window."))

(defmethod draw ((field field))
  "Clear and redraw the field and its contents and background."
  (with-accessors ((pos .position) (width .width) (inbuf .buffer) (inptr .fill-pointer) (dptr .display-pointer)
                   (style .style) (selected .selected) (form .form)) field
    (let ((window (if (.window field) (.window field) (.window form)))
          (fg (nth 0 style))
          (sfg (nth 2 style))
          (len (length inbuf))
          (str (coerce (reverse inbuf) 'string)))
          ;; value
      (clear field)
      (add-string window
                  ;; display only max width chars starting from dptr
                  (if (< len width)
                      ;; if the buffer is shorter than the field, just display it.
                      str
                      ;; otherwise display a substring starting with dptr.
                      (subseq str dptr (if (< width (- len dptr))
                                           ;; if the remaining substring is longer than width, display just width chars.
                                           (+ dptr width)
                                           ;; if the remaining substring is shorter than width, just display it.
                                           len) ))
                  ;; TODO: find a more elegant way to do this.
                  :attributes (if selected (if sfg (.attributes sfg) nil) (if fg (.attributes fg) nil))
                  :color-pair (if selected (if sfg (.color-pair sfg) nil) (if fg (.color-pair fg) nil)))
      (update-cursor-position field))))

(defmethod draw ((form form))
  "Draw the form by drawing the fields, then moving the cursor to the current field."
  (with-accessors ((fields .fields) (window .window)) form
    (loop for field in fields do
      (draw field))
    ;; after drawing the fields, reposition the cursor to the current field
    (update-cursor-position form)
    (refresh window)))

;; prev-field and next-field are the only two elements where the current-field-number is changed.
;; here also current-field has to be set.
(defun select-prev-field (win event form)
  "Select the previous field in a form's field list."
  ;;(declare (special form))
  (with-accessors ((fields .fields) (current-field-number .current-field-number) (current-field .current-field)) form
    (setf (.selected current-field) nil)
    ;; use mod to cycle the field list.
    (setf current-field-number (mod (- current-field-number 1) (length fields)))
    (setf current-field (nth current-field-number fields))
    (setf (.selected current-field) t))
  ;; after we switched the field number, we also have to move the cursor.
  (update-cursor-position form)
  (draw form)
  (refresh win))

(defun select-next-field (win event form)
  "Select the next field in a form's field list."
  ;;(declare (special form))
  (with-accessors ((fields .fields) (current-field-number .current-field-number) (current-field .current-field)) form
    (setf (.selected current-field) nil)
    ;; use mod to cycle the field list.
    (setf current-field-number (mod (+ current-field-number 1) (length fields)))
    (setf current-field (nth current-field-number fields))
    (setf (.selected current-field) t))
  ;; after we switched the field number, we also have to move the cursor.
  (update-cursor-position form)
  (draw form)
  (refresh win))

(defgeneric move-prev-char (window event object)
  (:documentation "Move to the previous char in a field or a form."))

(defmethod move-prev-char (win event (field field))
  "Move the cursor to the previous char in the field."
  (with-accessors ((inptr .fill-pointer) (dptr .display-pointer)) field
    (when (> inptr 0)
      (decf inptr))
    ;; when the inptr moves left past the dptr, simultaneously decf the dptr.
    (when (< inptr dptr)
      (decf dptr)))
  (update-cursor-position field)
  (draw field) ;; we have to redraw in the case of horizontal scrolling
  (refresh win))

(defmethod move-prev-char (win event (form form))
  "Move the cursor to the previous char in the current field of form."
  (move-prev-char win event (.current-field form)))

(defgeneric move-next-char (window event object)
  (:documentation "Move to the next char in a field or a form."))

(defmethod move-next-char (win event (field field))
  "Move the cursor to the next char in the field."
  (with-accessors ((width .width) (inbuf .buffer) (inptr .fill-pointer) (dptr .display-pointer) (mlen .max-buffer-length)) field
    (when (and (< inptr (length inbuf))
               (not (= (1+ inptr) mlen width)))
      (incf inptr))
    ;; when the inptr moves past the width, simultaneously incf the dptr.
    (when (and (>= inptr (+ dptr width))
               ;; do not incf the dptr when we're at the end of a fixed width field.
               (not (= inptr mlen width)))
      (incf dptr)))
  (update-cursor-position field)
  (draw field) ;; we have to redraw in the case of horizontal scrolling
  (refresh win))

(defmethod move-next-char (win event (form form))
  "Move the cursor to the next char in the current field of the form."
  (move-next-char win event (.current-field form)))

(defgeneric delete-prev-char (window event object)
  (:documentation "Delete the previous char in the field or form, moving the cursor to the left."))

(defmethod delete-prev-char (win event (field field))
  "Delete the previous char in the field, moving the cursor to the left."
  (with-accessors ((inbuf .buffer) (inptr .fill-pointer) (dptr .display-pointer)) field
    (when (> inptr 0)
      (decf inptr)
      (when (> dptr 0)
        (decf dptr))
      (setf inbuf (remove-nth (- (length inbuf) 1 inptr) inbuf))))
  ;; we dont have to redraw the complete form, just the changed field.
  (draw field))

(defmethod delete-prev-char (win event (form form))
  "Delete the previous char in the field, moving the cursor to the left."
  (delete-prev-char win event (.current-field form)))

(defgeneric delete-next-char (window event object)
  (:documentation "Delete the next char (character under the cursor) in the field or form, not moving the cursor."))

(defmethod delete-next-char (win event (field field))
  "Delete the next char (char under the cursor) in the field, not moving the cursor."
  (with-accessors ((inbuf .buffer) (inptr .fill-pointer) (dptr .display-pointer)) field
    ;; we can only delete to the right if the inptr is not at the end of the inbuf.
    (when (> (length inbuf) inptr)
      (when (> dptr 0)
        ;; when a part of the string is hidden on the left side, shift it to the right.
        (decf dptr))
      (setf inbuf (remove-nth (- (length inbuf) (1+ inptr)) inbuf))))
  (draw field))

(defmethod delete-next-char (win event (form form))
  "Delete the next char (char under the cursor) in the current field of form, not moving the cursor."
  (delete-next-char win event (.current-field form)))

(defun field-add-char (win char field)
  "Add char to the current cursor position in the field.

The buffer can be longer than the displayed field width, horizontal scrolling is enabled."
  (if (and (characterp char) (graphic-char-p char))
      (progn
        (with-accessors ((width .width) (inbuf .buffer) (mlen .max-buffer-length) (inptr .fill-pointer) (dptr .display-pointer)) field
          (let ((len (length inbuf)))
            (if (.insert-mode win)
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

(defun form-add-char (win char form)
  "Add char to the current field of the form."
  (field-add-char win char (.current-field form)))

(defun field-buffer-to-string (field)
  "Return the value of the field buffer (list of chars) as a string."
  (coerce (reverse (.buffer field)) 'string))

(defun debug-print-field-buffer (win event object)
  (declare (ignore event))
  (typecase object
    (field
     (with-accessors ((inbuf .buffer) (inptr .fill-pointer) (dptr .display-pointer)) object
       (when (> (length inbuf) 0)
         (clear win)
         (format win "~A ~%" (coerce (reverse inbuf) 'string))
         (setf inbuf nil inptr 0 dptr 0))))
    (form
     (with-accessors ((current-field .current-field)) object
       (debug-print-field-buffer win event current-field))))
  (draw object))

;; TODO: compare to how emacs handles keymaps, add sub-keymaps
;; keymap for editing forms comprised of single fields.
(add-keymap :form-default-keymap
  (make-keymap

    ;; Use C-a ^A #\soh 1 to exit the edit loop.
    ;; TODO: what should the exit return?
    #\soh      'exit-event-loop
   
    :btab      'select-prev-field
    :up        'select-prev-field
    #\tab      'select-next-field
    :down      'select-next-field
    :left      'move-prev-char
    :right     'move-next-char
    :backspace 'delete-prev-char
    :dc        'delete-next-char

    :ic        (lambda (win event form)
                 ;; If an optional argument like form is passed to run-event-loop,
                 ;; the handler functions have to handle it.  
                 (setf (.insert-mode win) (not (.insert-mode win))))

    ;;#\newline  'debug-print-field-buffer
    :default   'form-add-char))

;; keymap used for editing single fields which are not part of a form.
(add-keymap :field-default-keymap
  (make-keymap

    ;; Use C-a ^A #\soh 1 to exit the edit loop.
    ;; TODO: what should the exit return?
    #\soh      'exit-event-loop
   
    :left      'move-prev-char
    :right     'move-next-char
    :backspace 'delete-prev-char
    :dc        'delete-next-char

    :ic        (lambda (win event form)
                 ;; If an optional argument like form is passed to run-event-loop,
                 ;; the handler functions have to handle it.  
                 (setf (.insert-mode win) (not (.insert-mode win))))

    ;;#\newline  'debug-print-field-buffer
    :default   'field-add-char))

(defun edit (window object)
  "Allow the used to edit fields or forms."
  (draw object)

  (let ((default-keymap (typecase object
                          (field :field-default-keymap)
                          (form  :form-default-keymap))))
  
    ;; if the user didnt add any event handlers, add the default keymap.
    (unless (.event-handlers window)
      (setf (.event-handlers window) (get-keymap default-keymap))))
  
  ;; the optional arg form will be passed by run-event-loop to the
  ;; event handler functions along with win and event.
  (run-event-loop window object))
