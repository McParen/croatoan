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

;; TODO: rename to clear, make clear a method specialized on fields, forms and windows.
;; TODO: (defmethod clear (win (field field)))
(defun clear-field (win field)
  "Do what (clear win) does, but for a single field.

clear = write space combined with the background char."
  (with-accessors ((pos .position) (width .width)) field
    (setf (.cursor-position win) pos)
    ;; TODO: overwrite the field with the background char, not just with a space.
    (add-char win #\space :n width)
    (setf (.cursor-position win) pos)
    ;; this is the only place we set the attribute for the whole field
    (change-attributes win width '(:underline))))

(defgeneric update-cursor-position (window object)
  (:documentation "Update the cursor position of a form or a field."))

(defmethod update-cursor-position (win (field field))
  "Update the cursor position of a field."
  (with-accessors ((pos .position) (inptr .fill-pointer)) field
    (move win
          (car pos)
          (+ (cadr pos) inptr))))

(defmethod update-cursor-position (win (form form))
  "Update the cursor position of the current field of the form."
  (update-cursor-position win (.current-field form)))

(defgeneric draw (window object)
  (:documentation "Draw forms and their elements like fields and buttons."))

(defmethod draw (window (field field))
  "Clear and redraw the field and its contents and background."
  (with-accessors ((pos .position) (width .width) (inbuf .buffer) (inptr .fill-pointer)) field
    (clear-field window field)
    (add-string window (coerce (reverse inbuf) 'string) :attributes '(:underline))
    (update-cursor-position window field)))

;; we have to make window a parameter because fields do not have a window slot
(defmethod draw (win (form form))
  "Draw the form by drawing the fields, then moving the cursor to the current field."
  (with-accessors ((fields .fields) (current-field .current-field)) form
    (loop for field in fields do
      (draw win field))
    ;; after drawing the fields, reposition the cursor to the current field
    (update-cursor-position win form)
    (refresh win)))

;; prev-field and next-field are the only two elements where the current-field-number is changed.
;; here also current-field has to be set.
(defun select-prev-field (win event form)
  "Select the previous field in a form's field list."
  ;;(declare (special form))
  (with-accessors ((fields .fields) (current-field-number .current-field-number) (current-field .current-field)) form
    ;; use mod to cycle the field list.
    (setf current-field-number (mod (- current-field-number 1) (length fields)))
    (setf current-field (nth current-field-number fields)))
  ;; after we switched the field number, we also have to move the cursor.
  (update-cursor-position win form)
  (refresh win))

(defun select-next-field (win event form)
  "Select the next field in a form's field list."
  ;;(declare (special form))
  (with-accessors ((fields .fields) (current-field-number .current-field-number) (current-field .current-field)) form
    ;; use mod to cycle the field list.
    (setf current-field-number (mod (+ current-field-number 1) (length fields)))
    (setf current-field (nth current-field-number fields)))
  ;; after we switched the field number, we also have to move the cursor.
  (update-cursor-position win form)
  (refresh win))

(defgeneric move-prev-char (window event object)
  (:documentation "Move to the previous char in a field or a form."))

(defmethod move-prev-char (win event (field field))
  "Move the cursor to the previous char in the field."
  (with-accessors ((inptr .fill-pointer)) field
    (when (> inptr 0)
      (decf inptr)))
  (update-cursor-position win field)
  (refresh win))

(defmethod move-prev-char (win event (form form))
  "Move the cursor to the previous char in the current field of form."
  (move-prev-char win event (.current-field form)))

(defgeneric move-next-char (window event object)
  (:documentation "Move to the next char in a field or a form."))

(defmethod move-next-char (win event (field field))
  "Move the cursor to the next char in the field."
  (with-accessors ((inbuf .buffer) (inptr .fill-pointer)) field
      (when (< inptr (length inbuf))
        (incf inptr)))
  (update-cursor-position win field)
  (refresh win))

(defmethod move-next-char (win event (form form))
  "Move the cursor to the next char in the current field of the form."
  (move-next-char win event (.current-field form)))

(defgeneric delete-prev-char (window event object)
  (:documentation "Delete the previous char in the field or form, moving the cursor to the left."))

(defmethod delete-prev-char (win event (field field))
  "Delete the previous char in the field, moving the cursor to the left."
  (with-accessors ((inbuf .buffer) (inptr .fill-pointer)) field
    (when (> inptr 0)
      (decf inptr)
      (setf inbuf (remove-nth (- (length inbuf) 1 inptr) inbuf))))
  ;; we dont have to redraw the complete form, just the changed field.
  (draw win field))

(defmethod delete-prev-char (win event (form form))
  "Delete the previous char in the field, moving the cursor to the left."
  (delete-prev-char win event (.current-field form)))

(defgeneric delete-next-char (window event object)
  (:documentation "Delete the next char (character under the cursor) in the field or form, not moving the cursor."))

(defmethod delete-next-char (win event (field field))
  "Delete the next char (char under the cursor) in the field, not moving the cursor."
  (with-accessors ((inbuf .buffer) (inptr .fill-pointer)) field
    ;; we can only delete to the right if the inptr is not at the end of the inbuf.
    (when (> (length inbuf) inptr)
      (setf inbuf (remove-nth (- (length inbuf) (1+ inptr)) inbuf))))
  (draw win field))

(defmethod delete-next-char (win event (form form))
  "Delete the next char (char under the cursor) in the current field of form, not moving the cursor."
  (delete-next-char win event (.current-field form)))

(defun field-add-char (win char field)
  "Add char to the current cursor position in the field."
  (if (and (characterp char)
           (graphic-char-p char))
      (progn
        (with-accessors ((width .width) (inbuf .buffer) (inptr .fill-pointer)) field
          (let ((len (length inbuf)))
            ;; only add chars until we have reached the max width of the field
            ;; longer buffers and scrolling are not supported yet.
            (unless (>= len width)
              ;; if we're at the end of the inbuf
              (if (= inptr len)
                  ;; just add another char to the inbuf
                  (setf inbuf (cons char inbuf))
                  ;; if we're in the middle of the field, either insert or replace
                  (if (.insert-enabled win)
                      (setf inbuf (insert-nth  (- len inptr)      char inbuf))
                      (setf inbuf (replace-nth (- len (1+ inptr)) char inbuf))))
              ;; both replacing and inserting advance the cursor
              (incf inptr))))
        ;; after were done with editing, redraw the field.
        ;; TODO: redraw the entire field or echo one single char?
        (draw win field))
      ;; if char is not graphic, simply return nil.
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
     (with-accessors ((inbuf .buffer) (inptr .fill-pointer)) object
       (when (> (length inbuf) 0)
         (clear win)
         (format win "~A ~%" (coerce (reverse inbuf) 'string))
         (setf inbuf nil inptr 0))))
    (form
     (with-accessors ((current-field .current-field)) object
       (debug-print-field-buffer win event current-field))))
  (draw win object))

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
                 (setf (.insert-enabled win) (not (.insert-enabled win))))

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
                 (setf (.insert-enabled win) (not (.insert-enabled win))))

    ;;#\newline  'debug-print-field-buffer
    :default   'field-add-char))

(defun edit (window object)
  "Allow the used to edit fields or forms."
  (draw window object)

  (let ((default-keymap (typecase object
                          (field :field-default-keymap)
                          (form  :form-default-keymap))))
  
    ;; if the user didnt add any event handlers, add the default keymap.
    (with-accessors ((event-handlers .event-handlers)) window
      (unless event-handlers
        (setf event-handlers (get-keymap default-keymap)))))

  ;; the optional arg form will be passed by run-event-loop to the
  ;; event handler functions along with win and event.
  (run-event-loop window object))
