(in-package :de.anvi.croatoan)

;; form
;; curses extension for programming forms
;; https://invisible-island.net/ncurses/man/form.3x.html

(defun remove-nth (n list)
  (declare
    (type (integer 0) n)
    (type list list))
  "Remove element at nth place from the list, decreasing the length of the list.

Example: (remove-nth 3 '(a b c d e)) => (A B C E)"
  (assert (>= n 0))
  (assert (> (length list) n))
  (if (or (zerop n) (null list))
    (cdr list)
    (cons (car list) (remove-nth (1- n) (cdr list)))))

(defun insert-nth (n element list)
  (declare
    (type (integer 0) n)
    (type list list))
  "Insert element into list at nth place, increasing the length of the list.

Example: (insert-nth 3 'x '(a b c d e)) => (A B C X D E)"
  (assert (>= n 0))
  (assert (>= (length list) n))
  (if (or (zerop n) (null list))
      (cons element list)
      (cons (car list) (insert-nth (1- n) element (cdr list)))))

(defun replace-nth (n element list)
  (declare
    (type (integer 0) n)
    (type list list))
  "Replaces element of list at nth place, not increasing the length of the list.

Example: (replace-nth 3 'x '(a b c d e)) => (A B C X E)"
  (assert (>= n 0))
  (assert (>= (length list) n))
  (if (or (zerop n) (null list))
      (cons element (cdr list))
      (cons (car list) (replace-nth (1- n) element (cdr list)))))

;; TODO: rename to clear, make clear a method specialized on fields
(defun clear-field (win field)
  "Do what (clear win) does, but for a single field.

clear = write space combined with the background char."
  (with-accessors ((pos .position) (width .width)) field
    (setf (.cursor-position win) pos)
    (add-char win #\space :n width)
    (setf (.cursor-position win) pos)
    ;; this is the only place we set the attribute for the whole field
    (change-attributes win width '(:underline))))

;; echo a single char to the field instead all these routines
;; TODO: we do not have to clear the field completely every time
;; we can write the string first, then clear the rest of the field.

(defgeneric draw (window object)
  (:documentation "Draw forms and their elements like fields and buttons."))

(defmethod draw (window (field field))
  "Clear and redraw the field and its contents and background."
  (with-accessors ((pos .position) (width .width) (inbuf .buffer) (inptr .fill-pointer)) field
    (clear-field window field)
    (add-string window (coerce (reverse inbuf) 'string) :attributes '(:underline))
    (move window (car pos) (+ (cadr pos) inptr)) ))

;; we have to make window a parameter because fields do not have a window slot
(defmethod draw (window (form form))
  "Draw the form by drawing the fields, then moving the cursor to the current field."
  (declare (ignore window))
  ;; TODO: "current" field or "active" field??
  (with-accessors ((fields .fields) (current-field-number .current-field-number) (win .window)) form
    (loop for field in fields do
      ;; TODO: do we refresh in draw-field AND here?
      (draw-field win field))
    ;; after drawing the fields, reposition the cursor to the current field
    ;; ugly, we have to find a better way to do this
    (let* ((current-field (nth current-field-number fields))
           (pos (.position current-field)))
      (move win
            (car pos)
            (+ (cadr pos)
               (.fill-pointer current-field))))
    (refresh win)))

(defun draw-field (win field)
  "Clear and redraw the field and its contents and background."
  (with-accessors ((pos .position) (width .width) (inbuf .buffer) (inptr .fill-pointer)) field
    (clear-field win field)
    (add-string win (coerce (reverse inbuf) 'string) :attributes '(:underline))
    (move win (car pos) (+ (cadr pos) inptr))
    (refresh win)))

;; is this better than a form-specific method?
(defun draw-form (win form)
  "Draw the form by drawing the fields, then moving the cursor to the current field."
  ;; TODO: "current" field or "active" field??
  (loop for field in (.fields form) do
       (draw-field win field))
  ;; after drawing the fields, reposition the cursor
  ;; ugly, we have to find a better way to do this
  (let* ((field (nth (.current-field-number form) (.fields form)))
         (pos (.position field)))
    (move win
          (car pos)
          (+ (cadr pos)
             (.fill-pointer field))))
  (refresh win))

;; we have to merge edit-form and edit(-field) because we have to
;; catch #\soh, tab and all other field-switching events in one single event loop.
(defun edit-field (win field)
  "Let the user edit the field, then return the contents."
  ;; declaring the field special lets the object keep its contents after the edit functions returns
  (declare (special field))
  (with-accessors ((pos .position) (width .width) (inbuf .buffer) (inptr .fill-pointer)) field
    (draw-field win field)
    (event-case (win event)
      ;; TODO: use some other key for exiting the edit loop, maybe C-something.
      ;; C-a ^A #\soh 1          
      (#\soh (return-from event-case inbuf))
      
      (:left
       (when (> inptr 0) (decf inptr))
       (move win (car pos) (+ (cadr pos) inptr)))

      (:right
       (when (< inptr (length inbuf)) (incf inptr))
       (move win (car pos) (+ (cadr pos) inptr)))
      
      ;; for debugging, return prints the content of the buffer and then deletes the buffer
      (#\newline
       (when (> (length inbuf) 0)
         (clear win)
         (format t "~A~%" (coerce (reverse inbuf) 'string)) (refresh win)
         (setf inbuf nil inptr 0)
         (draw-field win field)))

      ;; delete one char to the right
      (:dc
       (when (> (length inbuf) inptr)
         (setf inbuf (remove-nth (- (length inbuf) (1+ inptr)) inbuf))
         (draw-field win field) ))

      ;; delete one char to the left
      (:backspace
       ;; we can use it only until the first char.
       (when (> inptr 0)
         (decf inptr)
         (setf inbuf (remove-nth (- (length inbuf) 1 inptr) inbuf))
         (draw-field win field) ))

      ;; toggle insert/overwrite mode
      (:ic (setf (.insert-enabled win) (not (.insert-enabled win))))

      ;; display every other character
      ;; TODO: limit handling to displayable graphic characters only
      (otherwise
       (let ((len (length inbuf)))
         ;; only add chars when we haven reached the max width of the field
         (unless (= len width)
           ;; if we're at the end of the inbuf
           (if (= inptr len)
               ;; just add another char to the inbuf
               (setf inbuf (cons event inbuf))
               ;; if we're in the middle of the field, either insert or replace
               (if (.insert-enabled win)
                   ;; if we're somewhere in the middle, either insert or replace.
                   (setf inbuf (insert-nth  (- len inptr)      event inbuf))
                   (setf inbuf (replace-nth (- len (1+ inptr)) event inbuf))))
           ;; both replacing and inserting advance the cursor
           (incf inptr)))

       (draw-field win field)) )))

;; TODO: add buttons as form elements, so we can reach them with TAB

(defun update-cursor-position (form)
  (with-accessors ((fields .fields) (current-field-number .current-field-number) (win .window)) form
    (let* ((current-field (nth current-field-number fields))
           (pos (.position current-field)))
      (move win
            (car pos)
            ;; the x position in the field is the x position of the field start + the fill pointer in the field
            (+ (cadr pos)
               (.fill-pointer current-field))))
    (refresh win)))

;; TODO: prev-field and next-field are the only two elements where the current-field-number is changed.
(defun select-prev-field (form)
  "Select the previous field in a form's field list."
  ;;(declare (special form))
  (with-accessors ((fields .fields) (current-field-number .current-field-number) (current-field .current-field) (win .window)) form
    ;; use mod to cycle the field list.
    (setf current-field-number (mod (- current-field-number 1) (length fields)))
    ;; TODO: before we can set the prev current field, we have to setf the first current field.
    (setf current-field (nth current-field-number fields)))
  ;; after we switched the field number, we also have to move the cursor.
  (update-cursor-position form))

(defun select-next-field (form)
  "Select the next field in a form's field list."
  ;;(declare (special form))
  (with-accessors ((fields .fields) (current-field-number .current-field-number) (current-field .current-field) (win .window)) form
    ;; use mod to cycle the field list.
    (setf current-field-number (mod (+ current-field-number 1) (length fields)))
    (setf current-field (nth current-field-number fields)))
  ;; after we switched the field number, we also have to move the cursor.
  (update-cursor-position form))

(defun move-prev-char (form)
  "Move the cursor to the previous char in the current field."
  (with-accessors ((current-field .current-field) (win .window)) form
      (with-accessors ((inptr .fill-pointer)) current-field
        (when (> inptr 0)
          (decf inptr))))
        ;; (move win (car pos) (+ (cadr pos) inptr)))
  (update-cursor-position form))

(defun move-next-char (form)
  "Move the cursor to the previous char in the current field."
  (with-accessors ((current-field .current-field) (win .window)) form
      (with-accessors ((inbuf .buffer) (inptr .fill-pointer)) current-field
        (when (< inptr (length inbuf))
          (incf inptr))))
        ;; (move win (car pos) (+ (cadr pos) inptr)))
  (update-cursor-position form))

;; :backspace, delete one char to the left
(defun delete-prev-char (form)
  "Delete the previous char in the current field, moving the cursor to the left."
  (with-accessors ((current-field .current-field) (win .window)) form
    (with-accessors ((inbuf .buffer) (inptr .fill-pointer)) current-field
      (when (> inptr 0)
        (decf inptr)
        (setf inbuf (remove-nth (- (length inbuf) 1 inptr) inbuf))))
    ;; we dont have to redraw the complete form, just the changed field.
    (draw-field win current-field)))

;; :dc, delete one char to the right
(defun delete-next-char (form)
  "Delete the next char in the current field, not moving the cursor."
  (with-accessors ((current-field .current-field) (win .window)) form
    (with-accessors ((inbuf .buffer) (inptr .fill-pointer)) current-field
      ;; we can only delete to the right if the inptr is not at the end of the inbuf.
      (when (> (length inbuf) inptr)
        (setf inbuf (remove-nth (- (length inbuf) (1+ inptr)) inbuf))))
    (draw-field win current-field)))

;; TODO: test that char is graphic
(defun form-add-char (form char)
  "Add char to the current field of the form."
  (with-accessors ((current-field .current-field) (win .window)) form
    (with-accessors ((width .width) (inbuf .buffer) (inptr .fill-pointer)) current-field
      (let ((len (length inbuf)))
        ;; only add chars when we haven reached the max width of the field
        ;; longer buffers and scrolling are not supported yet.
        (unless (= len width)
          ;; if we're at the end of the inbuf
          (if (= inptr len)
              ;; just add another char to the inbuf
              (setf inbuf (cons char inbuf))
              ;; if we're in the middle of the field, either insert or replace
              (if (.insert-enabled win)
                  (setf inbuf (insert-nth  (- len inptr)      char inbuf))
                  (setf inbuf (replace-nth (- len (1+ inptr)) char inbuf)))))
        ;; both replacing and inserting advance the cursor
        (incf inptr)))
    ;; after were done with editing, redraw the field.
    (draw-field win current-field)))

(defun field-buffer-to-string (field)
  "Return the value of the field buffer (list of chars) as a string."
  (coerce (reverse (.buffer field)) 'string))

(defun debug-print-field-buffer (form event)
  (declare (ignore event))
  (with-accessors ((current-field .current-field) (win .window)) form
    (with-accessors ((inbuf .buffer) (inptr .fill-pointer)) current-field
      (when (> (length inbuf) 0)
        (clear win)
        (format win "~A ~%" (coerce (reverse inbuf) 'string))
        (setf inbuf nil inptr 0)
        (draw win form) ))))

;; here we do not pass the window to the function, but we associate the window with the form in the window slot
;; see t16f
(defun edit (form)
  "Let the user edit the form fields, then return the form object."
  ;; declaring the field special lets the object keep its contents after the edit functions returns
  ;; we need this only when we want to edit the field values in place instead of returning the form object
  ;;(declare (special form))
  (draw (.window form) form)
  (with-accessors ((current-field .current-field) (win .window)) form
    ;; TODO: we need better event handling than a event-case.
    ;; a user needs to be able to pass his own keybindings (alist or hash table)
    (event-case (win event)
      ;; Use C-a ^A #\soh 1 to exit the edit loop.
      ;; TODO: what to return, a form or the buffers?
      ;; TODO: we need a function and throw-catch so the user can bind his own key to accept the form.
      ;; TODO: trim left and right spaces before returning
      (#\soh (return-from event-case nil))

      ;; TODO: the parameter form doesnt fit to the (handler win event) form used in run-event-loop.
      ;; this means that the used cant simply bind these functions to the events
      ;; how do we reconcile this?
      ;; maybe accept no arguments and only a rest parameter and every handler has to pick arguments by itself?
            
      ;; TAB, C-i, S-TAB (= :btab)
      ((:btab :up)   (select-prev-field form))
      ((#\tab :down) (select-next-field form))
      (:left         (move-prev-char form))
      (:right        (move-next-char form))
      (:backspace    (delete-prev-char form))
      (:dc           (delete-next-char form))

      ;; for debugging, return prints the content of the buffer and then deletes the buffer
      ;; PROBLEM: how is the user supposed to add bindings to the event loop?
      ;; everything we have done here is hard-coding all the keys.
      ;; TODO: we dont want to define handlers here, the user should define them in his keybindings.
      (#\newline     (debug-print-field-buffer form event))

      ;; toggle insert/overwrite mode
      (:ic (setf (.insert-enabled win) (not (.insert-enabled win))))

      ;; TODO: try to print only graphic chars.
      (otherwise (form-add-char form event)) )))
