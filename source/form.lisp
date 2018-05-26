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

(defun draw-field (win field)
  "Clear and redraw the field and its contents and background."
  (with-accessors ((pos .position) (width .width) (inbuf .buffer) (inptr .fill-pointer)) field
    (clear-field win field)
    (add-string win (coerce (reverse inbuf) 'string) :attributes '(:underline))
    (move win (car pos) (+ (cadr pos) inptr))
    (refresh win)))

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

;; we HAVE to merge edit-form and edit(-field) because we have to
;; catch #\soh, tab and all other field-switching events in one single event loop.

(defun edit-form (win form)
  "Let the user edit the form fields, then return the form object."
  ;; declaring the field special lets the object keep its contents after the edit functions returns
  (declare (special form))
  (with-accessors ((fields .fields) (current-field-number .current-field-number)) form
    
    ;;(draw-field win field)
    (draw-form win form)
    ;; TODO: (draw-form form-window)
    ;; TODO: (draw form-window)

    ;;(event-case (win event)
    (loop named event-case do
       (let ((event (get-wide-event win))
             ;; we have to get the current field and its acessors every time we go through the loop.
             ;; TODO: field -> current-field
             ;; TODO: use symbol-macrolet for the current field
             (field (nth current-field-number fields)))
         (with-accessors ((pos .position) (width .width) (inbuf .buffer) (inptr .fill-pointer)) field
           (case event

      ;; Use C-a ^A #\soh 1 to exit the edit loop.
      ;; TODO: what to return, a form or the buffers?          
      (#\soh (return-from event-case inbuf))

      ;; TODO: use mod to cycle the field list.
      ;; TODO: also change the cursor
      (#\tab (setf current-field-number (mod (+ 1 current-field-number) (length fields)))      ; TAB, C-i
             ;;(move win (car pos) (+ (cadr pos) inptr))

             ;; print the current field number for debug purposes
             (move win 0 0)
             (format t "~A~%" current-field-number) (refresh win)

             ;; TODO: after we switched the field number, we also have to move the cursor.
             ;; the way we do it here is ugly.
             (let* ((field (nth current-field-number fields))
                    (pos (.position field)))
               (move win
                     (car pos)
                     (+ (cadr pos)
                        (.fill-pointer field))))
             (refresh win))

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
         (draw-form win form)
         (move win (car pos) (+ (cadr pos) inptr))
         (refresh win)
         ;;(draw-field win field)
         ))

      ;; delete one char to the right
      (:dc
       (when (> (length inbuf) inptr)
         (setf inbuf (remove-nth (- (length inbuf) (1+ inptr)) inbuf))
         (draw-field win field)
         ;;(draw-form win form)
         ))

      ;; delete one char to the left
      (:backspace
       ;; we can use it only until the first char.
       (when (> inptr 0)
         (decf inptr)
         (setf inbuf (remove-nth (- (length inbuf) 1 inptr) inbuf))
         (draw-field win field)
         ;;(draw-form win form)
         ))

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
       (draw-field win field)) ))))))
       ;;(draw-form win form))
