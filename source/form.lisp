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

(defun edit (win field)
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
