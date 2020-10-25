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

(defun find-element (form element-name &key (test #'eql) (key #'name))
  "Return from the given form the element given by its name.

The name should be a keyword, symbol or integer, the default test is eql.

If the name is a string, equal should be used as the test.

Instead of the name, another key can be provided to identify the element."
  (find element-name (elements form) :test test :key key))

;; this is the only place we set the background style for the field
;; TODO: how to access the default fg and bg of a form,
;; if the field is not part of a form? by having a form slot in the field.

(defmethod clear ((field field) &key)
  "Clear the field by overwriting it with the background char.

The default background char is #\space."
  (with-accessors ((pos element-position) (width width) (selected selectedp) (win window) (style style)) field
    (let* ((bg-style (if selected (getf style :selected-background) (getf style :background)))
           (bg-char  (if (getf bg-style :simple-char) (getf bg-style :simple-char) #\space)))
      (setf (cursor-position win) pos)
      (add win bg-char :style bg-style :n width)
      (setf (cursor-position win) pos))))

(defgeneric update-cursor-position (object)
  (:documentation "Update the cursor position of the element of a form.")
  (:method (object)
    "The default method puts the cursor at the start position of the element."
    (setf (cursor-position (window object)) (element-position object))
    (refresh (window object))))

;; when the form element is an embedded selection menu or checklist
;; will not work for menu-windows, which arent yet embedded in forms.
;; we need a separate update-cursor-position for menu-window.
;; used in menu.lisp/(draw menu)

(defmethod update-cursor-position ((object menu))
  "Update the cursor position of a menu after it is drawn.

Place the cursor, when it is visible, on the first char of the current item."
  (setf (cursor-position (window object)) (current-item-position object))
  (refresh (window object)))

(defmethod update-cursor-position ((object checklist))
  "Update the cursor position of a checklist after it is drawn.

Place the cursor between the brackets [_] of the current item."
  (with-accessors ((pos current-item-position) (win window)) object
    (move win
          (car pos)
          (1+ (cadr pos))) ;; put the cursor after the [
    (refresh win)))

(defmethod update-cursor-position ((checkbox checkbox))
  "Update the cursor position of a checkbox."
  (with-accessors ((pos element-position) (win window)) checkbox
    (move win
          (car pos)
          (1+ (cadr pos))) ;; put the cursor after the [
    (refresh win) ))

(defmethod update-cursor-position ((field field))
  "Update the cursor position of a field."
  (with-accessors ((pos element-position) (inptr input-pointer) (dptr display-pointer) (win window)) field
    (move win
          ;; TODO: assumes a single-line field.
          (car pos)
          (+ (cadr pos)           ; beginning of the field
             (- inptr dptr) ))    ; position in the field starting with dptr
    (refresh win)))

(defmethod update-cursor-position ((form form))
  "Move the cursor to the correct position in current element of the form."
  (update-cursor-position (current-element form)))

(defgeneric draw (object)
  (:documentation "Draw objects (form, field, menu) to their associated window."))

(defmethod draw ((label label))
  (with-accessors ((pos element-position) (win window) (name name) (title title) (width width) (style style) (reference reference)
                   (parent-form parent-form)) label
    ;; pick the string to write in the following order
    ;;   title of the label
    ;;   title of the referenced element
    ;;   name of the referenced element
    ;;   name of the label
    (let* ((text (or title
                     (title (find-element parent-form reference))
                     (name (find-element parent-form reference))
                     name))
           (string (when text (format nil "~A" text)))
           (fg-style (getf style :foreground))
           (bg-style (getf style :background))
           (bg-char (if (getf bg-style :simple-char) (getf bg-style :simple-char) #\space)))
      (when string
        ;; first draw the background, but only if width > string
        (when width
          (apply #'move win pos)
          (add win bg-char :style bg-style :n width))
        ;; then the label over the background
        (apply #'move win pos)
        (add-string win string :style fg-style)))))

(defmethod draw ((button button))
  (with-accessors ((pos element-position) (name name) (title title) (win window) (selected selectedp) (style style)) button
    (apply #'move win pos)
    (let* ((fg-style (if selected (getf style :selected-foreground) (getf style :foreground))))
      (add-string win (format nil "<~A>" (if title title name)) :style fg-style))))

(defmethod draw ((checkbox checkbox))
  (with-accessors ((pos element-position) (name name) (win window) (selected selectedp) (style style)
                   (checkedp checkedp)) checkbox
    (apply #'move win pos)
    (let* ((fg-style (if selected (getf style :selected-foreground) (getf style :foreground))))
      (add-string win (format nil "[~A]" (if checkedp "X" "_")) :style fg-style)
      (update-cursor-position checkbox))))

(defmethod draw ((field field))
  "Clear and redraw the field and its contents and background."
  (with-accessors ((pos element-position) (width width) (inbuf buffer) (inptr input-pointer) (dptr display-pointer)
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

(defmethod draw ((form form))
  "Draw the form by drawing the elements, then moving the cursor to the current element."
  (with-accessors ((elements elements) (window window)) form
    (loop for element in elements do
      (draw element))
    ;; after drawing the elements, reposition the cursor to the current element
    (update-cursor-position form)))

(defun make-title-string (widget &optional (beg "") (end ""))
  "Make a title string for a decorated window containig a form or a menu.

Use the title string of the widget if provided, otherwise use its symbol name."
  (with-accessors ((title title) (name name)) widget
    (let* ((str (if (typep title 'string) title name)))
      (format nil
              ;; format template for the title depending on its length.
              ;; "12345" => "| 12345 |"
              (concatenate 'string beg "~" (write-to-string (+ (length str) 2)) ":@<~A~>" end)
              str))))

(defun add-title (win)
  "Draw a title to the first line of a window.

Usually, this will be a decorated window with a border and the title on the top border.

When title is t instead of a title string, display the symbol name of the widget."
  (add-string win (make-title-string win) :y 0 :x 2))

(defmethod draw ((win decorated-window))
  "Draw the background window, and the title and the border if they are given."
  (with-accessors ((title title)) win
    (touch win)
    (when title
      (add-title win))
    (refresh win)
    ;; in multiple inheritance, a decorated-window should be the first superclass
    ;; so we can first draw the border and title here and then
    ;; call-next-method the second class here to draw the contents
    ;; provided by the next superclass (for example menu or form)
    ;; If there was a draw method for windows, it would be called before going to the next superclass
    ;; but we do not have a draw method for simple windows.
    (when (next-method-p)
      (call-next-method))))

(defmethod draw ((fwin form-window))
  "Draw the the border and the title, then the form."
  ;; The draw method does nothng specific for a form-window
  ;; first call next method to draw the decorated window (border and title)
  ;; then from the decorated window call-next-method to draw the form contents
  (when (next-method-p)
    (call-next-method)))

;; previous-element and next-element are the only two elements where the current-element-number is changed.
;; here also current-element and selected has to be set.
(defun select-previous-element (form event &rest args)
  "Select the previous element in a form's element list."
  ;;(declare (special form))
  (with-accessors ((elements elements) (current-element-number current-element-number) (current-element current-element) (win window)) form
    (setf (selectedp current-element) nil)

    ;; use mod to cycle the element list.
    (setf current-element-number (mod (- current-element-number 1) (length elements)))
    (setf current-element (nth current-element-number elements))
    
    ;; ignore inactive elements like labels.
    (if (activep current-element)
        (progn
          (setf (selectedp current-element) t)
          ;; after we switched the element number, we also have to redraw the form.
          (draw form))
        (select-previous-element form event))))

(defun select-next-element (form event &rest args)
  "Select the next element in a form's element list."
  ;;(declare (special form))
  (with-accessors ((elements elements) (current-element-number current-element-number) (current-element current-element) (win window)) form
    (setf (selectedp current-element) nil)
    
    ;; use mod to cycle the element list.
    (setf current-element-number (mod (+ current-element-number 1) (length elements)))
    (setf current-element (nth current-element-number elements))

    ;; ignore inactive elements like labels.
    (if (activep current-element)
        (progn
          (setf (selectedp current-element) t)
          ;; after we switched the element number, we also have to redraw the form.
          (draw form))
        (select-next-element form event))))

(defun move-start-of-line (field event &rest args)
  "Move the cursor to the first char in the field."
  (declare (ignore event))
  (with-accessors ((inptr input-pointer) (dptr display-pointer) (win window)) field
    (setf inptr 0 dptr 0)
    (draw field)))

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

(defgeneric move-previous-char (object event &rest args)
  (:documentation "Move the cursor to the previous character.")
  (:method (object event &rest args)
    "The default method does nothing."
    nil))

(defmethod move-previous-char ((field field) event &rest args)
  "Move the cursor to the previous char in the field."
  (with-accessors ((inptr input-pointer) (dptr display-pointer) (win window)) field
    (when (> inptr 0)
      (decf inptr))
    ;; when the inptr moves left past the dptr, simultaneously decf the dptr.
    (when (< inptr dptr)
      (decf dptr))
    (draw field)))

(defgeneric move-next-char (object event &rest args)
  (:documentation "Move the cursor to the next char.")
  (:method (object event &rest args)
    "The default method does nothing."
    nil))

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
                        (incf dptr))) ))))
        (draw field))
      ;; if the char isnt graphic, do nothing.
      ;; TODO: this doesnt work with acs chars, which are keywords.
      nil))

(defun debug-print-field-buffer (object event &rest args)
  (declare (ignore event))
  (typecase object
    (field
     (with-accessors ((inbuf buffer) (inptr input-pointer) (dptr display-pointer) (win window)) object
       (when (> (length inbuf) 0)
         (clear win)
         (format win "~A ~%" (value object))
         (setf inbuf nil inptr 0 dptr 0))))
    ;; when we want to debug the whole form.
    (form
     (debug-print-field-buffer (current-element object) event)))
  (draw object))

(defun cancel (object event &rest args)
  "Associate this function with an event (key binding or button) to exit the event loop of a form or form element.

The return value is nil, emphasizing that the user has canceled the form.

The second value is a list containing the object, the event that called the exit and the args passed.

This allows to specify why the form was canceled."
  (when (or (eq (type-of object) 'form)
            (eq (type-of object) 'form-window))
    (reset-form object event))
  (throw (if (or (eq (type-of object) 'form)
                 (eq (type-of object) 'form-window)
                 (eq (type-of object) 'msgbox))
             object
             (if (parent-form object)
                 (parent-form object)
                 object))
    (values nil (list object event args))))

(defun accept (object event &rest args)
  "Associate this function with an event (key binding or button) to exit the event loop of a form or form element.

The first return value is t, emphasizing that the user has accepted the form.

The second value is a list containing the object, the event that called the exit and the args passed.

This allows to specify by which button or event the form was accepted."
  ;; if the object has a parent-form, do not throw the object,
  ;; throw its parent form, otherwise throw the object.
  (throw (if (or (eq (type-of object) 'form)
                 (eq (type-of object) 'form-window)
                 (eq (type-of object) 'msgbox))
             object
             (if (parent-form object)
                 (parent-form object)
                 object))
    (values t (list object event args))))

(defun reset-field (field event &rest args)
  "Clear the field and reset its internal buffers and pointers."
  (with-accessors ((inbuf buffer) (inptr input-pointer) (dptr display-pointer) (win window)) field
    (clear field)
    (setf inbuf nil inptr 0 dptr 0)))

(defun reset-form (object event &rest args)
  (declare (ignore event))
  (let ((form (typecase object
                (form object)
                (t (parent-form object)))))
    (loop for element in (elements form)
       do (when (and (typep element 'field) (activep element))
            (with-accessors ((inbuf buffer) (inptr input-pointer) (dptr display-pointer) (win window)) element
              (setf inbuf nil
                    inptr 0
                    dptr 0))))
    (draw form)))

(define-keymap form-map
  ;; C-a = ^A = #\soh = 1 = start of heading
  ;; exit the edit loop, return t
  (#\soh 'accept)
  ;; C-x = cancel = CAN = #\can
  ;; exit the edit loop, return nil
  (#\can 'cancel)
  ;; C-r = reset = DC2 = #\dc2
  ;; reset editable elements of the form (fields, checkboxes)
  (#\dc2 'reset-form)
  (:btab 'select-previous-element)
  (#\tab 'select-next-element))

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

(defun call-button-function (button event &rest args)
  (declare (ignore event))
  (when (callback button)
    (funcall (callback button) button event)))

(defun toggle-checkbox (checkbox event &rest args)
  (declare (ignore event))
  (setf (checkedp checkbox) (not (checkedp checkbox)))
  (draw checkbox))

;; How to automatically bind a hotkey to every button?
;; that hotkey would have to be added to the form keymap, not to that of a button.
;; that would be like a global keymap, in contrast to an elements local keymap.

(define-keymap button-map
  (#\space 'call-button-function)
  (#\newline 'call-button-function))

(define-keymap checkbox-map
  (#\space 'toggle-checkbox)
  (#\x 'toggle-checkbox))

(defun edit (object &rest args)
  "Modify a form or form element. Return t if the edit was accepted, nil of it was canceled.

The return values of the event handler are the return values of the event loop and thus
also returned by edit."
  (draw object)
  ;; since we have args passed to run-event-loop, all handler functions have to accept
  ;; a &rest args argument.
  (apply #'run-event-loop object args))
