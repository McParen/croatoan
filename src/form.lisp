(in-package :de.anvi.croatoan)

;; form
;; curses extension for programming forms
;; https://invisible-island.net/ncurses/man/form.3x.html

;; TODO: add a keyword :type, so we can have elements of different types with the same name.
;; TODO: add a type keyword to find-keymap, so we can do:
;; (find-element form 'field name)
;; (find-element form 'button name)
;; so different elements can have the same name
;; TODO: elements should be sorted into trees (or alists or hashmaps) depending on their type like blessed.js does it.
(defun find-element (form element-name &key (test #'eql) (key #'name))
  "Return from the given form the element given by its name.

The name should be a keyword, symbol or integer, the default test is eql.

If the name is a string, equal should be used as the test.

Instead of the name, another key can be provided to identify the element."
  (find element-name (elements form) :test test :key key))

;; TODO: rename this, since we're not updating anything, just moving the cursor to its position in the field.
(defgeneric update-cursor-position (object)
  (:documentation "Update the cursor position of the element of a form.")
  (:method (object)
    "The default method puts the cursor at the start position of the element."
    (setf (cursor-position (window object)) (widget-position object))
    (refresh (window object))))

(defmethod update-cursor-position ((obj button))
  "Update the cursor position of a button."
  (with-accessors ((pos content-position) (win window)) obj
    (goto win pos)
    (refresh win)))

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
          (+ (car pos) (slot-value object 'item-padding-top))
          ;; put the cursor after the [ in "[ ] ".
          (+ (cadr pos) 1 (slot-value object 'item-padding-left)))
    (refresh win)))

(defmethod update-cursor-position ((checkbox checkbox))
  "Update the cursor position of a checkbox."
  (with-accessors ((pos widget-position) (win window)) checkbox
    (move win
          (car pos)
          (1+ (cadr pos))) ;; put the cursor after the [
    (refresh win)))

;; update-cursor-position for form-window is identical to that for form.
(defmethod update-cursor-position ((form form))
  "Move the cursor to the correct position in current element of the form."
  (update-cursor-position (current-item form)))

(defgeneric draw (object)
  (:documentation "Draw objects (form, field, menu) to their associated window."))

(defmethod clear ((obj element) &key)
  "Clear the element by overwriting the underlying window with the background char.

The default background char is #\space.

If the underlying window has a background char, that will be used to
clear the window instead of #\space.

The char, attributes and colors can be set by providing the following styles:

:border
:selected-border
:background
:selected-background

If an element is inactive, for example the label, the selected style is not applied."
  (with-accessors ((x position-x) (y position-y) (ew external-width) (eh external-height) (vw visible-width) (vh visible-height)
                   (bt border-width-top) (bl border-width-left) (borderp borderp) (selectedp selectedp) (win window) (style style)) obj
    (let* ((bg-style (if selectedp
                         (getf style :selected-background)
                         (getf style :background)))
           (border-style (if selectedp
                             (getf style :selected-border)
                             (getf style :border))))
      (if borderp
          ;; first draw the border background, then the content background.
          (progn
            ;; external border rectangle
            (fill-rectangle win (apply #'make-instance 'complex-char border-style) y x eh ew)
            ;; visible bg inside the border
            (fill-rectangle win (apply #'make-instance 'complex-char bg-style) (+ y bt) (+ x bl) vh vw))
          ;; visible bg without a border
          (fill-rectangle win (apply #'make-instance 'complex-char bg-style) y x vh vw)))))

(defgeneric content-position (element))

(defmethod content-position ((element element))
  "Return the inner position of the content area of the element.

content position = widget position + border + padding"
  (with-accessors ((x position-x) (y position-y)
                   (bt border-width-top) (bl border-width-left)
                   (pl padding-left) (pt padding-top) (borderp borderp)) element
    (if borderp
        ;; if we have a border, content is shifted by border width and padding
        (list (+ y pt bt)
              (+ x pl bl))
        ;; if we have no border, content is shifted only by padding
        (list (+ y pt)
              (+ x pl)))))

(defmethod content-position ((obj menu))
  "Return the inner position of the content area of the menu.

content position = widget position + border + padding"
  (with-accessors ((x position-x) (y position-y) (pos widget-position) (borderp borderp) (tablep tablep)
                   (bt border-width-top) (bl border-width-left)
                   (pl padding-left) (pt padding-top)) obj
    (cond (tablep
           ;; the table of the menu, including the table border, is part of the "content",
           ;; it has neither a boder width nor content padding
           pos)
          (borderp
           (list (+ y pt bt)
                 (+ x pl bl)))
          (t
           (list (+ y pt)
                 (+ x pl))))))

(defgeneric visible-width (element))
(defgeneric visible-height (element))

(defmethod visible-width ((element element))
  "visible width = content width + padding"
  (with-accessors ((w width) (pl padding-left) (pr padding-right)) element
    (+ w
       pl pr)))

(defmethod visible-height ((element element))
  "visible width = content width + padding"
  (with-accessors ((h height) (pt padding-top) (pb padding-bottom)) element
    (+ h
       pt pb)))

(defun visible-dimensions (element)
  (list (visible-height element)
        (visible-width element)))

(defgeneric external-width (element))
(defgeneric external-height (element))

;; use this in grid/colomn-widths instead of width
(defmethod external-width ((obj element))
  "external-width = content width + padding + border-width"
  (with-accessors ((w width) (pl padding-left) (pr padding-right)
                   (borderp borderp) (bl border-width-left) (br border-width-right)) obj
    (+ w
       pl pr
       (if borderp (+ bl br) 0))))

(defmethod external-height ((obj element))
  "external-width = content width + padding + border-width"
  (with-accessors ((h height) (pt padding-top) (pb padding-bottom)
                   (borderp borderp) (bt border-width-top) (bb border-width-bottom)) obj
    (+ h
       pt pb
       (if borderp (+ bt bb) 0))))

(defun external-dimensions (element)
  (list (external-height element)
        (external-width element)))

(defmethod draw ((obj label))
  (with-accessors ((pos content-position) (win window) (width width)
                   (style style) (selectedp selectedp) (borderp borderp)) obj
    (clear obj)
    (let* ((text (label-text obj))
           (lines (count-lines text))
           (height (slot-value obj 'height))
           (border-style (if selectedp (getf style :selected-border) (getf style :border)))
           (fg-style (if selectedp (getf style :selected-foreground) (getf style :foreground)))
           (y 0)
           (x 0))

      ;; when the border flag is t, draw the border line
      ;; what we want next is here to use all the same arguments as add
      ;; just add "dimensions", which is the only difference between add-char and draw-rectangle
      (when borderp
        (draw-rectangle win (position-y obj) (position-x obj) (external-height obj) (external-width obj) :style border-style))

      (goto win pos (list y x))
      (if (or (and height (= height 1))
              (= lines 1))
          ;; single line label
          (add-string win (text-ellipsize text width) :style fg-style)
          ;; multiline label
          (dotimes (i (length text))
            (if (char= (char text i) #\newline)
                (progn
                  (setq y (1+ y) x 0)
                  ;; if we have more lines than the given height, stop printing new lines
                  (when (and height (>= y height))
                    (return))
                  (goto win pos (list y x)))
                (progn
                  (add-char win (char text i) :style fg-style))))))))

(defmethod draw ((button button))
  (with-accessors ((pos content-position) (y position-y) (x position-x) (name name) (title title) (win window) (selectedp selectedp) (style style) (borderp borderp)) button
    (let ((border-style (if selectedp (getf style :selected-border) (getf style :border))))
      (clear button)
      (when borderp
        (draw-rectangle win y x (external-height button) (external-width button) :style border-style))
      (goto win pos)
      (let* ((fg-style (if selectedp (getf style :selected-foreground) (getf style :foreground))))
        ;; if a title is given, display it, otherwise use the name.
        (add-string win (format nil "<~A>" (if title title name)) :style fg-style)))))

;; TODO: for a checkbox, we need a style for checked and unchecked
(defmethod draw ((checkbox checkbox))
  (with-accessors ((pos widget-position) (name name) (win window) (selectedp selectedp) (style style)
                   (checkedp checkedp)) checkbox
    (goto win pos)
    (let* ((fg-style (if selectedp (getf style :selected-foreground) (getf style :foreground))))
      (add-string win (format nil "[~A]" (if checkedp "X" "_")) :style fg-style)
      (update-cursor-position checkbox))))

(defmethod draw ((form form))
  "Draw the form by drawing the elements, then moving the cursor to the current element."
  (with-accessors ((elements elements)) form
    ;; TODO: try with mapc instead of loop.
    (loop for element in elements do
      (draw element))
    ;; after drawing the elements, reposition the cursor to the current element
    (update-cursor-position form)))

(defgeneric format-title (object &optional beg end)
  (:documentation "Make a title string for a widget."))

(defmethod format-title ((item menu-item) &optional (beg "") (end ""))
  "If neither the title nor the name are provided, print the object as a default title."
  (with-accessors ((title title) (name name)) item
    (let ((str (cond ((and title
                           (stringp title))
                      title)
                     ((and name
                           (symbolp name))
                      (symbol-name name))
                     (t
                      (prin1-to-string item)))))
      str)))

(defmethod format-title ((win window) &optional (beg "") (end ""))
  "If the title string is provided, use the title.

If title is t, use the name. If title is nil, return an empty string."
  (with-accessors ((title title) (name name)) win
    (let ((str (cond ((and title
                           (stringp title))
                      title)
                     ((and (eq title t)
                           name
                           (symbolp name))
                      (symbol-name name))
                     ((null title)
                      ""))))
      (concatenate 'string beg str end))))

;; TODO 201025 add align: left right center
;; draw the background window according to the style sheet
;; styles: title, border
(defun add-title (win &optional title)
  "Draw a title to the first line of a window.

Usually, this will be an extended-window with a border and the title on the top border.

When title is t instead of a title string, display the symbol name of the widget."
  (add-string win
              (if title
                  title
                  (format-title win "| " " |"))
              :y 0 :x 2 :style (getf (slot-value win 'style) :title)))

(defmethod draw ((win extended-window))
  "Draw the background window, and the title and the border if they are given."
  ;; update cursor position only refreshes the window associated with the form, which is the sub-window
  ;; in order to see the border, we have to touch and refresh the parent border window.
  ;; refreshing the parent window has to be done before refreshing the cursor position in the sub
  ;; or the cursor will be moved to 0,0 of the parent window.

  ;; TODO 201101 check ncurses:wsyncup, maybe we can avoid the explicit touch
  ;; TODO 210117 check whether we can touch in the refresh routine for a subwin
  (touch win)

  ;; TODO do we have to refresh here?
  (refresh win)

  ;; in multiple inheritance, an extended-window should be the first superclass
  ;; so we can first draw the border and title here and then
  ;; call-next-method the second class here to draw the contents
  ;; provided by the next superclass (for example menu or form)
  ;; If there was a draw method for windows, it would be called before going to the next superclass
  ;; but we do not have a draw method for simple windows.
  (when (next-method-p)
    (call-next-method)))

(defmethod draw ((win form-window))
  "Draw the the border and the title, then the form."
  (with-accessors ((title title) (borderp borderp)) win
    ;; we can add a title even when there is no border
    ;; If a title is given as a string, it is displayed
    ;; If the title is t, the name is displayed as the title.
    ;; If the title is nil or the border is nil, no title is displayed.
    ;; we do not want to draw a title for menu-windows by drawing them in extended window
    ;; a title is only added for form-window
    (when (and borderp title)
      (add-title win))

    ;; first call next method to draw the extended window
    ;; then from the decorated window call-next-method to draw the form contents
    (when (next-method-p)
      (call-next-method))))

(defmethod select-previous-item ((form form))
  "Select the previous element in a form's element list."
  (with-accessors ((current-item current-item)) form
    (setf (selectedp current-item) nil)
    ;; call parent method for collection
    (call-next-method)
    ;; ignore inactive elements like labels.
    (if (activep current-item)
        (setf (selectedp current-item) t)
        (when (previous-item-p form)
          (select-previous-item form)))))

(defun select-previous-element (form)
  (select-previous-item form)
  (draw form))

(defmethod select-next-item ((form form))
  "Select the next element in a form's element list."
  (with-accessors ((current-item current-item)) form
    (setf (selectedp current-item) nil)
    ;; call parent method for collection
    (call-next-method)
    ;; ignore inactive elements like labels.
    (if (activep current-item)
        (setf (selectedp current-item) t)
        (when (next-item-p form)
          (select-next-item form)))))

(defun select-next-element (form)
  (select-next-item form)
  (draw form))

(defgeneric move-previous-char (object)
  (:documentation "Move the cursor to the previous character.")
  (:method (object)
    "The default method does nothing."
    nil))

(defgeneric move-next-char (object)
  (:documentation "Move the cursor to the next char.")
  (:method (object)
    "The default method does nothing."
    nil))

(defun get-catch-tag (object)
  "Sets the proper catch tag for exiting the event loop.

If the object is an element in a form, use the form object as the catch tag.

If the object is used outside a form, use the object itself as the catch tag."
  (if (typep object 'form)
      ;; the form is the tag
      object
      ;; if the object is a form element
      (if (and (parent object)
               (typep (parent object) 'form))
          ;; if the element has a parent form,
          ;; the parent form is the tag
          (parent object)
          ;; the object doesnt have a parent form,
          ;; the event loop uses the element directly
          object)))

(defun accept (object)
  "Exit the event loop of a form or form element.

The first return value is t, emphasizing that the user has accepted the form.

The element name is returned as a second value.

Bind this function to an event (key binding or button)."
  (throw (get-catch-tag object)
    (values t
            (name object))))

(defun cancel (object)
  "Exit the event loop of a form or form element.

The return value is nil, emphasizing that the user has canceled the form.

The element name is returned as a second value.

As a consequence of the cancel, all elements are reset and the input content is discarded.

Bind this function to an event (key binding or button)."
  ;; TODO: should the reset upon a cancel be done by the routine or explicitely by the user?
  ;; TODO: if we cancel with a button, object will be a button, so we wont cancel the form.
  ;; so check that we have a form
  (when (typep object 'form)
    (reset-form object))
  (throw (get-catch-tag object)
    (values nil
            (name object))))

(defun return-element-value (object)
  "Exit the event loop and return the value of the (current) element.

The element name is returned as a second value.

Instead of accept or cancel, which return t or nil, this function
allows to exit the form event loop and return any value."
  (throw (get-catch-tag object)
    (if (typep object 'form)
        ;; form
        (values (value (current-item object))
                (name (current-item object)))
        ;; form element
        (values (value object)
                (name object)))))

(defun return-form-values (object)
  "Return an alist with element names as keys and element values as values.

It is supposed to resemble GET params fname=John&lname=Doe from html forms.

Bind this to an event or element to exit the event loop of a form."
  (throw (if (typep object 'form)
             object
             (parent object))
    (loop for element in (elements (if (typep object 'form)
                                       object
                                       (parent object)))
          when (activep element)
          collect (cons (name element) (value element)))))

(defgeneric reset (object)
  (:documentation "Clear user-editable form elements and reset its internal buffers and pointers."))

(defmethod reset ((form form))
  (dolist (el (elements form))
    (when (and (or (typep el 'field)
                   (typep el 'textarea))
               (activep el))
      (reset el)))
  (draw form))

(defun reset-form (object)
  "Reset a parent form from an element callback, for example a button."
  (reset (parent object)))

(define-keymap form-map
  ;; C-a = ^A = #\soh = 1 = start of heading
  ;; exit the edit loop, return t
  (#\soh 'accept)
  ;; C-x = cancel = CAN = #\can
  ;; exit the edit loop, return nil
  (#\can 'cancel)
  ;; C-r = reset = DC2 = #\dc2
  ;; reset editable elements of the form (fields, checkboxes)
  (#\dc2 'reset)
  (:btab 'select-previous-element)
  (#\tab 'select-next-element))

(defun call-button-function (button event)
  (with-accessors ((callback callback)) button
    (when callback
      (apply-handler callback button event nil))))

(defun toggle-checkbox (checkbox)
  (setf (checkedp checkbox) (not (checkedp checkbox)))
  (draw checkbox))

;; How to automatically bind a hotkey to every button?
;; that hotkey would have to be added to the form keymap, not to that of a button.
;; that would be like a global keymap, in contrast to an elements local keymap.
;; TODO: how to make :left and :right jump to the next element without binding those keys in the form keymap?
;; how are we going to use :up and :down bound t the form when we later have a multiline-field?

(define-keymap button-map
  (#\space 'call-button-function)
  (#\newline 'call-button-function))

(define-keymap checkbox-map
  (#\space 'toggle-checkbox)
  (#\x 'toggle-checkbox))

(defun edit (object)
  "Modify a form or form element. Return t if the edit was accepted, nil of it was canceled.

The return values of the event handler are the return values of the event loop and thus
also returned by edit."
  (draw object)
  (run-event-loop object))
