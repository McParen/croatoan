(in-package :de.anvi.croatoan)

;; (defclass name (superclasses) (slots) class-options)

;;; data types

;; will be abbreviated as "xchar" or "xch" in function parameters.
(defclass complex-char ()
  ((simple-char
    :initarg       :simple-char
    :initform      nil
    :type          (or null integer character keyword)
    :accessor      simple-char
    :documentation "Lisp primitive character type, like #\a.")

   (attributes
    :initarg       :attributes
    :initform      '()
    :type          (or null cons)
    :accessor      attributes
    :documentation "List of keywords denoting attributes.")

   (color-pair
    :initarg       :color-pair
    :initform      nil
    :type          (or null cons)
    :accessor      color-pair
    :documentation "Two element list of keywords denoting a foreground and background color of the char."))

  (:documentation
   "A complex char consists of a simple char, a list of attribute keywords and a pair of color keywords."))

(defclass complex-string ()
  ((complex-char-array
    :initarg       :complex-char-array
    :initform      (make-array 0 :element-type 'complex-char :fill-pointer 0 :adjustable t)
    :type          vector
    :accessor      complex-char-array
    :documentation "Lisp primitive string type."))

  (:documentation
   "A complex string consists of an array of complex characters."))

;; TODO: what to do when there is no init string, when we start empty and build the string char by char,
;; for example when extracting a complex string.
(defmethod initialize-instance :after ((cstr complex-string) &key string attributes color-pair)
  (with-slots (complex-char-array) cstr
    (when string
      (loop for char across string
         do (vector-push-extend
             (make-instance 'complex-char :simple-char char :attributes attributes :color-pair color-pair)
             complex-char-array)))))

(defclass keymap ()
  ((bindings
    :initarg       :bindings
    :initform      nil
    :type          (or null cons)
    :accessor      bindings
    :documentation "Alist of events and handler functions."))

  (:documentation  "A keymap contains an alist of events as keys and event handlers or chained keymaps as values."))

;; initialize instance takes a plist initarg and converts it to an alist.
(defmethod initialize-instance :after ((keymap keymap) &key bindings-plist)
  (with-slots (bindings) keymap
    (when (and (null bindings) bindings-plist)
      (setf bindings (plist2alist bindings-plist)))))

(defclass window (fundamental-character-input-stream fundamental-character-output-stream)
  ((location
    :initarg       :location
    :initform      '(0 0)
    :type          cons
    :documentation "The (y=row x=column) coordinate of the top left corner of the window.")

   (width
    :initarg       :width
    :initform      nil
    :type          (or null integer)
    :documentation "The width (second, horizontal, x dimension) of the window.")

   (height
    :initarg       :height
    :initform      nil
    :type          (or null integer)
    :documentation "The height (first, vertical, y dimension) of the window.")

   ;; has to be a 2el-list so we can use 1 arg with setf.
   (cursor-position
    :type          cons
    :documentation "The current cursor position coordinates in the form (y x).")

   (input-blocking
    :initarg       :input-blocking
    :initform      t
    :type          (or boolean integer)
    :documentation "Input mode: blocking (t), non-blocking (nil) or blocking duration in (positive integer) miliseconds.")

   (frame-rate
    :initarg       :frame-rate
    :initform      nil
    :type          (or null integer)
    :accessor      frame-rate
    :documentation "Set the frame rate in fps (frames per second). When input-blocking is nil, sleep for 1/frame-rate seconds between event loop cycles. Has the same effect as setting input-blocking duration, and should thus not be used simultaneously.")

   (function-keys-enabled-p
    :initarg       :enable-function-keys
    :initform      nil
    :type          boolean
    :documentation "Enable (t) or disable (nil) support for function keys.")

   (scrolling-enabled-p
    :initarg       :enable-scrolling
    :initform      nil
    :type          boolean
    :documentation "Enable (t) or disable (nil) support for window scrolling.")

   (scrolling-region
    :initarg       :scrolling-region
    :initform      nil
    :type          (or null cons)
    :documentation "When scrolling is enabled, only scroll the window region from line y1 to y2 given as the list (y1 y2).")

   ;; IC / INS / Insert / Einfg key
   ;; this will only apply to lisps standard output functions.
   ;; ncurses functions add-char and insert-char can be used explicitely.
   (insert-mode-p
    :initarg       :insert-mode
    :initform      nil
    :type          boolean
    :accessor      insert-mode-p
    :documentation "Printing a new char will insert (t) it before the character under the cursor instead of overwriting it (nil, default).")

   ;; we need instance-local bindings so we dont have to create a keymap for every window and every program
   (bindings
    :initarg       :bindings
    :initform      nil
    :type          (or null cons)
    :accessor      bindings
    :documentation
    "Alist of events (characters, keywords or integers) as keys and handler functions as values. 
    Used by the run-event-loop function.")

   ;; if using an instance-local binding isnt sufficient, we can create a keymap and reference it in the object.
   (keymap
    :initarg       :keymap
    :initform      nil
    :type          (or null symbol keyword keymap)
    :accessor      keymap
    :documentation "Keymap containing the key bindings to be used by run-event-loop instead of the object's own bindings.")

   (background
    :initarg       :background
    :initform      nil
    :type          (or null complex-char)
    :documentation
    "Sets a complex char with its attributes and colors as the default style of unrendered text and empty cells of the window. 
    Complex chars with pre-existing attributes and colors are not changed.")

   (attributes
    :initarg       :attributes
    :initform      nil
    :type          (or null cons)
    :documentation "A list of keywords denoting attributes of new characters added to a window.")

   (color-pair
    :initarg       :color-pair
    :initform      nil
    :type          (or null cons)
    :documentation "A two element list of keywords denoting the foreground and background color of new characters added to a window.")

   (draw-border-p
    :initarg       :draw-border
    :initform      nil
    :type          boolean
    :reader        draw-border-p
    :documentation "Draw (t) or don't draw (nil, default) an initial border around a window. Not redrawn automatically.")

   (stackedp
    :initarg       :stacked
    :initform      nil
    :type          boolean
    :documentation "If stacked is t, the window is added to the global window stack, so overlapping windows can be refreshed in the stacking order.")

   (visiblep
    :initarg       :visible
    :initform      t
    :type          boolean
    :accessor      visiblep
    :documentation "If visible is nil, do not refresh the stacked window when refreshing the window stack.")

   (winptr
    :initform      nil
    :reader        winptr
    :documentation "Pointer to a C/ncurses WINDOW structure after the window is created."))

  (:documentation "A curses window object as returned by newwin."))

;; also see source/panel.lisp
(defparameter *window-stack* nil)

;; ":stacked t" or "(setf (stackedp win) t)" adds a new window to the stack
;; "(setf (hidden win) t)" prevents the window from being refreshed.
(defmethod stackedp ((win window))
  (slot-value win 'stackedp))
(defmethod (setf stackedp) (stackedp (win window))
  (if stackedp
      ;; t:   check if in stack, if not, add to stack, if yes, error
      (if (member win *window-stack* :test #'eq)
          (error "setf stackedp t: window already on stack")
          (progn
            (push win *window-stack*)
            (setf (slot-value win 'stackedp) t)))
      ;; nil: check if in stack, if yes, remove from stack, if not, error
      (if (member win *window-stack* :test #'eq)
          (progn
            (setf *window-stack* (remove win *window-stack* :test #'eq))
            (setf (slot-value win 'stackedp) t))
          (error "setf stackedp nil: window not on stack"))))

(defclass screen (window)
  ((colors-enabled-p
    :initarg       :enable-colors
    :initform      nil
    :type          boolean
    :documentation "Enable (t) or disable (nil) display of colors, if the terminal supports it.")

   (use-default-colors-p
    :initarg       :use-default-colors
    :initform      nil
    :type          boolean
    :documentation "Use (t) the default colors of the terminal, instead of the ncurses default pair white on black.")

   (cursor-visible-p
    :initarg       :cursor-visible
    :initform      t
    :type          boolean
    :documentation "Enable (t) or disable (nil) the display of a visible cursor.")

   (input-echoing-p
    :initarg       :input-echoing
    :initform      t
    :type          boolean
    :documentation "Enable (t) or disable (nil) echoing of chars during keyboard input.")

   (input-buffering-p
    :initarg       :input-buffering
    :initform      nil
    :type          boolean
    :documentation
    "Set whether typed characters will be returned immediately when they are typed (nil, default) 
    or buffered until Return is typed (t).")

   (process-control-chars-p
    :initarg       :process-control-chars
    :initform      t
    :type          boolean
    :documentation
    "If input-buffering is nil, set whether some control characters like ^C, ^S, ^Q, ^D will be processed (t, default) 
    or passed directly to the program (nil).
    When input-buffering is t, control chars are always processed and this option has no effect.")

   (newline-translation-enabled-p
    :initarg       :enable-newline-translation
    :initform      t
    :type          boolean
    :documentation
    "If t (default), the #\return character (CR ^M \r) is automatically translated to newline (NL) on input, 
     and NL is translated to CR LF on output. 
     NL is the standard, system independent, portable way to end a line.
     It can be either #\linefeed (LF ^J \n) on Linux, carriage #\return on MacOS, CRLF \r\n on Windows.
     Setting newline translation to nil is necessary to be able to detect the #\return key.")

   (closed-p
    :type          boolean
    :documentation "Check whether the screen has been closed, without a subsequent call to refresh to reactivate it."))

  (:documentation "Represents the main window created upon screen initialisation."))

(defclass sub-window (window)
  ((parent
    :initarg       :parent
    :initform      nil
    :type          (or null window)
    :documentation "The parent window which will contain the sub-window.")

   (relativep
    :initarg       :relative
    :initform      nil
    :type          boolean
    :documentation "The location of the sub-window is relative to the parent window (t) or to the screen (nil, default).")

   (source-location
    :initarg       :source-location
    :initform      nil
    :type          (or null cons)
    :documentation "Location (y x) of the area of the parent window, which is mapped to the subwindow. By default it is identical to the location of the subwindow."))

  (:documentation  "A sub-window shares the memory and the display with and has to be contained within a parent window."))

(defclass decorated-window (window)
  ((sub-window
    :initarg       :sub-window
    :initform      nil
    :type          (or null sub-window)
    :reader        sub-window
    :documentation "Active content window, for example for menu items.")

   (title
    :initarg       :title
    :initform      nil
    :accessor      title
    :type          (or boolean string)
    :documentation "If t, a title of the window will be displayed over the top border."))

  (:documentation  "A decorated-window is a window consisting of a background window for a border, a title and a content sub-window."))

(defclass menu-item ()
  ((name
    :initarg       :name
    :initform      nil
    :reader        name
    :type          (or null string symbol)
    :documentation "Short name of a menu item displayed in the menu.")

   (checkedp
    :initarg       :checked
    :initform      nil
    :accessor      checkedp
    :type          boolean
    :documentation "t if the item has been checked, nil if it hasn't.")

   (value
    :initarg       :value
    :initform      nil
    :reader        value
    :type          (or symbol string menu menu-window function)
    :documentation "Object associated with and returned upon selection of the item."))
  
  (:documentation  "A menu consists of a list of menu items."))

;; default size of ncurses menus is 16 rows, 1 col.
(defclass menu ()
  ((name
    :initarg       :name
    :initform      nil
    :reader        name
    :type          (or null string symbol)
    :documentation "Name of the menu. (For example for setting the menu-window title, if title is t.)")

   (items
    :initarg       :items
    :initform      nil
    :accessor      items
    :type          (or null cons)
    :documentation "List of menu items. Item types can be strings, symbols, other menus or callback functions.")

   (menu-type
    :initarg       :menu-type
    :initform      :selection
    :accessor      menu-type
    :type          keyword
    :documentation "Types of menus: :selection (default, can contain strings, symbols, menus) or :checklist.")

   (current-item-number
    :initform      0
    :accessor      current-item-number
    :type          integer
    :documentation "Number of the currently selected item.")

   (current-item
    :initform      nil
    :type          (or null string symbol menu-item)
    :accessor      current-item
    :documentation "Pointer to the currently selected item object. The first item is initialized as the current item.")
   
   (current-item-mark
    :initarg       :current-item-mark
    :initform      ""
    :reader        current-item-mark
    :type          string
    :documentation "A string prefixed to the current item in the menu.")

   (cyclic-selection-p
    :initarg       :cyclic-selection
    :initform      nil
    :accessor      cyclic-selection-p
    :type          boolean
    :documentation "Wrap around when the end of a non-scrolled menu is reached.")

   (max-item-length
    :initarg       :max-item-length
    :initform      15
    :accessor      max-item-length
    :type          integer
    :documentation "Max number of characters displayed for a single item.")

   (layout
    :initarg       :layout
    :initform      nil
    :accessor      layout
    :type          (or null cons)
    :documentation "Layout (no-of-rows no-of-columns) of the items in the menu. If nil, we have a vertical list.")

   (scrolled-layout
    :initarg       :scrolled-layout
    :initform      nil
    :accessor      scrolled-layout
    :type          (or null cons)
    :documentation "Layout (no-of-rows no-of-columns) of the menu items actually displayed on screen.")

   (scrolled-region-start
    :initform      (list 0 0)
    :accessor      scrolled-region-start
    :type          (or null cons)
    :documentation "A 2-element list tracking the starting row/y and column/x of the displayed menu region.")

   ;; we need instance-local bindings so we dont have to create a keymap for every window and every program
   (bindings
    :initarg       :bindings
    :initform      nil
    :type          (or null cons)
    :accessor      bindings
    :documentation
    "Alist of events (characters, keywords or integers) as keys and handler functions as values. 
    Used by the run-event-loop function.")

   ;; if using an instance-local binding isnt sufficient, we can create a keymap and reference it in the object.
   (keymap
    :initarg       :keymap
    :initform      'menu-map
    :type          (or null symbol keyword keymap)
    :accessor      keymap
    :documentation "Keymap containing the key bindings to be used by run-event-loop instead of the object's own bindings.")

   (window
    :initarg       :window
    :initform      nil
    :type          (or null window)
    :accessor      window
    :documentation "Window created separately and then associated with the menu."))

  (:documentation  "A menu is a list of items that can be selected by the user."))

(defclass menu-window (menu decorated-window)
  ()
  (:documentation "A menu-window is decorated-window providing a list of items to be selected by the user."))

;; init for menus which aren't menu windows
(defmethod initialize-instance :after ((menu menu) &key)
  (with-slots (type items current-item layout) menu
    ;; Convert strings and symbols to item objects
    (setf items (mapcar (lambda (x)
                          (if (typep x 'menu-item)
                              ;; if an item object is given
                              x
                              ;; if we have strings, symbols or menus, convert them to menu-items
                              (make-instance 'menu-item
                                             :name (typecase x
                                                     (string x)
                                                     (symbol (symbol-name x))
                                                     (menu-window (name x))
                                                     (menu (name x)))
                                             :value x)))
                        ;; apply the function to the init arg passed to make-instance.
                        items))

    ;; Initialize the current item as the first item from the items list.
    (setf current-item (car items))

    ;; if the layout wasnt passed as an argument, initialize it as a single one-column menu.
    (unless layout (setf layout (list (length items) 1))) ))

(defmethod initialize-instance :after ((win menu-window) &key)
  (with-slots (winptr items type height width location sub-window draw-border-p layout scrolled-layout max-item-length
                      current-item-mark color-pair) win
    ;; only for menu windows
    (when (eq (type-of win) 'menu-window)
      (let ((padding (if draw-border-p 1 0)))
        ;; if no layout was given, use a vertical list (n 1)
        (unless layout (setf layout (list (length items) 1)))
        ;; if height and width are not given as initargs, they will be calculated,
        ;; according to no of rows +/- border, and _not_ maximized like normal windows.
        (unless height (setf height (+ (* 2 padding) (car (or scrolled-layout layout)))))
        (unless width  (setf width  (+ (* 2 padding) (* (cadr (or scrolled-layout layout))
                                                        (+ (length current-item-mark) max-item-length)))))
        (setf winptr (%newwin height width (car location) (cadr location)))
        (setf sub-window
              (make-instance 'sub-window
                             :parent win :height (car (or scrolled-layout layout))
                             :width (* (cadr (or scrolled-layout layout)) (+ (length current-item-mark) max-item-length))
                             :location (list padding padding) :relative t))
        (when color-pair
          (setf (color-pair win) color-pair
                ;; we need to set the window color pair for the :reverse attribute to work
                (color-pair sub-window) color-pair
                ;; we also need the background to have the whole item width colored
                (background win)        (make-instance 'complex-char :color-pair color-pair)
                (background sub-window) (make-instance 'complex-char :color-pair color-pair))) ))))

(defclass dialog-window (menu-window)
  ;; this has to be a pad, so we can scroll it if the message is large.
  ;; TODO: check how scrolling would work with a subwindow.
  ((message-pad
    :initform      nil
    :type          (or null pad)
    :reader        message-pad
    :documentation "Passive content window, for example for menu descriptions.")

   ;; check how message is called in the dialog program.
   (message-text
    :initarg       :message-text
    :initform      nil
    :accessor      message-text
    :type          (or null string)
    :documentation "Optional message text to describe the choices in the menu below.")
   ;; make shorter slot names
   (message-height
    :initarg       :message-height
    :initform      0
    :reader        message-height
    :type          integer
    :documentation "Max number of lines reserved for the optional message text.")

   (message-pad-coordinates
    :initform      nil
    :reader        message-pad-coordinates
    :type          (or null cons)
    :documentation "List of four coordinates where to refresh/display the message pad: min-y min-x max-y max-x."))

  (:documentation  "A dialog is a decorated menu with a title, a message and items."))

(defmethod initialize-instance :after ((win dialog-window) &key center)
  (with-slots (winptr items height width location sub-window draw-border-p layout max-item-length current-item-mark color-pair
                      message-pad message-text message-height message-pad-coordinates) win
    ;; only for dialog windows
    (when (eq (type-of win) 'dialog-window)

      ;; dialog windows should always have borders
      (let ((padding 1))
        ;; if no layout was given, use a vertical list (n 1)
        (unless layout (setf layout (list (length items) 1)))

        ;; if height and width are not given as initargs, they will be calculated,
        ;; according to no of rows +/- border, and _not_ maximized like normal windows.
        (unless height (setf height (+ 2 message-height (* 2 padding) (car layout))))
        (unless width (setf width (+ (* 2 padding) (* (cadr layout) (+ (length current-item-mark) max-item-length)))))

        ;; if the key center was given, calculate location automatically, even if it was explicitely given.
        (when center (setf location (list (- (round (/ %LINES 2)) (round (/ height 2)))
                                          (- (round (/ %COLS  2)) (round (/ width  2))))))

        (setf winptr (%newwin height width (car location) (cadr location)))
        (setf sub-window
              (make-instance 'sub-window
                             :parent win :height (car layout)
                             :width (* (cadr layout) (+ (length current-item-mark) max-item-length))
                             :location (list (+ 2 message-height padding) (+ padding 1)) :relative t))

        ;; if there is space reserved for a message, and the message is provided,
        ;; initialize a pad and set the background color.
        (when (and message-text (> message-height 0))
          (setf message-pad (make-instance 'pad :height message-height :width (- width 4)))
          (setf message-pad-coordinates
                (list (+ 2 (car location)) ;screen-min-y
                      (+ 2 (cadr location)) ;screen-min-x
                      (+ (+ 2 (car location)) message-height) ;screen-max-y
                      (+ (+ 2 (cadr location) (- width 4))))) ;screen-max-x
          (when color-pair
            (setf (background message-pad) (make-instance 'complex-char :color-pair color-pair)))
          (format message-pad message-text))

        ;; TODO: do this once for all menus and dialogs, at the moment it is duplicated
        (when color-pair
          (setf (color-pair win) color-pair
                ;; we need to set the window color pair for the :reverse attribute to work
                (color-pair sub-window) color-pair
                ;; we also need the background to have the whole item width colored
                (background win)        (make-instance 'complex-char :color-pair color-pair)
                (background sub-window) (make-instance 'complex-char :color-pair color-pair ))) ))))

(defclass element ()
  ((name
    :initarg       :name
    :initform      nil
    :reader        name
    :type          (or null symbol keyword string)
    :documentation "Optional unique name by which the element can be identified and accessed.")

   (title
    :initarg       :title
    :initform      nil
    :accessor      title
    :type          (or null string)
    :documentation "Title of the element to be displayed at an location depending on the element type.")

   (location
    :initarg       :location
    :initform      nil
    :type          (or null cons)
    :accessor      location
    :documentation "A two-element list (y=row x=column) containing the coordinate of the top left corner of the element.")

   (bindings
    :initarg       :bindings
    :initform      nil
    :type          (or null cons)
    :accessor      bindings
    :documentation
    "Alist of events (characters, keywords or integers) as keys and handler functions as values. 
    Used by the run-event-loop function.")

   (keymap
    :initarg       :keymap
    :initform      nil
    :type          (or null symbol keyword keymap)
    :accessor      keymap
    :documentation "Keymap containing the key bindings to be used by run-event-loop instead of the object's own bindings.")
   
   ;; we need this to draw selected and other elements with different styles.
   ;; this has to be toggled at the same time as current-element of a form
   (selectedp
    :initform      nil
    :type          boolean
    :accessor      selectedp
    :documentation "Flag denoting whether the element is currently selected in a form.")

   (activep
    :initform      t
    :type          boolean
    :accessor      activep
    :documentation "If t (default), the element can be selected. Used to prevent labels from being selected.")

   (parent-form
    :initform      nil
    :type          (or null form)
    :accessor      parent-form
    :documentation "Parent form of the element. Added to every element upon the initialization of the form.")

   ;; elements do not necessarily have to have an associated window, only when they are used stand-alone.
   ;; when they are part of a form, we can reference the window associated with the form.
   (window
    :initarg       :window
    :initform      nil
    :type          (or null window)
    :documentation "If the element is not part of a form, a window can also be associated with the stand-alone element."))

  (:documentation "An element of a form, like a field or button."))

;; (window (if (window field) (window field) (window (parent-form field))))
(defmethod window ((element element))
  "Return the window associated with an element, which can optionally be part of a form.

If there is no window asociated with the element, return the window associated with the parent form."
  (with-slots (window parent-form) element
    (if window
        window
        (if parent-form
            (if (slot-value parent-form 'window)
                (slot-value parent-form 'window)
                (error "(window element) ERROR: No window is associated with the parent form."))
            (error "(window element) ERROR: Neither a window nor a parent form are associated with the element.")))))

(defmethod (setf window) (window (element element))
  (setf (slot-value element 'window) window))

(defmethod style ((element element))
  "If the element's style slot is empty, check whether a default style has been defined in the parent form."
  (with-slots (style parent-form) element
    (if style
        style
        (if (slot-value parent-form 'style)
            ;; get the default element style from the form style slot.
            (getf (slot-value parent-form 'style) (type-of element))
            nil))))

(defmethod (setf style) (style (element element))
  (setf (slot-value element 'style) style))

(defclass label (element)
  ((reference
    :initarg       :reference
    :initform      nil
    :accessor      reference
    :type          (or null element)
    :documentation "If a reference element is specified, the element's title will be displayed instead of the label's.")
   
   (width
    :initarg       :width
    :initform      nil
    :type          (or null integer)
    :accessor      width
    :documentation "The width of the label.")

   (style
    :initarg       :style
    :initform      nil
    :type          (or null cons)
    :documentation "")

   (activep
    :initform      nil
    :documentation "Labels are by default not active and can not be selected when cycling through the elements."))
  
  (:documentation "A string displayed at the specified location."))

(defclass button (element)
  ((callback
    :initarg       :callback
    :initform      nil
    :accessor      callback
    :type          (or null symbol function)
    :documentation "Callback function called when the button is activated.")

   (style
    :initarg       :style
    :initform      nil
    :type          (or null cons)
    :documentation "A plist of two complex-chars (or nil): :foreground and :selected-foreground.")

   (bindings
    :initarg       :bindings
    :initform      nil
    :type          (or null cons)
    :accessor      bindings
    :documentation
    "Alist of events (characters, keywords or integers) as keys and handler functions as values. 
    Used by the run-event-loop function.")

   (keymap
    :initarg       :keymap
    :initform      'button-map
    :type          (or null symbol keyword keymap)
    :accessor      keymap
    :documentation "Keymap containing the key bindings to be used by run-event-loop instead of the object's own bindings."))

  (:documentation "An element that can call a function by pressing enter (or in future, with a mouse click)."))

(defclass field (element)
  ((width
    :initarg       :width
    :initform      nil
    :type          (or null integer)
    :accessor      width
    :documentation "The width of the field. The default buffer length is equal the width.")

   (style
    :initarg       :style
    :initform      nil
    :type          (or null cons)
    :documentation "A plist of four complex-chars (or nil): :foreground, :background, :selected-foreground, :selected-background.")

   (bindings
    :initarg       :bindings
    :initform      nil
    :type          (or null cons)
    :accessor      bindings
    :documentation
    "Alist of events (characters, keywords or integers) as keys and handler functions as values. 
    Used by the run-event-loop function.")

   (keymap
    :initarg       :keymap
    :initform      'field-map
    :type          (or null symbol keyword keymap)
    :accessor      keymap
    :documentation "Keymap containing the key bindings to be used by run-event-loop instead of the object's own bindings.")

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

  (:documentation "A field is an editable part of the screen for user input. Can be part of a form."))

(defmethod initialize-instance :after ((field field) &key)
  (with-slots (max-buffer-length width bindings keymap) field
    ;; If unspecified, the default max-buffer-length should be equal to the visible field width.
    (unless max-buffer-length
      (setf max-buffer-length width))))

(defmethod value ((field field))
  (coerce (reverse (slot-value field 'buffer)) 'string))

(defmethod (setf value) (new-value (field field))
  (setf (slot-value field 'buffer) (reverse (coerce new-value 'list))))

(defclass form ()
  ((elements
    :initarg       :elements
    ;; Make sure we already have fields when we initialize a form.
    :initform      nil
    :type          (or null cons)
    :accessor      elements
    :documentation "List of elements. The first element will be initialized as the current element.")

   ;; if init is 0, what if we have no fields yet?
   (current-element-number
    :initform      0
    :type          integer
    :accessor      current-element-number
    :documentation "Number of the currently selected element.")

   ;; has to be updated every time the current element number is updated.
   (current-element
    :initform      nil
    :type          (or null field button)
    :accessor      current-element
    :documentation "Currently selected element object.")

   (style
    :initarg       :style
    :initform      nil
    :type          (or null cons)
    :accessor      style
    :documentation "A plist of default styles for each form element type.")

   (bindings
    :initarg       :bindings
    :initform      nil
    :type          (or null cons)
    :accessor      bindings
    :documentation
    "Alist of events (characters, keywords or integers) as keys and handler functions as values. 
    Used by the run-event-loop function.")

   (keymap
    :initarg       :keymap
    :initform      'form-map
    :type          (or null symbol keyword keymap)
    :accessor      keymap
    :documentation "Keymap containing the key bindings to be used by run-event-loop instead of the object's own bindings.")

   (window
    :initarg       :window
    :initform      nil
    :type          (or null window)
    :accessor      window
    :documentation "Window created separately and then associated with the form."))

  (:documentation "A form is a list of fields."))

(defmethod initialize-instance :after ((form form) &key)
  (with-slots (current-element elements bindings keymap) form
    ;; Initialize the current element as the first element from the passed elements list.
    ;; we have to set the current element before we can change it with select-previous-element and select-next-element
    (setf current-element (car elements))
    ;; set the selected option of the initial current element.
    (setf (slot-value current-element 'selectedp) t)
    ;; set the parent form slot of every field.
    (if elements
        (loop for element in elements
           do (setf (slot-value element 'parent-form) form))
        ;; if a list of elements was not passed, signal an error.
        (error "A list of elements is required to initialize a form."))))

(defclass form-window (form decorated-window)
  ()
  (:documentation ""))

(defmethod initialize-instance :after ((win form-window) &key)
  (with-slots (winptr height width location sub-window draw-border-p) win
    ;; only for form windows
    (when (eq (type-of win) 'form-window)
      (setf winptr (%newwin height width (car location) (cadr location)))
      (setf sub-window
            (make-instance 'sub-window :parent win :height (- height 2) :width (- width 2) :location (list 1 1) :relative t)))))

;; if a window-location is given during make-instance, it can be simply ignored.
;; or we can check for location and signal an error.
(defclass pad (window)
  ()
  (:documentation "A pad is a window without a specified location on screen, which is specified dynamically during refresh."))

(defmethod initialize-instance :after ((win pad) &key)
  (with-slots (winptr height width) win
    ;; just for a pad window
    (when (eq (type-of win) 'pad)
      (setf winptr (%newpad height width)))))

(defclass sub-pad (pad)
  ((parent
    :initarg       :parent
    :initform      nil
    :type          (or null pad)
    :documentation "The parent pad which will contain the sub-pad."))
  (:documentation  "A sub-pad shares the memory and the display with a parent pad and has to be contained within it."))

;; sub-pads always use locations relative to parent pads
(defmethod initialize-instance :after ((win sub-pad) &key)
  (with-slots (winptr parent height width location) win
    ;; just for a sub-pad window
    (when (eq (type-of win) 'sub-pad)
      (setf winptr (%subpad (slot-value parent 'winptr) height width (car location) (cadr location))))))

#|
;; this will be called for both window and screen.
;; create a curses window when an instance is created.
(defmethod initialize-instance :after ((win window) &key)
  (with-slots (winptr cursor-visible-p colors-enabled-p input-echoing-p input-reading 
                      input-blocking function-keys-enabled-p scrolling-enabled-p height width location) win

    ;; different initialisations depending on the window type.
    ;; %initscr initializes a screen, %newwin initializes a window.
    ;; TODO: subwin, derwin, pad...
    ;; CAUTION: these initializations have to be done _before_ any other commands.
    ;; because of that we cant use call-next-method.
    (case (type-of win)
      ;; a screen is initialized when we start ncurses.
      (screen (progn
                (setf winptr (%initscr))
                (when colors-enabled-p (%start-color))
                (if input-echoing-p (%echo) (%noecho))
                (set-input-reading winptr input-reading)
                (set-cursor-visibility cursor-visible-p))) ;kernel.lisp
      ;; a window is initialized when we create a new window.
      (window (setf winptr (%newwin height width (car location) (cadr location)))))

    ;; the following settings have to be executed for all window types.
    (set-input-blocking winptr input-blocking)
    (%scrollok winptr scrolling-enabled-p)
    (%keypad winptr function-keys-enabled-p)))
|#

;; We cant do this because _all_ auxiliary methods are always used and combined.

(defmethod initialize-instance :after ((win window) &key)
  (with-slots (winptr height width location color-pair background) win
    ;; just for WINDOW types
    (when (eq (type-of win) 'window)
      (unless width (setf width 0))
      (unless height (setf height 0))
      (setf winptr (%newwin height width (car location) (cadr location)))

      (when color-pair (setf (color-pair win) color-pair))
      (when background (setf (background win) background)) )))

(defmethod initialize-instance :after ((scr screen) &key)
  (with-slots (winptr colors-enabled-p use-default-colors-p cursor-visible-p input-echoing-p input-blocking
                      function-keys-enabled-p newline-translation-enabled-p
                      scrolling-enabled-p color-pair background input-buffering-p process-control-chars-p) scr
    ;; just for screen window types.
    (when (eq (type-of scr) 'screen)
      (setf winptr (%initscr))
      (when colors-enabled-p
        (if (%has-colors)
            (progn
              (%start-color)
              ;; if t, set (:default :default), if nil set (:white :black), which is the ncurses default.
              (set-default-color-pair use-default-colors-p))
            (error "initialize-instance screen: This terminal does not support colors.")))

      (when color-pair (setf (color-pair scr) color-pair))
      (when background (setf (background scr) background))
      (if newline-translation-enabled-p (%nl) (%nonl))
      (if input-echoing-p (%echo) (%noecho))
      (set-input-mode input-buffering-p process-control-chars-p)
      (set-cursor-visibility cursor-visible-p))))

;; sub-window has to be contained within a parent window
;; touch parent before refreshing a subwindow
;; move also needs a method for subwindows.
;; Subwindows must be deleted before the main window can be deleted.
(defmethod initialize-instance :after ((win sub-window) &key)
  (with-slots (winptr parent height width location relativep) win
    ;; leaving out the size of a window maxes it out to the right and to the bottom
    (unless width (setf width 0))
    (unless height (setf height 0))
    ;; just for SUB-WINDOW types
    (when (eq (type-of win) 'sub-window)
      (if relativep
          ;;(setf winptr (%derwin (slot-value parent 'winptr) height width (car location) (cadr location)))
          (setf winptr (let ((val (%derwin (slot-value parent 'winptr) height width (car location) (cadr location))))
                         (if (null-pointer-p val)
                             ;; may also be null if the parent window passed is null
                             (error "Subwindow could not be created. Probably too big and not contained in the parent window.")
                             val)))
          (setf winptr (%subwin (slot-value parent 'winptr) height width (car location) (cadr location)))))))

(defmethod initialize-instance :after ((win decorated-window) &key)
  (with-slots (winptr width height location sub-window) win
    ;; only for decorated window types
    (when (eq (type-of win) 'decorated-window)
      (setf winptr (%newwin height width (car location) (cadr location)))
      (setf sub-window
            (make-instance 'sub-window :parent win :height (- height 2) :width (- width 2) :location (list 1 1) :relative t)))))

;; called after _all_ :after aux methods.
;; for all window types in the hierarchy.
(defmethod initialize-instance :around ((win window) &key)
  ;; before :before, :after and primary.
  (let ((result (call-next-method)))
    ;; after :before, :after and primary.
    (with-slots (winptr input-blocking function-keys-enabled-p scrolling-enabled-p draw-border-p stackedp) win
      (set-input-blocking winptr input-blocking)
      (%scrollok winptr scrolling-enabled-p)
      (when draw-border-p (%box winptr 0 0))
      (%keypad winptr function-keys-enabled-p)
      (when stackedp (push win *window-stack*)))

    ;; why do we have to return the result in :around aux methods?
    result))

;; Accessors

(defgeneric location (window))
(defmethod location ((window window))
  (list (%getbegy (slot-value window 'winptr)) (%getbegx (slot-value window 'winptr))))
(defmethod location ((win sub-window))
  (with-slots (winptr relativep) win
    (if relativep
        (list (%getpary winptr) (%getparx winptr))
        (list (%getbegy winptr) (%getbegx winptr)))))
(defgeneric (setf location) (coordinates window))
(defmethod (setf location) (coordinates (w window))
  (setf (slot-value w 'location) coordinates)
  (%mvwin (slot-value w 'winptr) (car coordinates) (cadr coordinates)))

;; The slots location-y and location-x dont exist, these accessors exist for convenience.

(defgeneric location-y (window))
(defmethod location-y ((win window))
  (%getbegy (slot-value win 'winptr)))
(defmethod location-y ((win sub-window))
  (with-slots (winptr relativep) win
    (if relativep
        (%getpary winptr)
        (%getbegy winptr))))

(defgeneric location-x (window))
(defmethod location-x ((win window))
  (%getbegx (slot-value win 'winptr)))
(defmethod location-x ((win sub-window))
  (with-slots (winptr relativep) win
    (if relativep
        (%getparx winptr)
        (%getbegx winptr))))

(defgeneric (setf location-y) (y window))
(defmethod (setf location-y) (y (win window))
  (let ((x (cadr (slot-value win 'location))))
    (setf (slot-value win 'location) (list y x))
    (%mvwin (slot-value win 'winptr) y x)))

(defgeneric (setf location-x) (x window))
(defmethod (setf location-x) (x (win window))
  (let ((y (car (slot-value win 'location))))
    (setf (slot-value win 'location) (list y x))
    (%mvwin (slot-value win 'winptr) y x)))

;; "The screen-relative parameters of the window are not changed. 
;; This routine is used to display different parts of the parent 
;; window at the same physical location on the screen."
;;
;; "The mvderwin() function specifies a mapping of characters.
;; The function identifies a mapped area of the parent of the specified window"

;; TODO: rename to source-location
(defgeneric (setf source-location) (coordinates sub-window))
(defmethod (setf source-location) (coordinates (w sub-window))
  (setf (slot-value w 'source-location) coordinates)
  (%mvderwin (slot-value w 'winptr) (car coordinates) (cadr coordinates)))

(defgeneric width (window))
(defmethod width ((window window))
  (%getmaxx (slot-value window 'winptr)))

(defgeneric height (window))
(defmethod height ((window window))
  (%getmaxy (slot-value window 'winptr)))

(defgeneric cursor-position (window))
(defmethod cursor-position ((window window))
  (list (%getcury (slot-value window 'winptr)) (%getcurx (slot-value window 'winptr))))

;; we can move the cursor by doing this, or by calling "move". 
;; both will use %wmove in the background.
;; note that incd and decf dont work.
(defgeneric (setf cursor-position) (coordinates window))
(defmethod (setf cursor-position) (coordinates (w window))
  (setf (slot-value w 'cursor-position) coordinates)
  (%wmove (slot-value w 'winptr) (car coordinates) (cadr coordinates)))

;; The slots location-y and location-x dont exist, but the pseudo accessors
;; exist for convenience.

(defgeneric cursor-position-y (window))
(defmethod cursor-position-y ((win window))
  (%getcury (slot-value win 'winptr)))

(defgeneric cursor-position-x (window))
(defmethod cursor-position-x ((win window))
  (%getcurx (slot-value win 'winptr)))

(defgeneric (setf cursor-position-y) (y window))
(defmethod (setf cursor-position-y) (y (win window))
  (let ((x (cadr (slot-value win 'cursor-position))))
    (setf (slot-value win 'cursor-position) (list y x))
    (%wmove (slot-value win 'winptr) y x)))

(defgeneric (setf cursor-position-x) (x window))
(defmethod (setf cursor-position-x) (x (win window))
  (let ((y (car (slot-value win 'cursor-position))))
    (setf (slot-value win 'cursor-position) (list y x))
    (%wmove (slot-value win 'winptr) y x)))

(defgeneric cursor-visible-p (window))
(defmethod cursor-visible-p ((screen screen))
  (slot-value screen 'cursor-visible-p))
(defgeneric (setf cursor-visible-p) (status screen))
(defmethod (setf cursor-visible-p) (status (screen screen))
  (setf (slot-value screen 'cursor-visible-p) status)
  (set-cursor-visibility status))

(defgeneric input-blocking (window))
(defmethod input-blocking ((window window))
  (slot-value window 'input-blocking))
(defgeneric (setf input-blocking) (status window))
(defmethod (setf input-blocking) (status (window window))
  (setf (slot-value window 'input-blocking) status)
  (set-input-blocking (slot-value window 'winptr) status))

(defgeneric function-keys-enabled-p (window))
(defmethod function-keys-enabled-p ((window window))
  (slot-value window 'function-keys-enabled-p))
(defgeneric (setf function-keys-enabled-p) (status window))
(defmethod (setf function-keys-enabled-p) (status (window window))
  (setf (slot-value window 'function-keys-enabled-p) status)
  (%keypad (slot-value window 'winptr) status))

(defgeneric scrolling-enabled-p (window))
(defmethod scrolling-enabled-p ((window window))
  (slot-value window 'scrolling-enabled-p))
(defgeneric (setf scrolling-enabled-p) (status window))
(defmethod (setf scrolling-enabled-p) (status (window window))
  (setf (slot-value window 'scrolling-enabled-p) status)
  (%scrollok (slot-value window 'winptr) status))

(defgeneric scrolling-region (window))
(defmethod scrolling-region ((window window))
  (slot-value window 'scrolling-region))
(defgeneric (setf scrolling-region) (list window))
(defmethod (setf scrolling-region) (list (window window))
  (setf (slot-value window 'scrolling-region) list)
  (%wsetscrreg (slot-value window 'winptr)
               (first  (slot-value window 'scrolling-region))
               (second (slot-value window 'scrolling-region))))
;; TODO: setf only when scrolling is enabled.

(defgeneric input-echoing-p (screen))
(defmethod input-echoing-p ((screen screen))
  (slot-value screen 'input-echoing-p))
(defgeneric (setf input-echoing-p) (status screen))
(defmethod (setf input-echoing-p) (status (screen screen))
  (setf (slot-value screen 'input-echoing-p) status)
  ;;(set-input-echoing status))
  (if status (%echo) (%noecho)))

(defgeneric input-buffering-p (screen))
(defmethod input-buffering-p ((screen screen))
  (slot-value screen 'input-buffering-p))
(defgeneric (setf input-buffering-p) (status screen))
(defmethod (setf input-buffering-p) (status (screen screen))
  (setf (slot-value screen 'input-buffering-p) status)
  (set-input-mode (slot-value screen 'input-buffering-p)
                  (slot-value screen 'process-control-chars-p)))

(defgeneric process-control-chars-p (screen))
(defmethod process-control-chars-p ((screen screen))
  (slot-value screen 'process-control-chars-p))
(defgeneric (setf process-control-chars-p) (status screen))
(defmethod (setf process-control-chars-p) (status (screen screen))
  (with-slots (input-buffering-p process-control-chars-p) screen
    ;; only make a change when the status changed.
    (when (not (eq process-control-chars-p status))
      ;; only call ncurses when buffering is nil.
      (unless input-buffering-p
        ;; before we switch the unbuffered mode, we switch to the default cooked mode.
        (if process-control-chars-p (%nocbreak) (%noraw))
        ;; then make a "clean" switch back to the new unbuffered mode.
        (set-input-mode input-buffering-p status))
      ;; then save the new status.
      (setf process-control-chars-p status) )))

(defgeneric newline-translation-enabled-p (screen))
(defmethod newline-translation-enabled-p ((screen screen))
  (slot-value screen 'newline-translation-enabled-p))
(defgeneric (setf newline-translation-enabled-p) (status screen))
(defmethod (setf newline-translation-enabled-p) (status (screen screen))
  (setf (slot-value screen 'newline-translation-enabled-p) status)
  (if status (%nl) (%nonl)))

(defgeneric background (window))
(defmethod background ((win window))
  (slot-value win 'background))

(defgeneric (setf background) (char window &optional apply))
(defmethod (setf background) (char (window window) &optional (apply t))
  (setf (slot-value window 'background) char)
  (if char
      (set-background-cchar_t window char apply)
      ;; if the background char is nil, set a space without attributes or colors.
      (set-background-cchar_t window
                              (make-instance 'complex-char :simple-char #\space)
                              apply)))

;(defgeneric attributes (window))
(defmethod attributes ((window window))
  (slot-value window 'attributes))

;;(defgeneric (setf attributes) (attributes window))
#|
(defmethod (setf attributes) (attributes (window window))
  (let ((added (set-difference attributes (slot-value window 'attributes)))
        (removed (set-difference (slot-value window 'attributes) attributes)))
    (setf (slot-value window 'attributes) attributes)
    (add-attributes window added)
    (remove-attributes window removed)))
|#
;; TODO use %wattron and %wattroff here.

(defmethod (setf attributes) (attributes (win window))
  (with-slots ((win-attributes attributes)) win
    (let ((added   (set-difference     attributes win-attributes))
          (removed (set-difference win-attributes     attributes)))
      ;; set the list of attributes to the window slot
      (setf win-attributes attributes)
      ;; pass the attrbutes to add and remove to ncurses
      (add-attributes win added)
      (remove-attributes win removed))))

;; we dont want to use set-attributes because it also overwrites the color attribute.
;; we want the attributes function to just handle attributes and leave the color alone.
;; to do that, we have to treat the attributes as a set and ignore set-attributes.

;; (defmethod (setf attributes) (attributes (window window))
;;   (setf (slot-value window 'attributes) attributes)
;;   (set-attributes window attributes))

;; the generic functions are already implicitly generated above in defclass.
;;(defgeneric color-pair (window))
;;(defgeneric (setf color-pair) (color-pair window))

(defmethod color-pair ((window window))
  (slot-value window 'color-pair))
(defmethod (setf color-pair) (color-pair (window window))
  (setf (slot-value window 'color-pair) color-pair)
  (set-color-pair (slot-value window 'winptr) color-pair))

;;; print, prin1, princ, format ~A, ~S

;; print a wide but simple lisp character to a window
;; TODO: add check for insert-mode
(defmethod print-object ((ch character) (stream window))
  (add-wide-char stream ch))

;; TODO: add check for insert-mode
(defmethod print-object ((ch complex-char) (stream window))
  (add-wide-char stream ch))

;; print only simple chars to the lisp repl.
(defmethod print-object ((ch complex-char) stream)
  (princ (simple-char ch) stream))

;; print simple strings to a ncurses widow.
(defmethod print-object ((str string) (stream window))
  (%waddstr (winptr stream) str))

;; print complex strings to a ncurses window.
(defmethod print-object ((cstr complex-string) (stream window))
  (loop for ch across (complex-char-array cstr)
     do (add-wide-char stream ch)))

;; print only simple chars to the lisp repl.
(defmethod print-object ((cstr complex-string) stream)
  (loop for ch across (complex-char-array cstr)
     do (princ (simple-char ch) stream)))

;; methods to close streams, and thus screen _and_ windows, since they are now gray streams.
;; end-screen = %endwin
;; delete-window = %delwin
;; see t07a
;; remember we cant use open to open a window. it works only for file streams.
;; we have to make-instance of a window or a screen.

(defmethod close ((stream window) &key abort)
  (declare (ignore abort))
  ;; if by time of closing, the window is still on the stack, remove it first.
  (if (member stream *window-stack* :test #'eq)
      (setf *window-stack* (remove stream *window-stack* :test #'eq)))
  (%delwin (winptr stream)))

(defmethod close ((stream sub-window) &key abort)
  (declare (ignore abort))
  (%delwin (winptr stream)))

(defmethod close ((stream screen) &key abort)
  (declare (ignore abort))
  (%endwin))

(defmethod close ((stream decorated-window) &key abort)
  (declare (ignore abort))
  (%delwin (winptr (sub-window stream)))
  (%delwin (winptr stream)))

;; although it is not a stream, we will abuse close to close a menu's window and subwindow, which _are_ streams.
(defmethod close ((stream menu-window) &key abort)
  (declare (ignore abort))
  (%delwin (winptr (sub-window stream)))
  (%delwin (winptr stream)))

(defmethod close ((stream dialog-window) &key abort)
  (declare (ignore abort))
  (%delwin (winptr (message-pad stream)))
  (%delwin (winptr (sub-window stream)))
  (%delwin (winptr stream)))

;; SBCL bug when specializing close on gray streams:
;; STYLE-WARNING:
;;    Generic function CLOSE clobbers an earlier FTYPE proclamation
;;    (FUNCTION (STREAM &KEY (:ABORT T)) (VALUES (MEMBER T) &OPTIONAL)) for the
;; same name with (FUNCTION (T &KEY (:ABORT T)) *).
