(in-package :de.anvi.croatoan)

;; menu
;; curses extension for programming menus
;; http://invisible-island.net/ncurses/man/menu.3x.html

;; default size of ncurses menus is 16 rows, 1 col.
(defclass menu (element)
  ((items
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

   ;; TODO: start with nil instead of 0
   (current-item-number
    :initform      0
    :accessor      current-item-number
    :type          integer
    :documentation "Number (row-major mode) of the currently selected item.")

   (current-item
    :initform      nil
    :type          (or null string symbol menu-item number)
    :accessor      current-item
    :documentation "Pointer to the currently selected item object. The first item is initialized as the current item.")

   (current-item-position
    :initarg       :current-item-position
    :initform      nil
    :accessor      current-item-position
    :type          (or null cons)
    :documentation
    "Position (y x) of the current item in the window.
    This information is useful for positioning the cursor on the current item after displaying the menu.")

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
    :documentation "A 2-element list tracking the starting row/y and column/x of the displayed menu region."))

  (:default-initargs :keymap 'menu-map)
  (:documentation "A menu is a list of items that can be selected by the user."))

;; init for menus which aren't menu windows
;; TODO 201230 here we allow which objects are allowed to be passed as items: string, number, symbol, menu
(defmethod initialize-instance :after ((menu menu) &key)
  (with-slots (items current-item layout) menu
    ;; Convert strings and symbols to item objects
    (setf items (mapcar (lambda (item)
                          (if (typep item 'menu-item)
                              ;; if an item object is given, just return it
                              item
                              ;; if we have strings, symbols or menus, convert them to menu-items
                              (make-instance 'menu-item
                                             :name (typecase item
                                                     (string nil)
                                                     (number nil)
                                                     (symbol item)
                                                     (menu (name item)))
                                             :title (typecase item
                                                      (string item)
                                                      (symbol (symbol-name item))
                                                      (number (princ-to-string item))
                                                      ;; if there is a title string, take it,
                                                      ;; otherwise use the menu name as the item title
                                                      (menu (if (and (title item)
                                                                     (stringp (title item)))
                                                                (title item)
                                                                (symbol-name (name item)))))
                                             :value item)))
                        ;; apply the function to the init arg passed to make-instance.
                        items))

    ;; Initialize the current item as the first item from the items list.
    ;; TODO: if initarg items is nil, signal an error.
    (setf current-item (car items))

    ;; if the layout wasnt passed as an argument, initialize it as a single one-column menu.
    (unless layout (setf layout (list (length items) 1)))))

(defclass menu-window (menu extended-window)
  ()
  (:documentation "A menu-window is an extended window displaying a menu in its sub-window."))

(defmethod initialize-instance :after ((win menu-window) &key color-pair)
  (with-slots (winptr items type height width (y position-y) (x position-x) element-position sub-window borderp border-width
               layout scrolled-layout max-item-length current-item-mark fgcolor bgcolor) win
    ;; only for menu windows
    (when (eq (type-of win) 'menu-window)
      (setf border-width (if borderp 1 0))
      ;; if the initarg :position was given, both the window position and the element-position
      ;; have been set. ignore the element position.
      (setf element-position nil)
      ;; if no layout was given, use a vertical list (n 1)
      (unless layout (setf layout (list (length items) 1)))
      ;; if height and width are not given as initargs, they will be calculated,
      ;; according to no of rows +/- border, and _not_ maximized like normal windows.
      (unless height (setf height (+ (* 2 border-width) (car (or scrolled-layout layout)))))
      (unless width  (setf width  (+ (* 2 border-width) (* (cadr (or scrolled-layout layout))
                                                           (+ (length current-item-mark) max-item-length)))))
      (setf winptr (ncurses:newwin height width y x))
      (setf sub-window (make-instance 'sub-window :parent win
                                                  :height (car (or scrolled-layout layout))
                                                  :width (* (cadr (or scrolled-layout layout)) (+ (length current-item-mark) max-item-length))
                                                  :position (list border-width border-width)
                                                  :relative t))
      (cond ((or fgcolor bgcolor)
             (set-color-pair winptr (color-pair win))
             (setf (color-pair sub-window) (color-pair win)
                   (background win) (make-instance 'complex-char :color-pair (color-pair win))
                   (background sub-window) (make-instance 'complex-char :color-pair (color-pair win)) ))
            ;; when a color-pair is passed as a keyword
            (color-pair
             ;; set fg and bg, pass to ncurses
             (setf (color-pair win) color-pair
                   (color-pair sub-window) color-pair
                   (background win) (make-instance 'complex-char :color-pair color-pair)
                   (background sub-window) (make-instance 'complex-char :color-pair color-pair)))))))

;; although it is not a stream, we will abuse close to close a menu's window and subwindow, which _are_ streams.
(defmethod close ((stream menu-window) &key abort)
  (declare (ignore abort))
  (ncurses:delwin (winptr (sub-window stream)))
  (ncurses:delwin (winptr stream)))

(defclass menu-panel (menu panel)
  ()
  (:documentation "A menu-panel is a panel providing a list of items to be selected by the user."))

(defmethod initialize-instance :after ((win menu-panel) &key color-pair)
  (when (eq (type-of win) 'menu-panel)
    (with-slots (winptr items type height width (y position-y) (x position-x) element-position borderp border-width border-win
                 shadow-win shadowp layout scrolled-layout max-item-length current-item-mark) win
      (setf element-position nil)
      (unless layout (setf layout (list (length items) 1)))
      (unless height (setf height (car (or scrolled-layout layout))))
      (unless width  (setf width  (* (cadr (or scrolled-layout layout))
                                     (+ (length current-item-mark) max-item-length))))
      (setf winptr (ncurses:newwin height width y x))
      (let* (;; main win
             (y1 y)
             (x1 x)
             ;; border win
             (y2 (- y1 border-width))
             (x2 (- x1 border-width))
             (h2 (+ height (* border-width 2)))
             (w2 (+ width (* border-width 2)))
             ;; shadow win
             (y3 (+ y2 1))
             (x3 (+ x2 1)))
        ;; check if border is t, if nil, do not create the border window
        (when borderp
          (setf border-win (make-instance 'window :height h2 :width w2 :position (list y2 x2) :border t)))
        ;; check if shadow is t, if nil, do not create the shadow window
        (when shadowp
          (setf shadow-win (make-instance 'window :height h2 :width w2 :position (list y3 x3))))))))

(defclass checklist (menu)
  ()
  (:default-initargs :menu-type :checklist)
  (:documentation "A checklist is a multi-selection menu with checkable items."))

(defclass menu-item (checkbox)
  ((value
    :type          (or symbol keyword string menu menu-window menu-panel function number)
    :documentation "The value of an item can be a string, a number, a sub menu or a function to be called when the item is selected."))

  (:documentation  "A menu contains of a list of menu items."))

;; not used anywhere
(defun list2array (list dimensions)
  "Example: (list2array '(a b c d e f) '(3 2)) => #2A((A B) (C D) (E F))"
  (let ((m (car dimensions))
        (n (cadr dimensions)))
    (assert (= (length list) (* m n)))
    (let ((array (make-array dimensions :initial-element nil)))
      (loop for i from 0 to (- m 1)
         do (loop for j from 0 to (- n 1)
               do (setf (aref array i j) (nth (+ (* i n) j) list))))
      array)))

;; TODO: see menu_format
(defun rmi2sub (layout rmi)
  "Take array dimensions and an index in row-major order, return two subscripts.

Example: (rmi2sub '(2 3) 5) => (1 2)"
  (let ((m (car layout))
        (n (cadr layout)))
    (assert (< rmi (* m n)))
    (multiple-value-bind (q r) (floor rmi n)
      (list q r))))

(defun sub2rmi (layout subs)
  "Take array dimensions and two subscripts, return an index in row-major order.

Example: (sub2rmi '(2 3) '(1 2)) => 5"
  (let ((m (car layout))
        (n (cadr layout))
        (i (car subs))
        (j (cadr subs)))
    (assert (and (< i m) (< j n)))
    (+ (* i n) j)))

(defmethod clear ((menu menu) &key)
  "Overwrite the displayed menu with spaces."
  (with-accessors ((win window)
                   (len max-item-length)
                   (pos element-position)) menu
    ;; return the visible layout of the menu
    (let* ((layout (if (scrolled-layout menu)
                       (scrolled-layout menu)
                       (layout menu)))
           (m (car layout))
           (n (cadr layout))
           (w (* n len)))
      (clear-rectangle win (car pos) (cadr pos) m w))))

;; TODO 190308: allow events other then the arrow keys to be used to control the menu.
(defun update-menu (menu event)
  "Take a menu and an event, update in-place the current item of the menu."
  ;; we need to make menu special in order to setf i in the passed menu object.
  (declare (special menu))
  (with-accessors ((current-item-number current-item-number) (current-item current-item) (items items)
                   (cyclic-selection cyclic-selection-p) (layout layout) (scrolled-layout scrolled-layout)
                   (scrolled-region-start scrolled-region-start)) menu
    (let ((i  (car  (rmi2sub layout current-item-number)))
          (j  (cadr (rmi2sub layout current-item-number)))
          (m  (car  layout))
          (n  (cadr layout))
          (m0 (car  scrolled-region-start))
          (n0 (cadr scrolled-region-start))
          (m1 (car  scrolled-layout))
          (n1 (cadr scrolled-layout)))
      (if scrolled-layout
          ;; when scrolling is on, the menu is not cycled.
          (progn
            ;; TODO 201114 allow the user to set events instead of hard-coding up left down right, for example hjkl.
            (case event
              (:up    (when (> i 0) (decf i))             ; when not in first row, move one row up
                      (when (< i m0) (decf m0)))          ; when above region, move region one row up
              (:down  (when (< i (1- m)) (incf i))        ; when not in last row, move one row down
                      (when (>= i (+ m0 m1)) (incf m0)))  ; when below region, move region one row down
              (:left  (when (> j 0) (decf j))             ; when not in first column, move one column left
                      (when (< j n0) (decf n0)))          ; when left of region, move region one column left
              (:right (when (< j (1- n)) (incf j))        ; when not in last column, move one column right
                      (when (>= j (+ n0 n1)) (incf n0)))) ; when right of region, move region one column right

            ;; set new scrolled-region coordinates
            (setf scrolled-region-start (list m0 n0)))

          ;; when scrolling is off, the menu can be cycled.
          (if cyclic-selection
                ;; do cycle through the items
                (case event
                  (:up    (setf i (mod (1- i) m)))
                  (:down  (setf i (mod (1+ i) m)))
                  (:left  (setf j (mod (1- j) n)))
                  (:right (setf j (mod (1+ j) n))))
                ;; dont cycle through the items
                (case event
                  (:up    (setf i (max (1- i) 0)))
                  (:down  (setf i (min (1+ i) (1- m))))
                  (:left  (setf j (max (1- j) 0)))
                  (:right (setf j (min (1+ j) (1- n)))))))

      ;; after updating i,j, update the current-item-number
      (setf current-item-number (sub2rmi layout (list i j)))
      ;; after updating the current-item-number, update the pointer to the current-item.
      (setf current-item (nth current-item-number items)) )))

(defun format-menu-item (menu item-number)
  "Take a menu and return item item-number as a properly formatted string.

If the menu is a checklist, return [ ] or [X] at the first position.

If a mark is set for the current item, display the mark at the second position.
Display the same number of spaces for other items.

At the third position, display the item given by item-number."
  (with-accessors ((items items)
                   (type menu-type)
                   (current-item-number current-item-number)
                   (current-item-mark current-item-mark)) menu
    ;; return as string
    (format nil "~A~A~A"
            ;; two types of menus: :selection or :checklist
            ;; show the checkbox before the item in checklists
            (if (eq type :checklist)
                (if (checkedp (nth item-number items)) "[X] " "[ ] ")
                "")
            
            ;; for the current item, draw the current-item-mark
            ;; for all other items, draw a space
            (if (= current-item-number item-number)
                current-item-mark
                (make-string (length current-item-mark) :initial-element #\space))

            ;; then add the item title
            (format-title (nth item-number items)) )))

;; this is the only function that actually positions items on the screen (using move)
(defun draw-menu-item (win menu item-number i j)
  "Draw the item given by item-number at item position (i j) in the window."
  (with-accessors ((current-item-number current-item-number)
                   (current-item-mark current-item-mark)
                   (current-item-position current-item-position)
                   (max-item-length max-item-length)
                   (element-position element-position)
                   (style style)) menu
    (let* (pos-y
           pos-x
           (item-selected-p (= item-number current-item-number))
           (len (+ (length current-item-mark) max-item-length)))
      (if element-position
          ;; add an offset when element-position is given
          (setq pos-y (+ i         (car  element-position))
                pos-x (+ (* j len) (cadr element-position)))
          ;; if a position is not given, display the menu starting at 0,0
          (setq pos-y i
                pos-x (* j len)))
      (move win pos-y pos-x)
      ;; save the position of the current item, to be used in update-cursor-position.
      (when item-selected-p
        (setf current-item-position (list pos-y pos-x)))

      ;; TODO 191226: foreground should be displayed when the item is current AND the menu element is selected.
      ;; TODO 191227: have a default selected style, the default non-selected style is nil.
      (let ((fg-style (if style
                          (getf style (if item-selected-p :selected-foreground :foreground))
                          ;; default foreground style
                          (if item-selected-p (list :attributes (list :reverse)) nil)))
            (bg-style (if style
                          (getf style (if item-selected-p :selected-background :background))
                          ;; default background style
                          (if item-selected-p (list :attributes (list :reverse)) nil))))
        ;; write an empty string as the background.
        ;; TODO 200705: :n should take mark length into account
        (save-excursion win (add win #\space :style bg-style :n len))
        ;; display it in the window associated with the menu
        (add win (format-menu-item menu item-number) :style fg-style)))))

;; draws to any window, not just to a sub-window of a menu-window.
(defun draw-menu (window menu)
  "Draw the menu to the window."
  (with-accessors ((layout layout) (scrolled-layout scrolled-layout) (scrolled-region-start scrolled-region-start)) menu

    ;; TODO 191201: remove this to be able to draw a menu within a form.
    ;; if we clear here, we clear the whole window, which also clears the rest of the form, not only the menu.
    ;; problem: when we do not clear the window, the highlight of the last item is not removed

    ;; t19b2 menus are not displayed properly if we do not clear
    
    ;;(clear window)

    (let ((m  (car  layout))
          (n  (cadr layout))
          (m0 (car  scrolled-region-start))
          (n0 (cadr scrolled-region-start))
          (m1 (car  scrolled-layout))
          (n1 (cadr scrolled-layout)))
      (if scrolled-layout
          ;; when the menu is too big to be displayed at once, only a part
          ;; is displayed, and the menu can be scrolled
          (loop for i from 0 to (1- m1)
             do (loop for j from 0 to (1- n1)
                   do (let ((item-number (sub2rmi layout (list (+ m0 i) (+ n0 j)))))
                        ;; the menu is given as a flat list, so we have to access it as a 2d array in row major order
                        (draw-menu-item window menu item-number i j))))
          ;; when there is no scrolling, and the whole menu is displayed at once
          (loop for i from 0 to (1- m)
             do (loop for j from 0 to (1- n)
                   do (let ((item-number (sub2rmi layout (list i j))))
                        (draw-menu-item window menu item-number i j)))) ))
    (refresh window)))

(defmethod draw ((menu menu))
  "Draw the menu to its associated window."
  (draw-menu (window menu) menu)
  ;; when menu is a part of a form:
  ;; update-cursor-position = place the cursor on the current item
  ;; if the menu is a checklist, place the cursor inside the [_], like it is done with a single checkbox.
  (update-cursor-position menu))

(defmethod draw ((menu menu-window))
  "Draw the menu to its content sub-window."
  (with-accessors ((sub-win sub-window)) menu
    ;; draw the menu to the sub-window
    (draw-menu sub-win menu)
    ;; we have to explicitely touch the background win, because otherwise it wont get refreshed.
    (touch menu)
    ;; TODO: when we refresh a window with a subwin, we shouldnt have to refresh the subwin separately.
    ;; make refresh specialize on menu and extended-window in a way to do both.
    (refresh menu)))

(defmethod draw ((menu menu-panel))
  (with-accessors ((title title) (borderp borderp)) menu
    (when (and borderp title)
      (add-title (slot-value menu 'border-win)
                 (format-title menu "| " " |"))))
  (draw-menu menu menu)
  (refresh menu))

(defmethod draw ((menu dialog-window))
  ;; draw the title only when we also have a border, because we draw the title on top of the border.
  ;; If there is a title string, take it, otherwise take the name.
  ;; The name is displayed only if title is t.
  (with-accessors ((title title) (borderp borderp)) menu
    (when (and borderp title)
      (add-title menu)))

  ;; then draw a menu-window, dialog-window's superclass
  (call-next-method)

  ;; then draw the message in the reserved space above the menu.
  (with-accessors ((message-text message-text) (message-height message-height)
                   (message-pad message-pad) (coords message-pad-coordinates)) menu
    ;; if there is text, and there is space reserved for the text, draw the text
    (when (and message-text (> message-height 0))
      (refresh message-pad
               0                   ;pad-min-y
               0                   ;pad-min-x
               (first  coords)     ;screen-min-y
               (second coords)     ;screen-min-x
               (third  coords)     ;screen-max-y
               (fourth coords))))) ;screen-max-x

;; called from:
;;   return-from-menu
(defun reset-menu (menu)
  "After the menu is closed reset it to its initial state."
  (with-slots (items current-item-number current-item scrolled-region-start menu-type) menu
    (setf current-item-number   0
          current-item          (car items)
          scrolled-region-start (list 0 0))
    (when (eq menu-type :checklist)
      (loop for i in items if (checkedp i) do (setf (checkedp i) nil)))))

;; stack for managing overlapping menu windows
(defparameter *menu-stack* (make-instance 'stack))

(defun return-from-menu (menu return-value)
  "Pop the menu from the menu stack, refresh the remaining menu stack.

If the menu is not a window, clear the menu from the window.

Return the value from select."
  (if (typep menu 'window)
      ;; only pop if menu is a window, it makes no sense to pop if it is a simple menu
      (unless (stack-empty-p *menu-stack*)
        (stack-pop *menu-stack*))
      ;; if the menu is not a window, clear the region where it was drawn
      ;; this has the same effect as removing the menu from the stack
      (clear menu))
  (reset-menu menu)
  (throw menu return-value))

(defun exit-menu-event-loop (menu event)
  "Associate this function with an event to exit the menu event loop."
  (declare (ignore event))
  (return-from-menu menu nil))

;; TODO 191201: (setf (checked-items menu) (list :name1 :name2))
;; pass a list of items to be checked.
(defun checked-items (menu)
  "Take a menu, return a list of checked menu items."
  (loop for i in (items menu) if (checkedp i) collect i))  

(defmethod value ((menu menu))
  "Return the value of the selected item."
  (value (current-item menu)))

(defmethod value ((checklist checklist))
  "Return the list of values of the checked items."
  (mapcar #'value (checked-items checklist)))

(defun accept-selection (menu event)
  "Return the value of the currently selected item or all checked items."
  (declare (ignore event))

  ;; TODO 191201: do not check the menu-type slot, but the object type
  ;; menu vs checklist
  (case (menu-type menu)
    (:checklist
     ;; return all checked items (not their values) in the item list.
     (return-from-menu menu (checked-items menu)))

    (:selection
     (let ((val (value (current-item menu))))
       (cond
         ;; if the item is a string or symbol, just return it.
         ((or (typep val 'string)
              (typep val 'symbol)
              (typep val 'number))
          (return-from-menu menu val))

         ;; TODO 201227 instead of adding separate function objects, do what CAPI does and add a callback slot to every item
         ;; if the item is a function object, call it.
         ((typep val 'function)
          (funcall val)
          (return-from-menu menu (name (current-item menu))))

         ;; if the item is a menu (and thus also a menu-window or panel), recursively select an item from that submenu
         ((or (typep val 'menu)
              (typep val 'menu-window)
              (typep val 'menu-panel))

          ;; code to run before a submenu is called
          ;; this can be used to clear a parent menu before calling a submenu
          (run-hook menu 'before-submenu-hook)

          (let ((selected-item (select val)))

            ;; when we have more than menu in one window, redraw the parent menu when we return from the submenu.
            (when (eq (type-of val) 'menu)
              (draw menu))

            ;; if a value was returned by the sub-menu, return it as the value of the parent menu.
            (when selected-item
              (return-from-menu menu selected-item)))) )))))

(defun update-redraw-menu (menu event)
  "Update the menu after an event, the redraw the menu."
  (update-menu menu event)
  (draw menu))

(defun toggle-item-checkbox (menu event)
  "Toggle the checked state of the current item, used in checkbox menus."
  (declare (ignore event))
  (setf (checkedp (current-item menu)) (not (checkedp (current-item menu))))
  (draw menu))

;; all of these take two arguments: menu event
;; there is no :default action, all other events are ignored for menus.
(define-keymap menu-map
  ;; q doesnt return a value, just nil, i.e. in the case of a checklist, an empty list.
  (#\q 'exit-menu-event-loop)
  (#\x 'toggle-item-checkbox)

  (:up    'update-redraw-menu)
  (:down  'update-redraw-menu)
  (:left  'update-redraw-menu)
  (:right 'update-redraw-menu)

  ;; return the selected item or all checked items, then exit the menu like q.
  ;; TODO 191213: when the menu is an element, this action should not be called from an element,
  ;; but from the parent form.
  ;; so if a menu is always be used as an element, remove this.
  ;; it should stay for the cases when we use menus which are not embedded in a form.
  (#\newline 'accept-selection))

(defgeneric select (obj))

(defmethod select ((menu menu))
  (draw menu)
  (run-event-loop menu))

(defmethod select ((menu menu-window))
  "Display the menu, let the user select an item, return the selected item.

If the selected item is a menu object, recursively display the sub menu."
  ;; when the menu is selected, push it to the menu stack.
  (stack-push menu *menu-stack*)

  (draw menu)
  
  ;; here we can pass the menu to run-event-loop because it is a menu-window.
  ;; all handler functions have to accept window and event as arguments.
  (let ((val (run-event-loop menu)))

    ;; when we return from a menu, the menu is closed and we have to repaint the windows below the menu.
    ;; this can be done manually or by adding them to the main stack with :stacked t
    (unless (stack-empty-p *main-stack*)
      (refresh *main-stack*))

    ;; when we return from a menu, we pop the menu from the menu stack, then repain the remaining stack
    ;; that way a stack of open sub-menus is cleanly displayed
    (unless (stack-empty-p *menu-stack*)
      (refresh *menu-stack*))

    ;; the return value of select is the return value of run-event-loop
    ;; is the value thrown to the catch tag 'event-loop.
    val))

(defmethod select ((menu menu-panel))
  (stack-push menu *menu-stack*)
  (draw menu)
  (let ((val (run-event-loop menu)))
    (unless (stack-empty-p *main-stack*)
      (refresh *main-stack*))
    (unless (stack-empty-p *menu-stack*)
      (refresh *menu-stack*))
    val))
