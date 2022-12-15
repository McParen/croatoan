(in-package :de.anvi.croatoan)

;; menu
;; curses extension for programming menus
;; http://invisible-island.net/ncurses/man/menu.3x.html

;; default size of ncurses menus is 16 rows, 1 col.
(defclass menu (element layout)
  ((menu-type
    :initarg       :menu-type
    :initform      :selection
    :accessor      menu-type
    :type          keyword
    :documentation "Types of menus: :selection (default) or :checklist.")

   (current-item-position
    :initarg       :current-item-position
    :initform      nil
    :accessor      current-item-position
    :type          (or null cons)
    :documentation "Position (y x) of the current item in the window.

This can be used to position the cursor on the current item after the menu is drawn.")

   (current-item-mark
    :initarg       :current-item-mark
    :initform      ""
    :reader        current-item-mark
    :type          string
    :documentation "A string prefixed to the current item in the menu.")

   (tablep
    :initarg       :table
    :initform      nil
    :reader        tablep
    :type          boolean
    :documentation "If t, table row and column lines are drawn between the items.")

   (item-padding-left
    :initarg       :item-padding-left
    :initform      0
    :type          integer
    :documentation "Additional space added to the left of the item title, with the same background style.")

   (item-padding-right
    :initarg       :item-padding-right
    :initform      0
    :type          integer
    :documentation "Additional space added to the right of the item title, with the same background style.")

   (variable-column-width-p
    :initarg       :variable-column-width
    :initform      nil
    :reader        variable-column-width-p
    :type          boolean
    :documentation "If t, columns widths are calculated from the items.

If nil (default), use max-item-length as the width for every column.")

   (max-item-length
    :initarg       :max-item-length
    :initform      15
    :accessor      max-item-length
    :type          integer
    :documentation "Max number of characters displayed for a single item."))

  (:default-initargs :keymap 'menu-map)
  (:documentation
   "A menu is a list of items displayed in a grid that can be selected by the user.

Item types can be strings, symbols, numbers, other menus or callback functions."))

;; init for menus which aren't menu windows
(defmethod initialize-instance :after ((menu menu) &key)
  (with-slots (children grid-rows grid-columns region-rows region-columns region-start-row region-start-column
               tablep grid-row-gap grid-column-gap) menu
    (setf region-start-row 0
          region-start-column 0)

    ;; Convert strings and symbols to item objects
    (setf children (mapcar (lambda (item)
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
                        children))

    ;; if the layout wasnt passed as an argument, initialize it as a single one-column menu.
    (unless grid-rows
      (setf grid-rows (length children)))
    (unless grid-columns
      (setf grid-columns 1))

    ;; If table lines have to be drawn, a gap between the items also has to be set.
    (when tablep
      (when (zerop grid-row-gap)
        (setf grid-row-gap 1))
      (when (zerop grid-column-gap)
        (setf grid-column-gap 1)))))

(defmethod width ((obj menu))
  (with-accessors ((len max-item-length)
                   (variable-column-width-p variable-column-width-p)
                   (items items)) obj
    (with-slots (scrolling-enabled-p
                 menu-type
                 item-padding-left
                 item-padding-right
                 (m  grid-rows)
                 (n  grid-columns)
                 (cg grid-column-gap)
                 (m0 region-start-row)
                 (n0 region-start-column)
                 (m1 region-rows)
                 (n1 region-columns)) obj
      (if variable-column-width-p
          ;; variable column width
          (destructuring-bind (m0 n0 m1 n1) (if scrolling-enabled-p (list m0 n0 m1 n1) (list 0 0 m n))
            (let* ((widths- (mapcar (lambda (i) (+ i item-padding-left item-padding-right))
                                    (if variable-column-width-p
                                        (subseq (column-widths items (list m n)) n0 (+ n0 n1))
                                        (loop for i below n1 collect len))))
                   (widths (if (eq menu-type :checklist)
                               (mapcar (lambda (i) (+ i 4)) widths-)
                               widths-))
                   (gaps (if (plusp cg) (* (1- n) cg) 0)))
              (+ gaps
                 (loop for i in widths sum i))))

          ;; fixed column width
          (let* ((w (* n (if (eq menu-type :checklist)
                             (+ len 4)
                             len)))
                 ;; if a table is drawn, we have n-1 row lines
                 (gaps (if (plusp cg) (* (1- n) cg) 0)))
            (+ w
               gaps
               (* n item-padding-left)
               (* n item-padding-right)))))))

(defmethod height ((obj menu))
  (with-accessors ((m visible-grid-rows)) obj
    (with-slots ((rg grid-row-gap)) obj
      (let* ((gaps (if (plusp rg) (* (1- m) rg) 0)))
        ;; the height of the menu is a sum of m items of height 1
        ;; and the m-1 gaps between the items
        (+ m gaps)))))

(defmethod external-width ((obj menu))
  (with-accessors ((w width) (borderp borderp) (tablep tablep) (bl border-width-left) (br border-width-right) (pl padding-left) (pr padding-right)) obj
    (cond (tablep
           (+ w bl br))
          (borderp
           (+ w pl pr bl br))
          (t
           (+ w pl pr)))))

(defmethod external-height ((obj menu))
  (with-accessors ((h height) (borderp borderp) (tablep tablep) (bt border-width-top) (bb border-width-bottom) (pt padding-top) (pb padding-bottom)) obj
    (cond (tablep
           (+ h bt bb))
          (borderp
           (+ h pt pb bt bb))
          (t
           (+ h pt pb)))))

(defmethod visible-width ((obj menu))
  (with-accessors ((w width) (borderp borderp) (tablep tablep) (bl border-width-left) (br border-width-right) (pl padding-left) (pr padding-right)) obj
    (cond (tablep
           (+ w bl br))
          (t
           (+ w pl pr)))))

(defmethod visible-height ((obj menu))
  (with-accessors ((h height) (borderp borderp) (tablep tablep) (bt border-width-top) (bb border-width-bottom) (pt padding-top) (pb padding-bottom)) obj
    (cond (tablep
           (+ h bt bb))
          (t
           (+ h pt pb)))))

(defclass menu-window (menu extended-window)
  ()
  (:documentation "A menu-window is an extended window displaying a menu in its sub-window."))

(defmethod initialize-instance :after ((win menu-window) &key color-pair)
  (with-slots (winptr children type height width (y position-y) (x position-x) sub-window borderp border-width
               grid-rows grid-columns region-rows region-columns max-item-length current-item-mark fgcolor bgcolor) win
    ;; only for menu windows
    (when (eq (type-of win) 'menu-window)
      (setf border-width (if borderp 1 0))

      ;; if no layout was given, use a vertical list (n 1)
      (unless grid-rows (setf grid-rows (length children)))
      (unless grid-columns (setf grid-columns 1))

      ;; if height and width are not given as initargs, they will be calculated,
      ;; according to no of rows +/- border, and _not_ maximized like normal windows.
      (unless height (setf height (+ (* 2 border-width) (visible-grid-rows win))))
      (unless width  (setf width  (+ (* 2 border-width) (* (visible-grid-columns win)
                                                           (+ (length current-item-mark) max-item-length)))))
      (setf winptr (ncurses:newwin height width y x))
      (setf sub-window (make-instance 'sub-window :parent win
                                                  :height (visible-grid-rows win)
                                                  :width (* (visible-grid-columns win)
                                                            (+ (length current-item-mark) max-item-length))
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

;; panel precedes menu because we need clear to clear the main window, not the menu contents.
(defclass menu-panel (panel menu)
  ()
  (:documentation "A menu-panel is a panel providing a list of items to be selected by the user."))

(defmethod initialize-instance :after ((win menu-panel) &key)
  (when (eq (type-of win) 'menu-panel)
    (with-slots (winptr children type height width (y position-y) (x position-x) borderp border-width border-win
                 shadow-win shadowp grid-rows grid-columns region-rows region-columns max-item-length current-item-mark) win
      (unless grid-rows (setf grid-rows (length children)))
      (unless grid-columns (setf grid-columns 1))
      (unless height (setf height (visible-grid-rows win)))
      (unless width  (setf width  (* (visible-grid-columns win)
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

(defun rmi2sub (dimensions rmi)
  "Take array dimensions and an index in row-major order, return two subscripts.

This works analogous to cl:row-major-aref.

Example: (rmi2sub '(2 3) 5) => (1 2)"
  (let ((m (car dimensions))
        (n (cadr dimensions)))
    (assert (< rmi (* m n)))
    (multiple-value-bind (q r) (floor rmi n)
      (list q r))))

(defun sub2rmi (dimensions subs)
  "Take array dimensions and two subscripts, return an index in row-major order.

This works analogous to cl:array-row-major-index.

Example: (sub2rmi '(2 3) '(1 2)) => 5"
  (let ((m (car dimensions))
        (n (cadr dimensions))
        (i (car subs))
        (j (cadr subs)))
    (assert (and (< i m) (< j n)))
    (+ (* i n) j)))

(defmethod clear ((obj menu) &key)
  (with-accessors ((x position-x) (y position-y) (ew external-width) (eh external-height) (vw visible-width) (vh visible-height) (tablep tablep)
                   (bt border-width-top) (bl border-width-left) (borderp borderp) (selectedp selectedp) (win window) (style style)) obj
    (let* ((bg-style (if selectedp
                         (getf style :selected-background)
                         (getf style :background)))
           (border-style (if selectedp
                             (getf style :selected-border)
                             (getf style :border))))
      (cond (tablep
             (fill-rectangle win (apply #'make-instance 'complex-char bg-style) y x vh vw))
            (borderp
             (fill-rectangle win (apply #'make-instance 'complex-char border-style) y x eh ew)
             (fill-rectangle win (apply #'make-instance 'complex-char bg-style) (+ y bt) (+ x bl) vh vw))
            (t
             (fill-rectangle win (apply #'make-instance 'complex-char bg-style) y x vh vw))))))

(defmethod clear ((obj menu-window) &key)
  (with-accessors ((sub-win sub-window)) obj
    (clear sub-win)))

;; clear menu-panel calls clear window, since the precedence is (panel menu)

(defun draw-table-top (win widths)
  "Draw the top line of a table at the current position using ANSI drawing characters."
  (add-char win :upper-left-corner)
  (dolist (n (butlast widths))
    (add-char win :horizontal-line :n n)
    (add-char win :tee-pointing-down))
  (add-char win :horizontal-line :n (car (last widths)))
  (add-char win :upper-right-corner))

(defun draw-table-row-separator (win widths)
  (crt:add-char win :tee-pointing-right)
  (dolist (n (butlast widths))
    (crt:add-char win :horizontal-line :n n)
    (crt:add-char win :crossover-plus))
  (add-char win :horizontal-line :n (car (last widths)))
  (add-char win :tee-pointing-left))

(defun draw-table-bottom (win widths)
  "Draw the top line of a table at the current position using ANSI drawing characters."
  (add-char win :lower-left-corner)
  (dolist (n (butlast widths))
    (add-char win :horizontal-line :n n)
    (add-char win :tee-pointing-up))
  (add-char win :horizontal-line :n (car (last widths)))
  (add-char win :lower-right-corner))

(defun draw-table-lines (win y x m1 n1 rg cg bt bl widths heights)
  ;;; draw the n1+1 vertical lines (plain, without endings and crossings)
  ;; first vline
  (draw-vline win (1+ y) x (1- (* m1 2)))
  ;; last n1 vlines
  (dotimes (j n1)
    (draw-vline win
                (1+ y)
                (+ x bl (* j cg) (loop for i from 0 to j sum (nth i widths)))
                (1- (* m1 2))))

  ;;; draw m1+1 horizontal lines (with endings and crossings)
  ;; top horizontal line of the table
  (move win y x)
  (draw-table-top win widths)
  ;; m1-1 row separators
  (dotimes (i (- m1 1))
    (move win
          (+ y bt (* i rg) (loop for j from 0 to i sum (nth j heights)))
          x)
    (draw-table-row-separator win widths))
  ;; bottom line
  (move win (+ y (* 2 m1)) x)
  (draw-table-bottom win widths))

(defun format-menu-item (menu item selectedp)
  "Take a menu and return item item-number as a properly formatted string.

If the menu is a checklist, return [ ] or [X] at the first position.

If a mark is set for the current item, display the mark at the second position.
Display the same number of spaces for other items.

At the third position, display the item given by item-number."
  (with-accessors ((type menu-type)
                   (current-item-number current-item-number)
                   (current-item-mark current-item-mark)) menu
    ;; return as string
    (format nil "~A~A~A"
            ;; two types of menus: :selection or :checklist
            ;; show the checkbox before the item in checklists
            (if (eq type :checklist)
                (if (checkedp item) "[X] " "[ ] ")
                "")

            ;; for the current item, draw the current-item-mark
            ;; for all other items, draw a space
            (if selectedp
                current-item-mark
                (make-string (length current-item-mark) :initial-element #\space))
            ;; then add the item title
            (format-title item))))

(defmethod external-width ((obj menu-item))
  (length (title obj)))

;; draws to any window, not just to a sub-window of a menu-window.
(defun draw-menu (win menu)
  "Draw the menu to the window."
  (with-slots (scrolling-enabled-p
               variable-column-width-p
               menu-type
               item-padding-left
               item-padding-right
               (cmark current-item-mark)
               (cpos current-item-position)
               (len max-item-length)
               (items children)
               (borderp borderp)
               (tablep tablep)
               (r grid-row)
               (c grid-column)
               (m  grid-rows)
               (n  grid-columns)
               (m0 region-start-row)
               (n0 region-start-column)
               (m1 region-rows)
               (n1 region-columns)
               (rg grid-row-gap)
               (cg grid-column-gap)
               (bt border-width-top)
               (bl border-width-left)) menu
    (with-accessors ((style style)) menu
      (clear menu)
      ;; start and end indexes
      (destructuring-bind (m0 n0 m1 n1)
          ;; when the menu is too big to be displayed at once, only a part
          ;; is displayed, and the menu can be scrolled
          (if scrolling-enabled-p (list m0 n0 m1 n1) (list 0 0 m n))
        (let* ((widths- (mapcar (lambda (i) (+ i item-padding-left item-padding-right))
                                (if variable-column-width-p
                                    (subseq (column-widths items (list m n)) n0 (+ n0 n1))
                                    (loop for i below n1 collect len))))
               (widths (if (eq menu-type :checklist)
                           (mapcar (lambda (i) (+ i 4)) widths-)
                           widths-))
               (xs (cumsum-predecessors widths))
               (heights (loop for i below m1 collect 1))
               ;; height of one item is just 1.
               (h 1)
               (y (car (content-position menu)))
               (x (cadr (content-position menu))))

          ;; draw a border and table lines only to simple menus (at the moment)
          (when (not (typep menu 'window))
            (when borderp
              (draw-rectangle win (position-y menu) (position-x menu) (external-height menu) (external-width menu) :style (getf style :border)))
            (when tablep
              ;; to draw table lines between the grid cells, a grid gap is required.
              (draw-table-lines win y x m1 n1 rg cg bt bl widths heights)))

          (dogrid ((i 0 m1)
                   (j 0 n1))
            ;; the menu is given as a flat list, so we have to access it as a 2d array in row major order
            (let* ((item (ref2d items (list m n) (+ m0 i) (+ n0 j)))
                   (selectedp (and (= i (- r m0)) (= j (- c n0))))
                   ;; calculated position of the item
                   posy
                   posx)
              (if (typep menu 'window)
                  ;; if the menu is a menu-window or a menu-panel, assume the (0 0) position
                  (progn
                    (setq posy i
                          posx (nth j xs)))
                  ;; otherwise, the menu itself can be positioned
                  (progn
                    (setq posy (+ y (if tablep bt 0) (* i rg) (* i h))
                          posx (+ x (if tablep bl 0) (* j cg) (nth j xs)))))
              (move win posy posx)
              ;; save the position of the current item, to be used in update-cursor-position.
              (when selectedp
                (setf cpos (list posy posx)))
              (let ((fg-style (if style
                                  (getf style (if selectedp :selected-foreground :foreground))
                                  ;; default foreground style
                                  (if selectedp (list :attributes (list :reverse)) nil)))
                    (bg-style (if style
                                  (getf style (if selectedp :selected-background :background))
                                  ;; default background style
                                  (if selectedp (list :attributes (list :reverse)) nil))))
                ;; write an empty string as the background.
                (save-excursion win (add win #\space :style bg-style :n (nth j widths)))
                ;; display it in the window associated with the menu
                (add win
                     (format nil
                             (concatenate 'string
                                          (make-string item-padding-left :initial-element #\space)
                                          ;; "~v,,,' @A" to right-justify
                                          "~v,,,' A"
                                          (make-string item-padding-right :initial-element #\space))
                             (- (nth j widths) item-padding-left item-padding-right)
                             (format-menu-item menu item selectedp))
                     :style fg-style)))))
        (refresh win)))))

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
  (with-slots (children current-item-number grid-row grid-column region-start-row region-start-column menu-type) menu
    (setf current-item-number 0
          grid-row 0
          grid-column 0
          region-start-row 0
          region-start-column 0)
    (when (eq menu-type :checklist)
      (loop for i in children if (checkedp i) do (setf (checkedp i) nil)))))

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

(defun exit-menu-event-loop (menu)
  "Associate this function with an event to exit the menu event loop."
  (return-from-menu menu nil))

(defun checked-items (menu)
  "Take a menu, return a list of checked menu items."
  (loop for i in (items menu) if (checkedp i) collect i))

(defmethod value ((menu menu))
  "Return the value of the selected item."
  (value (current-item menu)))

(defmethod value ((checklist checklist))
  "Return the list of values of the checked items."
  (mapcar #'value (checked-items checklist)))

(defun accept-selection (menu)
  "Return the value of the currently selected item or all checked items."
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

(defun toggle-item-checkbox (menu)
  "Toggle the checked state of the current item, used in checkbox menus."
  (setf (checkedp (current-item menu)) (not (checkedp (current-item menu))))
  (draw menu))

(defun sync-collection-grid (obj)
  "Sync the position in 1D collection list with the yx position in a 2D grid."
  (with-slots (current-item-number grid-rows grid-columns grid-row grid-column) obj
    (setf current-item-number (sub2rmi (list grid-rows grid-columns) (list grid-row grid-column)))))

(defun sync-grid-collection (obj)
  "Set the 2D yx grid position from the 1D position in the collection list."
  (with-slots ((i current-item-number)
               (m grid-rows)
               (n grid-columns)
               (y grid-row)
               (x grid-column)) obj
    (setf y (car (rmi2sub (list m n) i))
          x (cadr (rmi2sub (list m n) i)))))

(defmethod move-left ((obj menu))
  ;; update the grid
  (call-next-method obj)
  ;; sync the 1D collection and the 2D grid
  (sync-collection-grid obj)
  ;; redraw the menu
  (draw obj))

(defmethod move-right ((obj menu))
  (call-next-method obj)
  (sync-collection-grid obj)
  (draw obj))

(defmethod move-up ((obj menu))
  (call-next-method obj)
  (sync-collection-grid obj)
  (draw obj))

(defmethod move-down ((obj menu))
  (call-next-method obj)
  (with-slots (current-item-number grid-rows grid-columns grid-row grid-column) obj
    (setf current-item-number
          (sub2rmi (list grid-rows grid-columns)
                   (list grid-row grid-column))))
  (draw obj))

;; all of these take two arguments: menu event
;; there is no :default action, all other events are ignored for menus.
(define-keymap menu-map
  ;; q doesnt return a value, just nil, i.e. in the case of a checklist, an empty list.
  (#\q 'exit-menu-event-loop)
  (#\x 'toggle-item-checkbox)
  (:up    'move-up)
  (:down  'move-down)
  (:left  'move-left)
  (:right 'move-right)
  ;; return the selected item or all checked items, then exit the menu like q.
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
