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
    :type          (or string cons)
    :documentation "A string prefixed to the current item in the menu.")

   (tablep
    :initarg       :table
    :initform      nil
    :reader        tablep
    :type          boolean
    :documentation "If t, table row and column lines are drawn between the items.")

   (item-padding-top
    :initarg       :item-padding-top
    :initform      0
    :type          integer
    :documentation "Additional space added to the top of the item title, with the same background style.")

   (item-padding-bottom
    :initarg       :item-padding-bottom
    :initform      0
    :type          integer
    :documentation "Additional space added below the item title, with the same background style.")

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

   (align
    :initarg       :align
    :initform      :left
    :type          keyword
    :documentation
    "Set how the item titles and thus the menu columns are aligned.

Items can be aligned if they are shorter than the max-item-length or
the calculated column width.

Possible values are :left (default) or :right.")

   ;; see examples t19b2, t19b3.
   (draw-stack-p
    :initarg       :draw-stack
    :initform      t
    :reader        draw-stack-p
    :type          boolean
    :documentation
    "Redraw all menus in the stack when a submenu is quit/entered, so we see the whole stack.

If nil, only one direct parent/child is redrawn, so we move through the stack one by one.

At the moment, this setting applies only to stacks of simple (non-window) menus).")

   (max-item-length
    :initarg       :max-item-length
    :initform      15
    :accessor      max-item-length
    :type          integer
    :documentation
    "If the variable column width is nil, this is the default width of all items.

If the variable column width is t, the width of every column is calculated from
the item titles and the columns exceeding max item length are ellipsized or
truncated.")

   (ellipsis
    :initarg       :ellipsis
    :initform      #.(format nil "~C" (code-char #x2026))
    :type          string
    :documentation "Ellipsis/truncation string for too long item titles.

If an item title is longer than max-item-length shorten the title by
replacing the last characters with the ellipsis string.

If the ellipsis is an empty string, the title will simply be truncated.

The default value is the unicode ellipsis character (of length 1),
#\horizontal_ellipsis (code point #x2026)."))

  (:default-initargs :keymap 'menu-map)
  (:documentation
   "A menu is a list of items displayed in a grid that can be selected by the user.

Item types can be strings, symbols, numbers, other menus or callback functions."))

;; init for menus which aren't menu windows
(defmethod initialize-instance :after ((menu menu) &key item-padding)
  (with-slots (children grid-rows grid-columns region-rows region-columns region-start-row region-start-column
               tablep grid-row-gap grid-column-gap) menu
    (setf region-start-row 0
          region-start-column 0)

    ;; item-padding is either an integer, or a list (top-bottom left-right) or (top bottom left right).
    (when item-padding
      (with-slots ((pt item-padding-top) (pb item-padding-bottom) (pl item-padding-left) (pr item-padding-right)) menu
        (typecase item-padding
          (list
           (case (length item-padding)
             (4 (setf pt (nth 0 item-padding)
                      pb (nth 1 item-padding)
                      pl (nth 2 item-padding)
                      pr (nth 3 item-padding)))
             (2 (setf pt (nth 0 item-padding)
                      pb (nth 0 item-padding)
                      pl (nth 1 item-padding)
                      pr (nth 1 item-padding)))))
          (integer
           (setf pt item-padding
                 pb item-padding
                 pl item-padding
                 pr item-padding)))))

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
  "content width = column-widths + item marks + item padding + checkbox"
  (with-accessors ((len max-item-length)
                   (variable-column-width-p variable-column-width-p)
                   (items items)) obj
    (with-slots (scrolling-enabled-p
                 menu-type
                 (cmark current-item-mark)
                 (ipl item-padding-left)
                 (ipr item-padding-right)
                 (m  grid-rows)
                 (n  grid-columns)
                 (cg grid-column-gap)
                 (m0 region-start-row)
                 (n0 region-start-column)
                 (m1 region-rows)
                 (n1 region-columns)) obj
      ;; r0, c0 region start; r1, c1 region rows, cols
      (destructuring-bind (r0 c0 r1 c1)
          (if scrolling-enabled-p (list m0 n0 m1 n1) (list 0 0 m n))
        (if variable-column-width-p
            ;; variable column width
            (let ((widths (mapcar (lambda (i)
                                    ;; if i > len, truncate (done in format-menu-item)
                                    (+ (if (<= i len) i len)
                                       ipl ipr
                                       (typecase cmark
                                         (string (length cmark))
                                         (list (+ (length (car cmark))
                                                  (length (cadr cmark)))))
                                       (if (eq menu-type :checklist) 4 0))) ; "[ ] "
                                   (subseq (column-widths items (list m n)) c0 (+ c0 c1))))
                   (gaps (if (plusp cg) (* (1- c1) cg) 0)))
              ;; width of the content of a menu is a sum of column widths and gaps.
              (+ gaps
                 (loop for i in widths sum i)))
            ;; fixed column width
            (let ((widths (* c1 (+ len ipl ipr
                                   (typecase cmark
                                     (string (length cmark))
                                     (list (+ (length (car cmark))
                                              (length (cadr cmark)))))
                                   (if (eq menu-type :checklist) 4 0)))) ; "[ ] "
                   ;; if a table is drawn, we have n-1 row lines
                   (gaps (if (plusp cg) (* (1- c1) cg) 0)))
              (+ widths gaps)))))))

(defmethod height ((obj menu))
  (with-accessors ((m visible-grid-rows)) obj
    (with-slots ((rg grid-row-gap)
                 item-padding-top
                 item-padding-bottom) obj
      (let* ((gaps (if (plusp rg) (* (1- m) rg) 0)))
        ;; the height of the menu is a sum of m items of height 1
        ;; m-1 gaps between the items, and m top and bottom paddings.
        (+ m
           gaps
           (* m item-padding-top)
           (* m item-padding-bottom))))))

(defmethod visible-width ((obj menu))
  "visible width = content width + element padding

In contrast to element padding, the item padding is part of the
content, so it is accounted for by (width menu)."
  (with-accessors ((w width) (borderp borderp) (tablep tablep) (bl border-width-left) (br border-width-right) (pl padding-left) (pr padding-right)) obj
    (cond (tablep
           ;; for tables, the outer border is part of the content
           (+ w bl br))
          (t
           ;; padding (inside the border) is part of the content
           (+ w pl pr)))))

(defmethod external-width ((obj menu))
  "external-width = content width + (padding + border-width)
                  = visible-width            + border-width"
  (with-accessors ((w width) (borderp borderp) (tablep tablep) (bl border-width-left) (br border-width-right) (pl padding-left) (pr padding-right)) obj
    (cond (tablep
           ;; for tables, item padding is part of the content
           (+ w bl br))
          (borderp
           (+ w pl pr bl br))
          (t
           (+ w pl pr)))))

(defmethod visible-height ((obj menu))
  (with-accessors ((h height) (borderp borderp) (tablep tablep) (bt border-width-top) (bb border-width-bottom) (pt padding-top) (pb padding-bottom)) obj
    (cond (tablep
           (+ h bt bb))
          (t
           (+ h pt pb)))))

(defmethod external-height ((obj menu))
  (with-accessors ((h height) (borderp borderp) (tablep tablep) (bt border-width-top) (bb border-width-bottom) (pt padding-top) (pb padding-bottom)) obj
    (cond (tablep
           (+ h bt bb))
          (borderp
           (+ h pt pb bt bb))
          (t
           (+ h pt pb)))))

(defclass menu-window (menu window)
  ()
  (:default-initargs :keymap 'menu-window-map)
  (:documentation "A menu-window is an extended window displaying a menu in its sub-window."))

(defmethod initialize-instance :after ((win menu-window) &key color-pair)
  (with-slots (winptr children type height width (y position-y) (x position-x) borderp tablep
               border-width-top border-width-bottom border-width-left border-width-right
               grid-rows grid-columns region-rows region-columns max-item-length current-item-mark fgcolor bgcolor) win
    ;; only for menu windows
    (when (eq (type-of win) 'menu-window)
      ;; if no layout was given, use a vertical list (n 1)
      (unless grid-rows (setf grid-rows (length children)))
      (unless grid-columns (setf grid-columns 1))

      ;; if height and width of the _window_ are not given as
      ;; initargs but calculated from menu data
      (unless height (setf height (external-height win)))
      (unless width  (setf width  (external-width win)))

      (setf winptr (ncurses:newwin height width y x))

      (cond ((or fgcolor bgcolor)
             (set-color-pair winptr (color-pair win))
             (setf (background win) (make-instance 'complex-char :color-pair (color-pair win))))
            ;; when a color-pair is passed as a keyword
            (color-pair
             ;; set fg and bg, pass to ncurses
             (setf (color-pair win) color-pair
                   (background win) (make-instance 'complex-char :color-pair color-pair)))))))

;; although it is not a stream, we will abuse close to close a menu's window and subwindow, which _are_ streams.
(defmethod close ((stream menu-window) &key abort)
  (declare (ignore abort))
  (ncurses:delwin (winptr stream)))

(defclass checklist (menu)
  ()
  (:default-initargs
   :menu-type :checklist
   :keymap 'checklist-map)
  (:documentation "A checklist is a multi-selection menu with checkable items."))

(defclass menu-item (checkbox)
  ((value
    :type          (or symbol keyword string menu menu-window function number)
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
  (with-accessors ((ew external-width) (eh external-height) (vw visible-width) (vh visible-height) (tablep tablep)
                   (bt border-width-top) (bl border-width-left) (borderp borderp) (selectedp selectedp) (style style)) obj
    (let* ((bg-style (if selectedp
                         (getf style :selected-background)
                         (getf style :background)))
           (border-style (if selectedp
                             (getf style :selected-border)
                             (getf style :border))))
      ;; the position of the menu in a menu-window is always (0 0).
      (let ((y 0)
            (x 0))
        (cond (tablep
               (fill-rectangle obj (apply #'make-instance 'complex-char bg-style) y x vh vw))
              (borderp
               (fill-rectangle obj (apply #'make-instance 'complex-char border-style) y x eh ew)
               (fill-rectangle obj (apply #'make-instance 'complex-char bg-style) (+ y bt) (+ x bl) vh vw))
              (t
               (fill-rectangle obj (apply #'make-instance 'complex-char bg-style) y x vh vw)))))))

(defun draw-table-top (win widths &key style)
  "Draw the top line of a table at the current position using ANSI drawing characters.

The top line is only drawn when border is t."
  (add-char win :upper-left-corner :style style)
  (dolist (n (butlast widths))
    (add-char win :horizontal-line :n n :style style)
    (add-char win :tee-pointing-down :style style))
  (add-char win :horizontal-line :n (car (last widths)) :style style)
  (add-char win :upper-right-corner :style style))

(defun draw-table-row-separator (win widths borderp &key style)
  "Draw the horizontal line between table cells."
  ;; the first char is only drawn for the border.
  (when borderp
    (crt:add-char win :tee-pointing-right :style style))
  ;; draw -- then + for every column.
  (dolist (n (butlast widths))
    (crt:add-char win :horizontal-line :n n :style style)
    (crt:add-char win :crossover-plus :style style))
  ;; finally, thaw only -- for the last column.
  (add-char win :horizontal-line :n (car (last widths)) :style style)
  ;; the last char is only drawn for the border.
  (when borderp
    (add-char win :tee-pointing-left :style style)))

(defun draw-table-bottom (win widths &key style)
  "Draw the top line of a table at the current position using ANSI drawing characters.

The bottom line is only drawn when border is t."
  (add-char win :lower-left-corner :style style)
  (dolist (n (butlast widths))
    (add-char win :horizontal-line :n n :style style)
    (add-char win :tee-pointing-up :style style))
  (add-char win :horizontal-line :n (car (last widths)) :style style)
  (add-char win :lower-right-corner :style style))

(defun draw-table-lines (win y x m n rg cg bt bl pt pb widths heights borderp &key style)
  ;; y position
  ;; x position
  ;; m number of displayed rows
  ;; n number of displayed columns
  ;; rg row gap
  ;; cg column-gap
  ;; bt width of the top border, currently assumed = 1
  ;; bl width of the left border, currently assumed = 1
  (assert (and (<= bt 1) (<= bl 1)))
  ;; pt padding top
  ;; pb padding bottom
  ;; witdhs, leights - list of widths of columns

  ;;; draw the n1+1 vertical lines (plain, without endings and crossings)
  ;; first vline (left border)
  (when borderp
    (draw-vline win
                ;; the first line is the top border, we start below the border.
                (+ y bt)
                x
                ;; length of the vertical lines
                (+ m                  ; rows
                   (- m 1)            ; row separators
                   (* m (+ pt pb)))   ; top and bottom padding
                nil
                :style style))

  ;; n-1 column separator vlines
  (dotimes (j (1- n))
    (draw-vline win
                (+ y bt)
                (+ x bl (* j cg) (loop for i from 0 to j sum (nth i widths)))
                (+ (1- (* m 2))
                   (* m (+ pt pb)))
                nil
                :style style))

  ;; last vline (right border)
  (when borderp
    (draw-vline win
                ;; the first line is the border top, we start below the border.
                (+ y bt)
                (+ x
                   bl
                   (* (1- n) cg)
                   (reduce #'+ widths))
                ;; length of the vertical lines
                (+ m                  ; rows
                   (- m 1)            ; row separators
                   (* m (+ pt pb)))   ; top and bottom padding
                nil
                :style style))

  ;;; draw m+1 horizontal lines (with endings and crossings)

  ;; they have to be drawn _after_ the vertical lines, because the
  ;; drawn vertical lines do not contain the crossover chars.

  ;; top horizontal line of the table, only drawn if borderp is t.
  (when borderp
    (move win y x)
    (draw-table-top win widths :style style))

  ;; m-1 row separators
  (dotimes (i (- m 1))
    (move win
          (+ y bt (* i rg) (loop for j from 0 to i sum (nth j heights)))
          x)
    (draw-table-row-separator win widths borderp :style style))

  ;; bottom line, only drawn if borderp is t.
  (when borderp
    (move win (+ y (* 2 m) (* m (+ pt pb))) x)
    (draw-table-bottom win widths :style style)))

(defun format-menu-item (menu item selectedp width)
  "Take a menu and return item item-number as a properly formatted string.

If the menu is a checklist, return [ ] or [X] at the first position.

If a mark is set for the current item, display the mark at the second position.
Display the same number of spaces for other items.

At the third position, display the item given by item-number."
  (with-accessors ((type menu-type)
                   (cmark current-item-mark)) menu
    (with-slots (align
                 ellipsis
                 item-padding-left
                 item-padding-right) menu
      ;; return as string
      (format nil
              ;; "~A~A~A~v,,,' @A~A~A" :right
              ;; "~A~A~A~v,,,' A~A~A"  :left
              (concatenate 'string
                           "~A~A~A~v,,,' "
                           (case align (:left "") (:right "@"))
                           "A~A~A")
              (make-string item-padding-left :initial-element #\space)
              ;; two types of menus: :selection or :checklist
              ;; show the checkbox before the item in checklists
              (if (eq type :checklist)
                  (if (checkedp item) "[X] " "[ ] ")
                  "")

              ;; for the current item, draw the current-item-mark
              ;; for all other items, draw spaces
              (let ((mark (typecase cmark
                            (string cmark)
                            (list (car cmark)))))
                (if selectedp
                    mark
                    (make-string (length mark) :initial-element #\space)))

              ;; the width of the item title, given by the set column width
              width
              ;; then add the item title
              (let* ((title (format-title item)))
                (if (> (length title) width)
                    (text-ellipsize title width :truncate-string ellipsis)
                    title))

              ;; if the mark is a list like ("> " "< ") draw the second mark
              ;; after the menu item.
              (let ((mark (typecase cmark
                            (string "")
                            (list (cadr cmark)))))
                (if selectedp
                    mark
                    (make-string (length mark) :initial-element #\space)))

              (make-string item-padding-right :initial-element #\space)))))

(defmethod external-width ((obj menu-item))
  (length (title obj)))

;; draws to any window, not just to a sub-window of a menu-window.
(defun draw-menu (win y x menu)
  "Draw the menu to the window."
  (with-slots (scrolling-enabled-p
               variable-column-width-p
               menu-type
               (ipt item-padding-top)
               (ipb item-padding-bottom)
               (ipl item-padding-left)
               (ipr item-padding-right)
               (pl padding-left)
               (pt padding-top)
               (cmark current-item-mark)
               (cpos current-item-position)
               (len max-item-length)
               (items children)
               borderp
               tablep
               (r grid-row)
               (c grid-column)
               (m grid-rows)
               (n grid-columns)
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
      ;; when the menu is too big to be displayed at once, only a part
      ;; is displayed in a "region", and the menu can be scrolled
      ;; r0, c0 region start;
      ;; r1, c1 region rows, cols
      (destructuring-bind (r0 c0 r1 c1)
          (if scrolling-enabled-p (list m0 n0 m1 n1) (list 0 0 m n))
        (let* (;; widths of the item titles only
               (widths- (if variable-column-width-p
                            (subseq (column-widths items (list m n)) c0 (+ c0 c1))
                            (loop for i below c1 collect len)))
               ;; widths plus item padding, mark and checkbox
               (widths (mapcar (lambda (i)
                                 ;; if i > len, truncate (done in format-menu-item)
                                 (+ (if (<= i len) i len)
                                    ipl ipr
                                    (typecase cmark
                                      (string (length cmark))
                                      (list (+ (length (car cmark))
                                               (length (cadr cmark)))))
                                    (if (eq menu-type :checklist) 4 0)))
                               widths-))
               (xs (cumsum-predecessors widths))
               ;; height of one item is 1 and the padding.
               (h (+ 1 ipt ipb))
               (heights (loop for i below r1 collect h))
               (border-style (if (selectedp menu)
                                 (getf style :selected-border)
                                 (getf style :border))))
          ;; draw the external border when table is nil.
          ;; a table draws its own "connected" border.
          (when (and borderp (not tablep))
            (if (typep menu 'window)
                (draw-rectangle win y x
                                ;; If window dimensions have been explicitely given, override the calculated
                                ;; menu dimensions. This allows to draw a "menu bar", which can be wider
                                ;; than the sum of its items, see t19e2.
                                (if (slot-value win 'height) (slot-value win 'height) (external-height menu))
                                (if (slot-value win 'width)  (slot-value win 'width)  (external-width menu))
                                :style border-style)
                ;; if menu is a simple menu, we are not able to set the width manually
                ;; to a value different than the calculated width
                (draw-rectangle win y x
                                (external-height menu)
                                (external-width menu)
                                :style border-style)))

          ;; draw table lines and if border is t, the table border.
          (when tablep
            ;; to draw table lines between the grid cells, a grid gap is required.
            (draw-table-lines win y x r1 c1 rg cg bt bl ipt ipb widths heights borderp
                              :style border-style))

          (dogrid ((i 0 r1)
                   (j 0 c1))
            ;; the menu is given as a flat list, so we have to access it as a 2d array in row major order
            (let* ((item (ref2d items (list m n) (+ r0 i) (+ c0 j)))
                   (selectedp (and (= i (- r r0)) (= j (- c c0))))
                   ;; calculated position of the item
                   (posy (+ y
                            (if borderp bt 0)
                            (if (and borderp (not tablep)) pt 0)
                            (* i rg)
                            (* i h)))
                   (posx (+ x
                            (if borderp bl 0)
                            (if (and borderp (not tablep)) pl 0)
                            (* j cg)
                            (nth j xs)))
                   (fg (if style
                           (getf style (if selectedp :selected-foreground :foreground))
                           ;; default selected-foreground style
                           (if selectedp
                               (list :attributes (list :reverse))
                               nil))))

              ;; save the position of the current item, to be used in update-cursor-position.
              (when selectedp
                (setf cpos (list posy posx)))

              ;; draw the top padding lines
              (when (plusp ipt)
                (dotimes (k ipt)
                  (move win (+ posy k) posx)
                  (add win #\space :style fg :n (nth j widths))))

              (move win (+ posy ipt) posx)
              ;; display the item in the window associated with the menu
              (add win (format-menu-item menu item selectedp
                                         (if (<= (nth j widths-) len)
                                             (nth j widths-)
                                             len))
                   :style fg)

              ;; draw the bottom padding lines
              (when (plusp ipb)
                (dotimes (k ipb)
                  (move win (+ posy ipt 1 k) posx)
                  (add win #\space :style fg :n (nth j widths))))))))))
  (refresh win))

(defmethod draw ((menu menu))
  "Draw the menu to its associated window."
  (draw-menu (window menu)
             (car (widget-position menu))
             (cadr (widget-position menu))
             menu)
  ;; when menu is a part of a form: update-cursor-position = place the cursor on the current item
  ;; if the menu is a checklist, place the cursor inside the [_], like it is done with a single checkbox.
  (update-cursor-position menu))

(defmethod draw ((menu menu-window))
  "Draw the menu to position (0 0) of its window."
  (draw-menu menu
             0
             0
             menu))

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
      (unless (stack-empty-p *menu-stack*)
        (stack-pop *menu-stack*))

      ;; if the menu is not a menu-window, clear the parent window.
      (unless (stack-empty-p *menu-stack*)
        (stack-pop *menu-stack*)
        (clear (window menu))
        (when (draw-stack-p menu)
          (mapc #'draw (reverse (items *menu-stack*))))
        (refresh (window menu))))

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
     ;; when one or more items have been checked,
     ;; return the checked items (not their values).
     (when (checked-items menu)
       (return-from-menu menu (checked-items menu))))

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

         ;; if the item is a menu (and thus also a menu-window), recursively select an item from that submenu
         ((or (typep val 'menu)
              (typep val 'menu-window))

          ;; when the stack is not drawn, clear the parent menu before drawing the sub menu
          (unless (draw-stack-p val)
            (clear (window menu)))

          (let ((selected-item (select val)))
            ;; when we have more than one menu in one window, redraw the parent menu after we return from the submenu.
            (when (or (eq (type-of val) 'menu)
                      (eq (type-of val) 'checklist))
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
  (with-slots (grid-rows grid-columns grid-row grid-column) obj
    (setf (current-item-number obj)
          (sub2rmi (list grid-rows grid-columns)
                   (list grid-row grid-column)))))

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
  (sync-collection-grid obj)
  (draw obj))

;; all of these take two arguments: menu event
;; there is no :default action, all other events are ignored for menus.
(define-keymap menu-map
  (:up    'move-up)
  (:down  'move-down)
  (:left  'move-left)
  (:right 'move-right))

(define-keymap checklist-map
  (#\x    'toggle-item-checkbox)
  (:up    'move-up)
  (:down  'move-down)
  (:left  'move-left)
  (:right 'move-right))

(define-keymap menu-window-map
  ;; q doesnt return a value, just nil, i.e. in the case of a checklist, an empty list.
  (#\q    'exit-menu-event-loop)
  (#\x    'toggle-item-checkbox)
  (:up    'move-up)
  (:down  'move-down)
  (:left  'move-left)
  (:right 'move-right)
  ;; return the selected item or all checked items, then exit the menu like q.
  (#\newline 'accept-selection))

(defgeneric select (obj))

(defmethod select ((obj menu))
  (stack-push obj *menu-stack*)
  (draw obj)
  (run-event-loop obj))

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
