(in-package :de.anvi.croatoan)

;; menu
;; curses extension for programming menus
;; http://invisible-island.net/ncurses/man/menu.3x.html

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

(defun update-menu (menu event)
  "Take a menu and an event, update in-place the current item of the menu."
  ;; we need to make menu special in order to setf i in the passed menu object.
  (declare (special menu))
  (with-accessors ((item .current-item-number) (items .items) (layout .layout) (cyclic-selection .cyclic-selection)
                   (scrolled-layout .scrolled-layout) (scrolled-region-start .scrolled-region-start)) menu
    (let ((i (car  (rmi2sub layout item)))
          (j (cadr (rmi2sub layout item))))
      (if scrolled-layout
          ;; when scrolling is on, the menu is not cycled.
          (let ((m (car scrolled-layout))
                (n (cadr scrolled-layout))
                (m1 (car scrolled-region-start))
                (n1 (cadr scrolled-region-start)))
            (case event
              (:up    (if (> i 0) (decf i))
                      (when (< i m1) (decf m1)))
              (:down  (if (< i (1- (car layout))) (incf i))
                      (when (>= i (+ m1 m)) (incf m1)))
              (:left  (if (> j 0) (decf j))
                      (when (< j n1) (decf n1)))
              (:right (if (< j (1- (cadr layout))) (incf j))
                      (when (>= j (+ n1 n)) (incf n1))))
            (setf scrolled-region-start (list m1 n1)))
          ;; scrolling is off
          (let ((m (car  layout))
                (n (cadr layout)))
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
                  (:right (setf j (min (1+ j) (1- n))))))))
      (setf item (sub2rmi layout (list i j))))))
  
(defgeneric draw-menu (s)
  (:documentation "Draw a menu."))

(defmethod draw-menu ((menu menu-window))
  (with-accessors ((current-item-number .current-item-number) (mark .current-item-mark) (items .items) (layout .layout)
                   (scrolled-layout .scrolled-layout) (scrolled-region-start .scrolled-region-start)
                   (title .title) (border .border) (color-pair .color-pair) (len .max-item-length) (sub-win .sub-window)) menu
    (clear sub-win)
    (if scrolled-layout
        ;; when the menu is too big to be displayed at once, only a part
        ;; is displayed, and the menu can be scrolled
        ;; draw a menu with a scrolling layout enabled
        (let ((m (car scrolled-layout))
              (n (cadr scrolled-layout))
              (m1 (car scrolled-region-start))
              (n1 (cadr scrolled-region-start)))
          (loop for i from 0 to (1- m)
             do (loop for j from 0 to (1- n)
                   do
                      ;; the menu is given as a flat list, so we have to access it as a 2d array
                      ;; in row major order
                     (let ((item (sub2rmi layout (list (+ m1 i) (+ n1 j)))))
                       ;;(format menu "~A ~A," j n1)
                       (move sub-win i (* j len))
                       (format sub-win "~A~A"
                               ;; for the current item, draw the current-item-mark
                               ;; for all other items, draw a space
                               (if (= current-item-number item)
                                   mark
                                   (make-string (length mark) :initial-element #\space))
                               ;; then add the item
                               (typecase (nth item items)
                                 (string    (nth item items))
                                 (menu-item (.name (nth item items)))))
                       ;; change the attributes of the current item
                       (when (= current-item-number item)
                         (move sub-win i (* j len))
                         (change-attributes sub-win (+ len (length mark)) '(:reverse) ))))))
        ;; when there is no scrolling, and the whole menu is displayd at once
        ;; cycling is enabled.
        (let ((m (car layout))
              (n (cadr layout)))
          (loop for i from 0 to (1- m)
             do (loop for j from 0 to (1- n)
                   do
                     (let ((item (sub2rmi layout (list i j))))
                       (move sub-win i (* j len))
                       (format sub-win "~A~A"
                               (if (= current-item-number item)
                                   mark
                                   (make-string (length mark) :initial-element #\space))
                               (nth item items))
                       (when (= current-item-number item)
                         (move sub-win i (* j len))
                         (change-attributes sub-win (+ len (length mark)) '(:reverse) )))))))
    ;; we have to explicitely touch the background win, because otherwise it wont get refreshed.
    (touch menu)
    ;; draw the title only when we have a border too, because we draw the title on top of the border.
    (when (and border title)
      ;; "|~12:@<~A~>|"
      (flet ((make-title-string (len)
               (concatenate 'string "|~" (write-to-string (+ len 2)) ":@<~A~>|")))
        (add menu (format nil (make-title-string (length title)) title) :y 0 :x 2)))
    ;; todo: when we refresh a window with a subwin, we shouldnt have to refresh the subwin separately.
    ;; make refresh specialize on menu and decorated window in a way to do both.
    (refresh menu)
    (refresh sub-win)))

(defmethod draw-menu ((menu dialog-window))
  ;; first draw a menu
  (call-next-method)

  ;; then draw the message in the reserved space above the menu.
  (with-accessors ((message-text .message-text) (message-height .message-height)
                   (message-pad .message-pad) (coords .message-pad-coordinates)) menu
    ;; if there is text, and there is space reserved for the text, draw the text
    (when (and message-text (> message-height 0))
      (refresh message-pad
               0                   ;pad-min-y
               0                   ;pad-min-x
               (first  coords)     ;screen-min-y
               (second coords)     ;screen-min-x
               (third  coords)     ;screen-max-y
               (fourth coords))))) ;screen-max-x

(defun current-item (menu)
  "Return the currently selected item of the given menu."
  (nth (.current-item-number menu) (.items menu)))

;; test submenus, test this in t19c
(defun select-item (menu)
  "Display a menu, let the user select an item, return the selected item.

If the item is itself a menu, recursively display the sub menu."
  (setf (.visible menu) t)
  ;; TODO: how to better avoid refresh-stack when we have no submenus?
  (when *window-stack* (refresh-stack))
  (draw-menu menu)
  (event-case (menu event)
    ((:up :down :left :right) (update-menu menu event) (draw-menu menu))
    ;; ENTER either enters a submenu or returns a selected item.
    (#\newline
     (let ((item (current-item menu)))
       (typecase item
         ;; if we have just a normal string, just return it.
         (string
          ;; when an item is selected, hide the menu or submenu, then return the item
          (setf (.visible menu) nil)
          (when *window-stack* (refresh-stack))
          (return-from event-case item))
         ;; a more complex item can be a sub menu, or in future, a function.
         (menu-item
          ;; if the item is a menu, call select-item recursively on the sub-menu
          (let ((selected-item (select-item (.value item))))
            ;; if we exit the submenu with q without selecting anything, we go back to the parent menu.
            ;; we only exit the menu completely if something is selected.
            ;; for all this to work, we have to put the overlapping menu windows in the stack.
            (when selected-item
              ;; when a submenu returns non-nil, hide the parent menu too.
              ;; when the submenu is exited with q, it returns nil, and we go back to the parent menu.
              (setf (.visible menu) nil)
              (when *window-stack* (refresh-stack))
              (return-from event-case selected-item)))))))
    ;; returns NIL
    (#\q
     ;; when q is hit to exit a menu, hide the menu or submenu, then refresh the whole stack.
     (setf (.visible menu) nil)
     (when *window-stack* (refresh-stack))
     (return-from event-case nil))))
