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
  (with-accessors ((current-item-number .current-item-number) (current-item .current-item) (items .items)
                   (cyclic-selection .cyclic-selection) (layout .layout) (scrolled-layout .scrolled-layout)
                   (scrolled-region-start .scrolled-region-start)) menu
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
  (with-accessors ((items .items)
                   (checklist .checklist)
                   (type .type)
                   (current-item-number .current-item-number)
                   (current-item-mark .current-item-mark)) menu
    ;; return as string
    (format nil "~A~A~A"
            ;; two types of menus: :selection or :checklist
            ;; show the checkbox before the item in checklists
            (if (eq type :checklist)
                (if (.checked (nth item-number items)) "[X] " "[ ] ")
                "")
            
            ;; for the current item, draw the current-item-mark
            ;; for all other items, draw a space
            (if (= current-item-number item-number)
                current-item-mark
                (make-string (length current-item-mark) :initial-element #\space))
            
            ;; then add the item name
            (.name (nth item-number items)) )))

(defun draw-menu-item (win menu item-number i j)
  "Draw the item given by item-number at item position i,j in the sub-window of the menu."
  (with-accessors ((current-item-number .current-item-number)
                   (max-item-length .max-item-length)) menu
    (move win i (* j max-item-length))

    ;; format the item text
    (let ((item-text (format-menu-item menu item-number)))
      ;; display it in the sub-window of the menu
      (format win item-text))
    
    ;; if the item is the current item, change its attributes
    ;; TODO: dont use change-attributes, add the correct attributes with add-string.
    (when (= item-number current-item-number)
      (move win i (* j max-item-length))
      (change-attributes win max-item-length '(:reverse) ))))

;; draws to any window, not to a sub-window of a menu-window.
(defmethod draw (window (menu menu))
  "Draw the menu to the window."
  (with-accessors ((layout .layout) (scrolled-layout .scrolled-layout) (scrolled-region-start .scrolled-region-start)) menu
    (clear window)
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

(defgeneric draw-menu (s)
  (:documentation "Draw a menu."))

(defmethod draw-menu ((menu menu-window))
  "Draw the menu-window."
  (with-accessors ((title .title) (border .border) (sub-win .sub-window)) menu
    ;; draw the menu to the sub-window
    (draw sub-win menu)
    ;; we have to explicitely touch the background win, because otherwise it wont get refreshed.
    (touch menu)
    ;; draw the title only when we have a border too, because we draw the title on top of the border.
    (when (and border title)
      ;; make a format template depending on the length of the title.
      ;; "|~12:@<~A~>|"
      (flet ((make-title-string (len)
               (concatenate 'string "|~" (write-to-string (+ len 2)) ":@<~A~>|")))
        (add menu (format nil (make-title-string (length title)) title) :y 0 :x 2)))
    ;; todo: when we refresh a window with a subwin, we shouldnt have to refresh the subwin separately.
    ;; make refresh specialize on menu and decorated window in a way to do both.
    (refresh menu)))

;; TODO: rename to draw. draw-menu is wrong, since it is a dialog, not really a menu.
(defmethod draw-menu ((menu dialog-window))
  ;; first draw a menu
  ;; TODO: describe what exactly is drawn here and what in the parent method.
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

(defun select-item (menu)
  "Display a menu, let the user select an item, return the selected item.

If the item is itself a menu, recursively display the sub menu."
  (with-accessors ((items .items) (current-item .current-item)) menu
    (setf (.visible menu) t)
    ;; TODO: how to better avoid refresh-stack when we have no submenus?
    (when *window-stack* (refresh-stack))
    (draw-menu menu)

    (event-case (menu event)
      ((:up :down :left :right)
       (update-menu menu event)
       (draw-menu menu))

      ;; x toggles whether the item is checked or unchecked (if menu type is a checklist)
      ((#\x #\space)
       (setf (.checked current-item) (not (.checked current-item)))
       (draw-menu menu))

      ;; ENTER either enters a submenu or returns a selected item, the menu is not redrawn.
      (#\newline
       (case (.type menu)
         (:checklist
          ;; all checked items in a list.
          (return-from select-item (loop for i in items if (.checked i) collect i)))

         (:selection
          ;; if the item is a string or symbol, just return it.
          (cond ((or (typep (.value current-item) 'string)
                     (typep (.value current-item) 'symbol))
                 (setf (.visible menu) nil)
                 (when *window-stack* (refresh-stack))
                 (return-from event-case (.value current-item)))

                ;; if the item is a menu, recursively select an item from that submenu
                ((typep (.value current-item) 'menu)
                 (let ((selected-item (select-item (.value current-item))))
                   (when selected-item
                     (setf (.visible menu) nil)
                     (when *window-stack* (refresh-stack))
                     (return-from event-case selected-item))))) )))

    ;; quitting a menu by pressing q just returns NIL
    (#\q
     ;; when q is hit to exit a menu, hide the menu or submenu, then refresh the whole stack.
     (setf (.visible menu) nil)
     (when *window-stack* (refresh-stack))
     (return-from event-case nil)))))

(defun return-from-menu (menu return-value)
  "Set menu to invisible, refresh the window stack, return the value from select."
  (setf (.visible menu) nil)
  (when *window-stack* (refresh-stack))
  (throw 'event-loop return-value))

(defun exit-menu-event-loop (menu-window event)
  "Associate this function with an event to exit the menu event loop."
  (return-from-menu menu-window nil))

;; TODO: for items of other types, return the item object.
(defun accept-selection (menu event)
  "Return the value of the currently selected item or all checked items."
  (declare (ignore event))
  (case (.type menu)
    (:checklist
     ;; return all checked items (not their values) in the item list.
     (throw 'event-loop (loop for i in (.items menu) if (.checked i) collect i)))
    (:selection
     (let ((val (.value (.current-item menu))))
       (cond
         ;; if the item is a string or symbol, just return it.
         ((or (typep val 'string)
              (typep val 'symbol))
          (return-from-menu menu val))
         ;; if the item is a menu, recursively select an item from that submenu
         ((typep val 'menu)
          (let ((selected-item (select val)))
            (when selected-item
              (return-from-menu menu selected-item)))) )))))

(defun update-redraw-menu (menu event)
  "Update the menu after an event, the redraw the menu."
  (update-menu menu event)
  (draw-menu menu))

(defun toggle-item-checkbox (menu event)
  (setf (.checked (.current-item menu)) (not (.checked (.current-item menu))))
  (draw-menu menu))

;; window event &optional args
(add-keymap :menu-default-keymap
  (make-keymap
   ;; q doesnt return a value, just nil, i.e. in the case of a checklist, an empty list.
   #\q       'exit-menu-event-loop

   #\x       'toggle-item-checkbox

   :up       'update-redraw-menu
   :down     'update-redraw-menu
   :left     'update-redraw-menu
   :right    'update-redraw-menu

   ;; there is no :default action, all other events are ignored for menus.

   ;; return the selected item or all checked items, then exit the menu like q.
   #\newline 'accept-selection))

(defun select (menu)
  "Display the menu, let the user select an item, return the selected item.

If the item is a menu object, recursively display the sub menu."
  (setf (.visible menu) t)
  (when *window-stack* (refresh-stack))
  (draw-menu menu)

  ;; if the user didnt add any event handlers, add the default keymap.
  (unless (.event-handlers menu)
    (setf (.event-handlers menu) (get-keymap :menu-default-keymap)))

  ;; here we can pass the menu to run-event-loop because it is a menu-window.
  ;; all handler functions have to accept window and event as arguments.
  ;; the return value of select is the return value of run-event-loop
  ;; is the value thrown to the catch tag 'event-loop.
  (run-event-loop menu))
