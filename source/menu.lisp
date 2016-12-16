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
  (with-accessors ((item .current-item) (items .items) (layout .layout)) menu
    (let (;(l (length items))
          (m (car  layout))
          (n (cadr layout))
          (i (car  (rmi2sub layout item)))
          (j (cadr (rmi2sub layout item))))
      (case event
        (:up    (setf i (mod (1- i) m)))
        (:down  (setf i (mod (1+ i) m)))
        (:left  (setf j (mod (1- j) n)))
        (:right (setf j (mod (1+ j) n))))
      (setf item (sub2rmi layout (list i j))))))

(defgeneric draw-menu (s)
  (:documentation "Draw a menu."))

(defmethod draw-menu ((menu menu-window))
  "Draw the current state of menu on the screen, then refresh the menu window."
  (with-accessors ((current-item .current-item) (mark .current-item-mark) (items .items) (layout .layout) (title .title)
                   (border .border) (len .max-item-length) (sub-win .sub-window)) menu
    (clear sub-win)
    (let ((m (car layout))
          (n (cadr layout)))
      (loop for i from 0 to (1- m)
         do (loop for j from 0 to (1- n)
               do
                 (let ((item (sub2rmi layout (list i j))))
                   (move sub-win i (* j len))
                   (format sub-win "~A~A"
                           (if (= current-item item)
                               mark
                               (make-string (length mark) :initial-element #\space))
                           (nth item items))
                   (when (= current-item item)
                     (move sub-win i (* j len))
                     (change-attributes sub-win (+ len (length mark)) '() :color-pair (list :yellow :red)))))))
    ;; we have to explicitely touch the background win, because otherwise it wont get refreshed.
    (touch menu)
    ;; draw the title only when we have a border too, because we draw the title on top of the border.
    (when (and border title)
      ;; "|~12:@<~A~>|"
      (flet ((make-title-string (len)
               (concatenate 'string "|~" (write-to-string (+ len 2)) ":@<~A~>|")))
        (add menu (format nil (make-title-string (length title)) title) :y 0 :x 2)))

    ;;(box menu)
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

;; display a menu, let the user select an item with up and down and confirm with enter,
;; return the selected item.
(defun select-item (menu)
  (draw-menu menu)
  (event-case (menu event)
    ((:up :down :left :right) (update-menu menu event) (draw-menu menu))
    (#\newline (return-from event-case (nth (.current-item menu) (.items menu))))
    ;; returns NIL
    (#\q (return-from event-case))))
