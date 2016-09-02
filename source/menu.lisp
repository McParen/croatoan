(in-package :de.anvi.croatoan)

;; menu
;; curses extension for programming menus
;; http://invisible-island.net/ncurses/man/menu.3x.html

(defun update-menu (menu event)
  "Take a menu and an event, update in-place the current item of the menu."
  ;; we need to make menu special in order to setf i
  (declare (special menu))
  (with-accessors ((i .current-item) (items .items)) menu
    (let ((n (length items)))
      (case event
        (:up   (setf i (mod (1- i) n)))
        (:down (setf i (mod (1+ i) n)))))))

(defun draw-menu (menu)
  "Draw the current state of menu on the screen, then refresh the menu window."
  (with-accessors ((i .current-item) (items .items) (title .title) (sub-win .sub-window)) menu
    (clear sub-win)
    (loop for j from 0 to (1- (length items))
       do
         (move sub-win j 0)
         (format sub-win "~A ~A" (if (= i j) ">" " ") (nth j items))
         (when (= i j)
           (move sub-win j 0)
           (change-attributes sub-win (.width sub-win) '() :color-pair (list :yellow :red))))
    ;; we have to explicitely touch the background win, because otherwise it wont get refreshed.
    (touch menu)
    (when title (add menu (format nil "~7:@<~A~>" title) :y 0 :x 2))
    ;;(box menu)
    (refresh menu)
    (refresh sub-win)))

;; display a menu, let the user select an item with up and down and confirm with enter,
;; return the selected item.
(defun select-item (menu)
  (draw-menu menu)
  (event-case (menu event)
    ((:up :down) (update-menu menu event) (draw-menu menu))
    (#\newline (return-from event-case (nth (.current-item menu) (.items menu))))
    ;; returns NIL
    (#\q (return-from event-case))))
