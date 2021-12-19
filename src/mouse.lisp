(in-package :de.anvi.croatoan)

(defparameter *mouse-button-event-bitmask-alist*
  '((:released       . #o01) ; 00001
    (:pressed        . #o02) ; 00010
    (:clicked        . #o04) ; 00100
    (:double-clicked . #o10) ; 01000
    (:triple-clicked . #o20) ; 10000
    (:reserved-event . #o40))) ; unused

;; buttons 1-5
(defmacro mouse-bitmask (button event)
  (let ((event-bitmasks
         '((:released       . #o01)
           (:pressed        . #o02)
           (:clicked        . #o04)
           (:double-clicked . #o10)
           (:triple-clicked . #o20)
           (:reserved-event . #o40)))
        ;; NCURSES_MOUSE_VERSION 1 => 6
        ;; NCURSES_MOUSE_VERSION 2 => 5 (currently used) for ncurses 6.3, 5-button mice and mouse wheels
        (offset 5))
    (cond ((integerp event) `(ash ,event (* ,offset (- ,button 1))))
          ((symbolp event)
           (let ((mask (cdr (assoc event event-bitmasks))))
             `(ash ,mask (* ,offset (- ,button 1))))))))

(defparameter *mouse-event-bitmask-alist*
  `((:button-1-released       . ,(mouse-bitmask 1 :released))
    (:button-1-pressed        . ,(mouse-bitmask 1 :pressed))
    (:button-1-clicked        . ,(mouse-bitmask 1 :clicked))
    (:button-1-double-clicked . ,(mouse-bitmask 1 :double-clicked))
    (:button-1-triple-clicked . ,(mouse-bitmask 1 :triple-clicked))
    ;;(:button-1-reserved-event . ,(mouse-bitmask 1 :reserved-event))
    (:button-2-released       . ,(mouse-bitmask 2 :released))
    (:button-2-pressed        . ,(mouse-bitmask 2 :pressed))
    (:button-2-clicked        . ,(mouse-bitmask 2 :clicked))
    (:button-2-double-clicked . ,(mouse-bitmask 2 :double-clicked))
    (:button-2-triple-clicked . ,(mouse-bitmask 2 :triple-clicked))
    ;;(:button-2-reserved-event . ,(mouse-bitmask 2 :reserved-event))
    (:button-3-released       . ,(mouse-bitmask 3 :released))
    (:button-3-pressed        . ,(mouse-bitmask 3 :pressed))
    (:button-3-clicked        . ,(mouse-bitmask 3 :clicked))
    (:button-3-double-clicked . ,(mouse-bitmask 3 :double-clicked))
    (:button-3-triple-clicked . ,(mouse-bitmask 3 :triple-clicked))
    ;;(:button-3-reserved-event . ,(mouse-bitmask 3 :reserved-event))
    (:button-4-released       . ,(mouse-bitmask 4 :released))
    (:button-4-pressed        . ,(mouse-bitmask 4 :pressed))
    (:button-4-clicked        . ,(mouse-bitmask 4 :clicked))
    (:button-4-double-clicked . ,(mouse-bitmask 4 :double-clicked))
    (:button-4-triple-clicked . ,(mouse-bitmask 4 :triple-clicked))
    ;;(:button-4-reserved-event . ,(mouse-bitmask 4 :reserved-event))
    (:button-5-released       . ,(mouse-bitmask 5 :released))
    (:button-5-pressed        . ,(mouse-bitmask 5 :pressed))
    (:button-5-clicked        . ,(mouse-bitmask 5 :clicked))
    (:button-5-double-clicked . ,(mouse-bitmask 5 :double-clicked))
    (:button-5-triple-clicked . ,(mouse-bitmask 5 :triple-clicked))
    (:button-ctrl             . ,(mouse-bitmask 6 #o01))
    (:button-shift            . ,(mouse-bitmask 6 #o02))
    (:button-alt              . ,(mouse-bitmask 6 #o04))
    (:report-mouse-position   . ,(mouse-bitmask 6 #o10))))

;; (:all-mouse-events        . ,(- (mouse-bitmask 5 #o10) 1)

;; take a unsigned long integer representing a bitmask of mouse events,
;; return a list of mouse event keywords.
(defun bitmask-to-keyword (bitmask)
  (loop for i in (mapcar #'car *mouse-event-bitmask-alist*)
     if (logtest bitmask (cdr (assoc i *mouse-event-bitmask-alist*))) return i))
;; use collect to catch more than 1 event at once.
;; (format scr "~32,'0b" bitmask)

(defun keyword-to-bitmask (keys)
  "Take a list of mouse event keywords, return a logiored bitmask."
  (apply #'logior (mapcar #'(lambda (x) (cdr (assoc x *mouse-event-bitmask-alist*)))
                          keys)))

(defun set-mouse-event (keyword-list)
  "Take a list of mouse events, activate tracking of those events.

Returns an integer bitmask. An empty list turns off mouse tracking."
  (ncurses:mousemask (keyword-to-bitmask keyword-list) (cffi:null-pointer)))

(defun get-mouse-event ()
  "Decode and return the mouse event struct as a mouse-event object."
  (flet ((plist-symbols-to-keywords (plist)
           ;; mem-ref returns a struct as a symbol plist.
           ;; we have to convert the symbols to keywords to transport them across packages.
           ;; '(bstate 4 z 0 y 17 x 84 id 0)) => (:BSTATE 4 :Z 0 :Y 17 :X 84 :ID 0)
           (loop for i in plist collect (if (symbolp i)
                                            (values (intern (symbol-name i) "KEYWORD"))
                                            ;; return any other object as it is
                                            i))))
    (cffi:with-foreign-object (ptr '(:struct ncurses:mevent))
      (ncurses:getmouse ptr)
      (let* ((struct (plist-symbols-to-keywords (cffi:mem-ref ptr '(:struct ncurses:mevent))))
             (x (getf struct :x))
             (y (getf struct :y))
             (b (getf struct :bstate))
             (mouse-event (bitmask-to-keyword b)))
        (values mouse-event y x)))))

(defun event-position (event)
  (with-slots (position-y position-x) event
    (if (and (null position-y)
             (null position-x))
        nil
        (list position-y position-x))))
