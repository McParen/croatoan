(in-package :de.anvi.croatoan)

(defparameter *mouse-button-event-bitmask-alist*
  '((:released       . #o01)
    (:pressed        . #o02)
    (:clicked        . #o04)
    (:double-clicked . #o10)
    (:triple-clicked . #o20)
    (:reserved-event . #o40)))

(defmacro mouse-bitmask (button event)
  (let ((*mouse-button-event-bitmask-alist*
         '((:released       . #o01)
           (:pressed        . #o02)
           (:clicked        . #o04)
           (:double-clicked . #o10)
           (:triple-clicked . #o20)
           (:reserved-event . #o40))))
    (cond ((integerp event) `(ash ,event (* 6 (- ,button 1))))
          ((symbolp event)
           (let ((mask (cdr (assoc event *mouse-button-event-bitmask-alist*))))
             `(ash ,mask (* 6 (- ,button 1))))))))

(defparameter *mouse-event-bitmask-alist*
  `((:button-1-released       . ,(mouse-bitmask 1 :released))
    (:button-1-pressed        . ,(mouse-bitmask 1 :pressed))
    (:button-1-clicked        . ,(mouse-bitmask 1 :clicked))
    (:button-1-double-clicked . ,(mouse-bitmask 1 :double-clicked))
    (:button-1-triple-clicked . ,(mouse-bitmask 1 :triple-clicked))
    (:button-1-reserved-event . ,(mouse-bitmask 1 :reserved-event))
    (:button-2-released       . ,(mouse-bitmask 2 :released))
    (:button-2-pressed        . ,(mouse-bitmask 2 :pressed))
    (:button-2-clicked        . ,(mouse-bitmask 2 :clicked))
    (:button-2-double-clicked . ,(mouse-bitmask 2 :double-clicked))
    (:button-2-triple-clicked . ,(mouse-bitmask 2 :triple-clicked))
    (:button-2-reserved-event . ,(mouse-bitmask 2 :reserved-event))
    (:button-3-released       . ,(mouse-bitmask 3 :released))
    (:button-3-pressed        . ,(mouse-bitmask 3 :pressed))
    (:button-3-clicked        . ,(mouse-bitmask 3 :clicked))
    (:button-3-double-clicked . ,(mouse-bitmask 3 :double-clicked))
    (:button-3-triple-clicked . ,(mouse-bitmask 3 :triple-clicked))
    (:button-3-reserved-event . ,(mouse-bitmask 3 :reserved-event))
    (:button-4-released       . ,(mouse-bitmask 4 :released))
    (:button-4-pressed        . ,(mouse-bitmask 4 :pressed))
    (:button-4-clicked        . ,(mouse-bitmask 4 :clicked))
    (:button-4-double-clicked . ,(mouse-bitmask 4 :double-clicked))
    (:button-4-triple-clicked . ,(mouse-bitmask 4 :triple-clicked))
    (:button-4-reserved-event . ,(mouse-bitmask 4 :reserved-event))
    (:button-ctrl             . ,(mouse-bitmask 5 #o01))
    (:button-shift            . ,(mouse-bitmask 5 #o02))
    (:button-alt              . ,(mouse-bitmask 5 #o04))
    (:report-mouse-position   . ,(mouse-bitmask 5 #o10))))

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
  (%mousemask (keyword-to-bitmask keyword-list) (null-pointer)))

;; decode and return the mouse event struct as multiple values:
;; mouse event keyword, y coordinate integer, x coordinate integer
(defun get-mouse-event ()
  (flet ((plist-symbols-to-keywords (plist)
           ;; mem-ref returns a struct as a symbol plist.
           ;; we have to convert the symbols to keywords to transport them across packages.
           (loop for i in plist collect (if (symbolp i) (values (intern (symbol-name i) "KEYWORD")) i))))
    (with-foreign-object (ptr '(:struct mevent))
      (%getmouse ptr)
      (let* ((struct (plist-symbols-to-keywords (mem-ref ptr '(:struct mevent))))
             (x (getf struct :x))
             (y (getf struct :y))
             (b (getf struct :bstate))
             (mouse-event (bitmask-to-keyword b)))
        (values mouse-event y x)))))
