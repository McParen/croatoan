(in-package :de.anvi.croatoan)

;; unused
(defparameter *mouse-button-event-bitmask-alist*
  '((:released       . #o01) ; 00001
    (:pressed        . #o02) ; 00010
    (:clicked        . #o04) ; 00100
    (:double-clicked . #o10) ; 01000
    (:triple-clicked . #o20) ; 10000
    (:reserved-event . #o40))) ; unused in NCURSES_MOUSE_VERSION 2

;; events for buttons 1-5
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
    (cond ((integerp event)
           `(ash ,event (* ,offset (- ,button 1))))
          ((symbolp event)
           (let ((mask (cdr (assoc event event-bitmasks))))
             `(ash ,mask (* ,offset (- ,button 1))))))))

;; button1    1- 5
;; button2    6-10
;; button3   11-15
;; button4   16-20
;; button5   21-25
;; modifiers 26-28
;; position     29
;; unused    30-32 bit
(defparameter *mouse-event-bitmask-alist*
  `((:button-1-released       . ,(mouse-bitmask 1 :released))
    (:button-1-pressed        . ,(mouse-bitmask 1 :pressed))
    (:button-1-clicked        . ,(mouse-bitmask 1 :clicked))
    (:button-1-double-clicked . ,(mouse-bitmask 1 :double-clicked))
    (:button-1-triple-clicked . ,(mouse-bitmask 1 :triple-clicked))
    (:button-2-released       . ,(mouse-bitmask 2 :released))
    (:button-2-pressed        . ,(mouse-bitmask 2 :pressed))
    (:button-2-clicked        . ,(mouse-bitmask 2 :clicked))
    (:button-2-double-clicked . ,(mouse-bitmask 2 :double-clicked))
    (:button-2-triple-clicked . ,(mouse-bitmask 2 :triple-clicked))
    (:button-3-released       . ,(mouse-bitmask 3 :released))
    (:button-3-pressed        . ,(mouse-bitmask 3 :pressed))
    (:button-3-clicked        . ,(mouse-bitmask 3 :clicked))
    (:button-3-double-clicked . ,(mouse-bitmask 3 :double-clicked))
    (:button-3-triple-clicked . ,(mouse-bitmask 3 :triple-clicked))
    (:button-4-released       . ,(mouse-bitmask 4 :released))
    (:button-4-pressed        . ,(mouse-bitmask 4 :pressed))
    (:button-4-clicked        . ,(mouse-bitmask 4 :clicked))
    (:button-4-double-clicked . ,(mouse-bitmask 4 :double-clicked))
    (:button-4-triple-clicked . ,(mouse-bitmask 4 :triple-clicked))
    (:button-5-released       . ,(mouse-bitmask 5 :released))
    (:button-5-pressed        . ,(mouse-bitmask 5 :pressed))
    (:button-5-clicked        . ,(mouse-bitmask 5 :clicked))
    (:button-5-double-clicked . ,(mouse-bitmask 5 :double-clicked))
    (:button-5-triple-clicked . ,(mouse-bitmask 5 :triple-clicked))

    ;; Unused in NCURSES_MOUSE_VERSION 2
    ;;(:button-1-reserved-event . ,(mouse-bitmask 1 :reserved-event))
    ;;(:button-2-reserved-event . ,(mouse-bitmask 2 :reserved-event))
    ;;(:button-3-reserved-event . ,(mouse-bitmask 3 :reserved-event))
    ;;(:button-4-reserved-event . ,(mouse-bitmask 4 :reserved-event))

    ;; Use TERM=xterm-1003 to enable this event
    ;; alternatively send the sequence "CSI ? 1003 h" to xterm
    ;; This event is reported even when it is not activated
    ;; when one of the modifier keys is pressed.
    (:report-mouse-position   . ,(mouse-bitmask 6 #o10))

    ;; https://en.wikipedia.org/wiki/Modifier_key

    ;; the modifiers are in the alist after the events, so they will be returned
    ;; by bitmask-to-keyword after the button event: (:event1 :mod1 :mod2)
    (:ctrl                    . ,(mouse-bitmask 6 #o01))
    (:shift                   . ,(mouse-bitmask 6 #o02))
    (:alt                     . ,(mouse-bitmask 6 #o04))

    ;; This is not a single event bitmask, but a bitmask enabling ALL events,
    ;; to be passed to set-mouse-event
    ;; Does not include :report-mouse-position, which has to be passed separately.
    (:all-mouse-events        . ,(- (mouse-bitmask 6 #o10) 1))))

(defun bitmask-to-keyword (bitmask alist)
  "Take a bitmask and an alist, return a list of keys with bits matching the mask.

The bitmask should be a 32bit integer.

The values in alist should be 32bit integers with only one single bit switched on."
  (loop for i in alist
        if (logtest bitmask (cdr i))
          collect (car i)))

(defun keyword-to-bitmask (keys alist)
  "Take a key list and an alist, return a bitmask combining the values of the keys.

The values in alist should be 32bit integers with only one single bit switched on."
  (apply #'logior (mapcar #'(lambda (x) (cdr (assoc x alist)))
                          keys)))

#|

(format t "~32,'0b" bitmask)

(let ((keys '(:BUTTON-1-CLICKED :BUTTON-4-PRESSED))
               (alist *mouse-event-bitmask-alist*))
           (equalp keys (bitmask-to-keyword (keyword-to-bitmask keys alist)
                                            (butlast alist))))
|#

(defun set-mouse-event (keys)
  "Take a list of mouse event keys, activate tracking of those events.

Returns an integer bitmask. An empty list turns off mouse tracking."
  (ncurses:mousemask (keyword-to-bitmask keys *mouse-event-bitmask-alist*)
                     (cffi:null-pointer)))

(defun get-mouse-event ()
  "Decode and return the mouse event struct as multiple values."
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
             (mouse-event (bitmask-to-keyword b
                                              ;; all-mouse-events isnt an event, so exclude it
                                              (butlast *mouse-event-bitmask-alist*))))
        ;; return the modifiers as the fourth value
        (values (car mouse-event) y x (cdr mouse-event))))))

(defun event-position (event)
  (with-slots (position-y position-x) event
    (if (and (null position-y)
             (null position-x))
        nil
        (list position-y position-x))))
