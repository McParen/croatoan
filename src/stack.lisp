(in-package :de.anvi.croatoan)

;; panel
;; panel stack extension for curses
;; http://invisible-island.net/ncurses/man/panel.3x.html

(defclass stack ()
  ((items
    :initarg       :items
    :initform      nil
    :type          (or null cons)
    :accessor      items
    :documentation "List containing the items."))
  (:documentation "Stack implementation of ncurses panels, allows management of overlapping windows."))

(defparameter *main-stack* (make-instance 'stack)
  "Global window stack. Windows can be added upon initialization with :stacked t or (setf (stackedp win) t).

(setf (visiblep win) nil) prevents a stacked window from being refreshed and thus displayed.")

(defun stack-push (obj stack)
  "Add the object or objects on top of the stack."
  (with-slots (items) stack
    (push obj items)))

(defun stack-pop (stack)
  "Remove and return the top item off the stack."
  (with-slots (items) stack
    (pop items)))

(defun stack-move (obj-or-n pos-or-dir stack)
  "Move an object in the stack.

The object can be passed directly and is the compared by eq, or by its
position in the stack.

Valid movement directions are :up :down :top and :bottom."
  (with-slots (items) stack
    (let* ((n (typecase obj-or-n
                (integer obj-or-n)
                (otherwise (position obj-or-n items)))))
      (case pos-or-dir
        (:up
         (unless (= n 0)
           (rotatef (nth (1- n) items)
                    (nth     n  items))))
        (:down
         (unless (= n (1- (length items)))
           (rotatef (nth (1+ n) items)
                    (nth     n  items))))
        (:top
         (unless (= n 0)
           (setf items (cons (nth n items)
                             (remove (nth n items) items)))))
        (:bottom
         (unless (= n (1- (length items)))
           (setf items (append (remove (nth n items) items)
                               (list (nth n items))))))))))

(defun empty-stack (stack)
  "Remove all items from the stack."
  (with-slots (items) stack
    (setf items nil)))

(defun stack-empty-p (stack)
  "Return t if there are no items on the stack, nil otherwise."
  (null (items stack)))

(defmethod refresh ((stack stack) &rest args)
  "Touch and refresh visible windows in the window stack.

The windows are refreshed in the order they are added, so that if the windows overlap,
the window added last will be displayed on top."
  (with-slots (items) stack
    (when items
      (mapc #'(lambda (w)
                (when (visiblep w)
                  ;; touch marks the windows as changed so that they are completely
                  ;; redrawn upon the refresh.
                  ;; this is necessary to remove remnants of overlapping windows.
                  (touch w)
                  (mark-for-refresh w)))
            (reverse items))
      (refresh-marked))))

;; https://www.informatimago.com/develop/lisp/l99/p19.lisp
;; todo: what if count is longer than a list? use mod
(defun rotate (list count)
  (if (minusp count)
      (rotate list (+ (length list) count))
      (nconc (subseq list count) (subseq list 0 count))))
