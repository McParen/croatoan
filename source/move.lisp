(in-package :de.anvi.croatoan)

(defun move (window y x &key (target :cursor))
  (let ((winptr (.winptr window)))
    (case target
      (:cursor (%wmove winptr y x))
      (:window (%mvwin winptr y x)))))

(defun move-by (window dy dx)
  (let ((y (car  (.cursor-position window)))
        (x (cadr (.cursor-position window))))
    (%wmove (.winptr window) (+ y dy) (+ x dx))))

(defun move-to (window direction &optional (n 1))
  (let ((winptr (.winptr window)))
    (case direction
      (:left  (move-by winptr        0 (* n -1)))
      (:right (move-by winptr        0 (* n  1)))
      (:up    (move-by winptr (* n -1)        0))
      (:down  (move-by winptr (* n  1)        0))
      (otherwise (error "valid cursor movement directions: :left, :right, :up, :down")))))

;;; TODOs

;; [ ] what about return values? check them.
;; [ ] decide what happens if the cursor moves outside of the window. %wmove returns ERR, but what do we do?
;; [ ] eventually collect all move-cursor functions together as methods.
