(in-package :de.anvi.croatoan.test)

;; On the death of John Horton Conway, 12.04.2020

(defun alive-neighbors-count (world y x)
  "Take the coordinates of one cell, return a number of its alive neighbors."
  (destructuring-bind (rows cols) (array-dimensions world)
    (loop for i from (max (1- y) 0) to (min (1+ y) (1- rows)) sum 
         (loop for j from (max (1- x) 0) to (min (1+ x) (1- cols)) 
            when (and (not (and (= i y) (= j x)))
                      (aref world i j))
            count i))))

(defun next (world)
  "Take an array representing the world at time t, return a new array at t+1."
  (let ((next (make-array (array-dimensions world) :initial-element nil))
        (rows (first (array-dimensions world)))
        (cols (second (array-dimensions world))))
    (loop for i from 0 to (1- rows) do
         (loop for j from 0 to (1- cols) do
              (case (alive-neighbors-count world i j)
                ((0 1)
                 ;; death by exposure
                 (setf (aref next i j) nil))
                (2
                 ;; survival
                 (when (aref world i j)
                   (setf (aref next i j) t)))
                (3
                 ;; survival
                 (when (aref world i j)
                   (setf (aref next i j) t))
                 ;; birth
                 (when (null (aref world i j))
                   (setf (aref next i j) t)))
                ((4 5 6 7 8)
                 ;; death by overcrowding
                 (when (aref world i j)
                   (setf (aref next i j) nil))))))
    next))

(defun draw-gol-world (world win)
  (clear win)
  (destructuring-bind (rows cols) (array-dimensions world)
    (dotimes (i rows)
      (dotimes (j cols)
        (move win i j)
        (when (aref world i j)
          (add win #\*)))))
  (refresh win))

(defun gol (&optional (init '((1 2) (2 0) (2 2) (3 1) (3 2))))
  (with-screen (scr :input-blocking nil :input-echoing nil :cursor-visible nil)
    (clear scr)
    (let ((world (make-array (list (height scr) (width scr)) :initial-element nil)))
      (bind scr #\q 'exit-event-loop)

      ;; add a glider
      (bind scr #\g (lambda (w e) (loop for pos in init do (setf (apply #'aref world pos) t))))

      ;; add an acorn
      (bind scr #\f
            (lambda (w e)
              (let* ((offset '(10 20))
                     (acorn (mapcar (lambda (pos) (mapcar #'+ pos offset))
                                   '((0 1) (1 3) (2 0) (2 1) (2 4) (2 5) (2 6)))))
                (loop for pos in acorn do (setf (apply #'aref world pos) t)))))

      ;; accelerate
      (bind scr #\a (lambda (w e) (setf (frame-rate scr) (* 2 (frame-rate scr)))))
      ;; decelerate
      (bind scr #\d (lambda (w e) (when (>= (frame-rate scr) 2)
                               (setf (frame-rate scr)
                                     (/ (frame-rate scr) 2)))))
      ;; draw and update
      (bind scr nil (lambda (win e)
                      (draw-gol-world world win)
                      (setf world (next world))))
      (setf (frame-rate scr) 2)
      (run-event-loop scr))))
