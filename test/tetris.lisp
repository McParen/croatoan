(in-package :de.anvi.croatoan.test)

(defun tetris ()
  (let ((scr (make-instance 'screen :input-echoing nil :input-blocking nil :enable-function-keys t :cursor-visible nil)))
    (unwind-protect

         (let* ((board (make-array '(20 10) :initial-element nil))
                (pieces

'((((0 0) (0 1) (0 2) (1 1)) ((0 1) (1 0) (2 1) (1 1)) ((1 0) (0 1) (1 2) (1 1)) ((0 0) (1 0) (2 0) (1 1)))  ;T
  (((0 0) (0 1) (0 2) (1 0)) ((0 0) (0 1) (2 1) (1 1)) ((1 0) (1 1) (1 2) (0 2)) ((0 0) (1 0) (2 0) (2 1)))  ;L
  (((0 0) (0 1) (0 2) (1 2)) ((0 1) (1 1) (2 1) (2 0)) ((0 0) (1 0) (1 2) (1 1)) ((0 0) (0 1) (1 0) (2 0)))  ;J
  (((1 0) (0 1) (0 2) (1 1)) ((0 0) (1 0) (2 1) (1 1)) ((1 0) (0 1) (0 2) (1 1)) ((0 0) (1 0) (2 1) (1 1)))  ;S
  (((0 0) (0 1) (1 2) (1 1)) ((1 0) (0 1) (2 0) (1 1)) ((0 0) (0 1) (1 2) (1 1)) ((1 0) (0 1) (2 0) (1 1)))  ;Z
  (((0 0) (0 1) (1 0) (1 1)) ((0 0) (0 1) (1 0) (1 1)) ((0 0) (0 1) (1 0) (1 1)) ((0 0) (0 1) (1 0) (1 1)))  ;O
  (((0 0) (0 1) (0 2) (0 3)) ((0 0) (1 0) (2 0) (3 0)) ((0 0) (0 1) (0 2) (0 3)) ((0 0) (1 0) (2 0) (3 0)))));I

                (position '(0 3))
                (orientation 0)
                (style 0)
                (piece (nth orientation (nth style pieces))))

           (labels (;; check whether we left the board.
                    (boundary-crossed-p (piece position)
                      (loop
                         for i in piece 
                         for j = (mapcar #'+ i position)
                         do
                           (when (or (> (cadr j) 9) (< (cadr j) 0))
                             (return t))))

                    (bottom-reached-p (piece position)
                      (loop
                         for i in piece 
                         for j = (mapcar #'+ i position)
                         do
                           (when (> (car j) 19) 
                             (return t))))

                    ;; check whether the piece coordinates are non-nil on the board.
                    (collision-occured-p (board piece position)
                      (loop
                         for i in piece 
                         for j = (mapcar #'+ i position)
                         do
                           (when (aref board (car j) (cadr j))
                             (return t))))

                    (move-permissible-p (board piece next-position)
                      (not (or (boundary-crossed-p piece next-position)
                               (bottom-reached-p piece next-position)
                               (collision-occured-p board piece next-position))))

                    (add-new-piece ()
                      (setf style (random 7))
                      (setf orientation 0)
                      (setf piece (elt (elt pieces style) 0))
                      (setf position '(0 3)))

                    ;; add piece to board.
                    (update-board ()
                      (loop
                         for i in piece 
                         for j = (mapcar #'+ i position)
                         do
                           (setf (aref board (car j) (cadr j)) t)))

                    (remove-line (board m)
                      (loop for i from m above 0 do
                           (loop for j from 0 to 9 do
                                (setf (aref board i j) (aref board (1- i) j)))))

                    ;; find geht nicht, wir muessen checken ob _alle_ t sind.
                    (remove-complete-lines ()
                      (loop for i from 0 to 19 do
                           (when (every #'identity (loop for j from 0 to 9 for y = (aref board i j) collect y))
                             (remove-line board i))))

                    (draw-board-and-piece ()
                      ;; first, draw the board
                      (loop for line from 0 to 19 do
                           (loop for column from 0 to 9 do
                                (move scr line column)
                                (if (aref board line column)
                                    (princ "x" scr)
                                    (princ "_" scr))))
                      ;; add coord of position to every coord of piece
                      ;; then draw the piece
                      (loop
                         for i in piece 
                         for j = (mapcar #'+ i position)
                         do
                           (move scr (car j) (cadr j))
                           (princ "x" scr))
                      (refresh scr))

                    ;; down #c(-1 0), left #c(-1 0), right #c(1 0)
                    (move-piece (direction)
                      (let ((next-position (list (- (car position) (imagpart direction))
                                                 (+ (cadr position) (realpart direction)))))
                        (if (move-permissible-p board piece next-position)
                            (setf position next-position)
                            ;; when sideways move not permissible: do nothing.
                            ;; when downwards move not permissible: add piece to board, start new piece.
                            (unless (zerop (imagpart direction))
                              (update-board)
                              (remove-complete-lines)
                              (add-new-piece))))
                      (draw-board-and-piece))

                    (rotate-piece ()
                      (let ((next-piece (elt (elt pieces style) (mod (+ orientation 1) 4))))
                        (when (move-permissible-p board next-piece position)
                          (setf orientation (mod (+ orientation 1) 4)) 
                          (setf piece (elt (elt pieces style) orientation))
                          (draw-board-and-piece))))

                    (drop-piece ()
                      (loop
                         (let ((next-position (list (1+ (car position)) (cadr position))))
                           (if (move-permissible-p board piece next-position)
                               (setf position next-position)
                               (return))))
                      (update-board)
                      (remove-complete-lines)
                      (add-new-piece)
                      (draw-board-and-piece)))

             ;; clear screen before starting.
             (clear scr)
             (refresh scr)

             ;; navigate with the arrow keys, q to quit.
             (loop
                (let ((event (get-event scr)))
                  (if event
                      (case event
                        (:up (rotate-piece))
                        (:down (drop-piece))
                        (:right (move-piece #c(1 0)))
                        (:left (move-piece #c(-1 0)))
                        (#\q (return)))
                      (progn
                        (move-piece #c(0 -1))
                        (sleep 0.3)))))))

      ;; cleanly close ncurses at the end.
      (close scr))))
