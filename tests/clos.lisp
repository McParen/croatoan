(in-package :de.anvi.croatoan.tests)

(defun tetris ()
  (let ((scr (make-instance 'screen :input-echoing nil :input-blocking nil :enable-fkeys t :cursor-visibility nil)))
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

(defun snake ()
  (labels ((display-snake (scr body)
             (mapc #'(lambda (pair) (add-char scr (char-code #\*) :y (car pair) :x (cadr pair))) body)))
    (with-screen (scr :input-echoing nil :input-blocking nil :enable-fkeys t :cursor-visibility nil)
      (let* ((body '((0 7) (0 6) (0 5) (0 4) (0 3) (0 2) (0 1) (0 0)))
             (xpos (cadar body))
             (ypos (caar body))
             (dir #c(1 0)))

        (clear scr)
        (display-snake scr body)
        (refresh scr)

         (loop
            (let ((event (get-event scr)))
              (if event
                  (case event
                    (:up    (setf dir #c( 0  1)))
                    (:down  (setf dir #c( 0 -1)))
                    (:right (setf dir #c( 1  0)))
                    (:left  (setf dir #c(-1  0)))
                    (#\q    (return)))
                  (progn
                    (sleep 0.1)
                    (add-char scr (char-code #\space) :y (caar (last body)) :x (cadar (last body)))
                    (setf body (cons (list (incf ypos (- (imagpart dir)))
                                           (incf xpos (realpart dir)))
                                     (butlast body)))
                    (display-snake scr body)
                    (refresh scr)))))))))

;; https://github.com/skydrome/random/blob/master/shell/screensaver.sh
;; https://github.com/pipeseroni/pipes.sh/blob/master/pipes.sh
;; https://www.youtube.com/watch?v=T4n87IIa--U
(defun pipes ()
  (with-screen (scr :input-echoing nil :input-blocking nil :enable-fkeys t :cursor-visibility nil)
    (let* ((pos (list (round (/ (.height scr) 2)) (round (/ (.width scr) 2))))
           (dirs '((1 0) (-1 0) (0 -1) (0 1)))
           (dir (nth (random 4) dirs))
           (dir-prev dir)
           ch
           (i 0))
      (clear scr)
      (box scr)
      (refresh scr)
      (event-case (scr event)
        (#\q (return-from event-case))
        ((nil)
         (sleep 0.1)
         (move scr (car pos) (cadr pos))
         (setf ch (cond ((equal dir dir-prev) (if (= (car dir) 0) :horizontal-line :vertical-line))
                        ((and (equal dir-prev '(1  0)) (equal dir '(0 -1))) :lower-right-corner)
                        ((and (equal dir-prev '(1  0)) (equal dir '(0  1))) :lower-left-corner)
                        ((and (equal dir-prev '(0  1)) (equal dir '(-1 0))) :lower-right-corner)
                        ((and (equal dir-prev '(0  1)) (equal dir '(1  0))) :upper-right-corner)
                        ((and (equal dir-prev '(-1 0)) (equal dir '(0 -1))) :upper-right-corner)
                        ((and (equal dir-prev '(-1 0)) (equal dir '(0  1))) :upper-left-corner)
                        ((and (equal dir-prev '(0 -1)) (equal dir '(-1 0))) :lower-left-corner)
                        ((and (equal dir-prev '(0 -1)) (equal dir '(1  0))) :upper-left-corner)))
         (add-char scr ch)
         (let ((pos-trial (list (+ (car pos) (car dir)) (+ (cadr pos) (cadr dir)))))
           (if (or (= (car pos-trial)  0)
                   (= (cadr pos-trial) 0)
                   (= (car pos-trial)  (1- (.height scr)))
                   (= (cadr pos-trial) (1- (.width scr))))
               (setf pos (list (round (/ (.height scr) 2)) (round (/ (.width scr) 2))))
               (setf pos pos-trial)))
         (if (> i (random 5))
             (progn (setf i 0)
                    (setf dir-prev dir)
                    (setf dir (nth (random 3) (remove (list (- (car dir)) (- (cadr dir))) dirs :test #'equal))))
             (progn (incf i)
                    (setf dir-prev dir)))
         (refresh scr))))))

(defun matrix ()
  (with-screen (scr :input-echoing nil :input-blocking nil :cursor-visibility nil)
    (let* ((width (.width scr))
           (height (.height scr))
           (positions (loop repeat width collect (random height)))
           (speeds (loop repeat width collect (random 4))))
      (event-case (scr event)
        (#\q (return-from event-case))
        ((nil)
         (sleep 0.05)
         (loop for column from 0 to (1- width) do
              (loop repeat (nth column speeds) do
                   (setf (.attributes scr) '(:bold))
                   (setf (.color-pair scr) '(:white :black))
                   (add-char scr (+ 64 (random 58)) :y (mod (nth column positions) height) :x column)
                   (setf (.color-pair scr) '(:green :black))
                   (add-char scr (+ 64 (random 58)) :y (mod (- (nth column positions) 1) height) :x column)
                   (add-char scr (+ 64 (random 58)) :y (mod (- (nth column positions) 2) height) :x column)
                   (setf (.attributes scr) '(:normal))
                   (add-char scr (+ 64 (random 58)) :y (mod (- (nth column positions) 3) height) :x column)
                   (add-char scr (char-code #\space) :y (mod (- (nth column positions) (floor height 2)) height) :x column)
                   (refresh scr)
                   (setf (nth column positions) (mod (1+ (nth column positions)) height)))))))))

;; initialize ncurses, deinitialize ncurses
;; tests initialize-instance
(defun t00 ()
  (let ((scr (make-instance 'screen)))
    (unwind-protect
         nil
      (close scr))))

;; clos screen, accessors, unwind-protect.
(defun t01 ()
  (unwind-protect
       (let ((scr (make-instance 'screen :enable-colors t)))

         ;; the text will be red on yellow. this doesnt set the overall background, just the text background.
         (setf (.color-pair scr) '(:red :yellow))

         (clear scr)
         (move scr 0 0)
         (add-string scr "hello there!")
         (move scr 5 5)
         (add-string scr "dear john!")
         (move-by scr 5 5)
         (add-string scr "call me maybe!")
         (refresh scr)

         ;; wait for keypress, works only in blocking mode, which is the default.
         (get-char scr)

         ;; set a background. green dots on white background.
         (setf (.background scr) (make-instance 'complex-char :simple-char #\. :color-pair '(:green :white)))

         (refresh scr)

         ;; wait for the next keypress, then end.
         (get-char scr))

    ;; unwind protect makes sure that ncurses is ended at all cost.
    (end-screen)))

;; the same as t01, but hides the window creation and ncurses ending by utilizing the with-screen macro.
(defun t02 ()
  (with-screen (scr)
    (setf (.color-pair scr) '(:red :yellow))
    (clear scr)
    (move scr 0 0)
    (add-string scr "hello there!")
    (move scr 5 5)
    (add-string scr "dear john!")
    (move-by scr 5 5)
    (add-string scr "call me really!")
    (refresh scr)
    (get-char scr)
    
    (setf (.background scr) (make-instance 'complex-char :simple-char #\. :color-pair '(:green :white)))
    (refresh scr)
    (get-char scr)))

;; read and display chars until a q is pressed, blocking version.
(defun t03 ()
  (with-screen (scr :input-echoing nil :input-blocking t)
    (clear scr)
    (add-string scr "Type chars. Type q to quit. ")
    (refresh scr)

    (loop for ch = (get-char scr)
       while (not (equal (code-char ch) #\q))
       do (add-char scr ch))

    (add-string scr "You pressed q. Now press any char to quit.")
    (get-char scr)))

;; read and display chars until a q is pressed, non-blocking version (leads to 100% CPU usage).
;; wait for keyboard using get-char makes no sense in non-blocking code because it doesnt wait.
(defun t03a ()
  (with-screen (scr :input-echoing nil :input-blocking nil)
    (clear scr)
    (add-string scr "Type chars. Type q to quit. ")
    (refresh scr)

    (loop for ch = (get-char scr)
       while (or (= ch -1) (not (equal (code-char ch) #\q)))
       do (unless (= ch -1) (add-char scr ch)))))

;; read and display chars until a q is pressed, non-blocking version (leads to 100% CPU usage).
;; uses get-event for event handling.
(defun t03b ()
  (with-screen (scr :input-echoing nil :input-blocking nil)
    (clear scr)
    (add-string scr "Type chars. Type q to quit. ")
    (refresh scr)

    (loop (let ((event (get-event scr)))
            (when event
              (case event
                (#\q (return))
                (otherwise (add-char scr (char-code event)))))))))

;; using the event-case macro to simplify the event loop.
;; do not use ((nil) nil) with input-blocking nil, it leads to 100% CPU usage.
;; if nothing happens in the nil case anyway, we can use blocking.
(defun t03b2 ()
  (with-screen (scr :input-echoing nil :input-blocking t)
    (clear scr)
    (add-string scr "Type chars. Type q to quit. ")
    (refresh scr)
    (event-case (scr event)
      ;; ((nil) nil)
      (#\q (return-from event-case))
      (otherwise (add-char scr (char-code event))))))

;; slightly improved t03b2 pasted as an example to the cliki croatoan page
(defun t03b3 ()
  (with-screen (scr :input-echoing nil :input-blocking t :enable-colors t)
    (clear scr)
    (move scr 2 0)
    (format scr "Type chars. Type q to quit.~%~%")
    (refresh scr)
    (setf (.color-pair scr) '(:yellow :red)
          (.attributes scr) '(:bold))
    (event-case (scr event)
      (#\q (return-from event-case))
      (otherwise (princ event scr)
                 (refresh scr)))))

;; read and display chars until a q is pressed, blocking + gray stream version.
;; the stream reading functiond wont work in non-blocking mode and with non-char keys.
(defun t03c ()
  (with-screen (scr :input-echoing nil :input-blocking t)
    (clear scr)
    (princ "Type chars. Type q to quit." scr)
    (refresh scr)

    (loop for ch = (read-char scr)
       while (not (equal ch #\q))
       do (princ ch scr))

    (princ "You pressed q. Now press any char to quit." scr)
    (read-char scr)))

;; take a function given as symbol name and display its docstring. press q to exit.
;; Example: (a:t04 'cdr)
(defun t04 (&optional (name 'car))
  (unwind-protect
       (let ((scr (make-instance 'screen)))
         (clear scr)
         (refresh scr)

         (setf (.attributes scr) '(:bold :reverse))
         (add-string scr "Docstring")

         (setf (.attributes scr) '(:normal))
         (add-string scr " --> ")

         (setf (.attributes scr) '(:bold :underline))
         (add-string scr (format nil "~A~%~%" name))

         (setf (.attributes scr) '(:normal))
         (add-string scr (documentation name 'function))

         (refresh scr)
         (get-char scr))
    (end-screen)))

;; adding and removing attributes.
(defun t04a ()
  (unwind-protect
       (let ((scr (make-instance 'screen :enable-colors t)))
         (clear scr)
         (refresh scr)

         (add-string scr (write-to-string (.attributes scr)))
         (add-char scr (char-code #\newline))

         (pushnew :bold (.attributes scr))
         (add-string scr (write-to-string (.attributes scr)))
         (add-char scr (char-code #\newline))

         (pushnew :underline (.attributes scr))
         (add-string scr (write-to-string (.attributes scr)))
         (add-char scr (char-code #\newline))

         (pushnew :reverse (.attributes scr))
         (add-string scr (write-to-string (.attributes scr)))
         (add-char scr (char-code #\newline))

         (setf (.attributes scr) (remove :reverse (.attributes scr)))
         (add-string scr (write-to-string (.attributes scr)))
         (add-char scr (char-code #\newline))

         (setf (.attributes scr) (remove :underline (.attributes scr)))
         (add-string scr (write-to-string (.attributes scr)))
         (add-char scr (char-code #\newline))

         (setf (.attributes scr) (remove :bold (.attributes scr)))
         (add-string scr (write-to-string (.attributes scr)))
         (add-char scr (char-code #\newline))

         (refresh scr)
         (get-char scr))
    (end-screen)))

;; a more concise way to write t04a
(defun t04b ()
  (with-screen (scr :enable-colors t :cursor-visibility nil)
    (clear scr)

    (print (.attributes scr) scr)

    (add-attributes scr '(:bold))
    (print (.attributes scr) scr)

    (add-attributes scr '(:underline))
    (print (.attributes scr) scr)

    (add-attributes scr '(:reverse))
    (print (.attributes scr) scr)

    (remove-attributes scr '(:bold :underline :reverse))
    (print (.attributes scr) scr)

    (add-attributes scr '(:bold :underline :reverse))
    (print (.attributes scr) scr)
    
    (setf (.attributes scr) nil)
    (print (.attributes scr) scr)
    (print "test" scr)

    (move scr 6 10)
    (change-attributes scr 10 '())
    
    (refresh scr)
    (get-char scr)))

;; Make sure we shut down ncurses and dont mess up the terminal when an error is signaled.
(defun t05 ()
  (unwind-protect
       (let ((scr (make-instance 'screen)))
         (add-string scr "press any char to signal an error and go to the debugger in a messed up screen!")
         (refresh scr)
         (get-char scr)
         (error "zu huelf!"))
    ;; this will be executed after we somehow return from the debugger.
    (end-screen)))

;; End ncurses cleanly _before_ getting into the debugger when an error is signalled.
;; do get into the debugger, but only after we are back to the repl.
(defun t06 ()
  (let ((*debugger-hook* #'(lambda (c h) (declare (ignore c)) (declare (ignore h)) (end-screen))))
    (unwind-protect
         (let ((scr (make-instance 'screen)))
           (add-string scr "hello there! press a char to signal an error and go to the debugger without messung up the screen!")
           (refresh scr)
           (get-char scr)
           (error "zu huelf!"))
      (end-screen))))

;; The debugger hook is added now to with-screen.
;; When an error is signalled in a ncurses app, we cleanly exit ncurses first, we dont get
;; into the debugger and we merely print the signalled condition to the REPL.
(defun t06a ()
  (with-screen (scr)
    (add-string scr "hello there! press a char to signal an error. the app will be ended without going to the debugger.")
    (refresh scr)
    (get-char scr)
    (error "zu huelf!")))

;; display a new window on the stadard screen.
;; problem: deleting different windows is not standardized. (problem soved, example obsolete)
(defun t07 ()
  (unwind-protect
       (let ((scr (make-instance 'screen :enable-colors t)))
         (clear scr)
         (add-string scr "Standard screen")
         (refresh scr)
         (get-char scr)

         (let ((win (make-instance 'window :height 15 :width 50 :position '(5 5))))
           (setf (.background win) (make-instance 'complex-char :color-pair '(:red :blue)))
           (add-string win "Window 1")
           (refresh win)
           (get-char win)))
           ;(delete-window win) ; this is missing, we have to properly delete windows manually.

    ;; this will be executed after we somhow return from the debugger.
    (end-screen)))

;; close now closes both widows and the main screen.
;; but the creation now has to be outside the unwind-protect form.
(defun t07a ()
  (let ((scr (make-instance 'screen :enable-colors t)))
    ;; since windows are streams now, we can use close on tem too.
    ;; in order to be able to close scr, as compared to end-screen without arguments in t07,
    ;; we have to move unwind-protect inside the let scope.
    (unwind-protect
         (progn
           (clear scr)
           (add-string scr "Standard screen")
           (refresh scr)
           (get-char scr)

           (let ((win (make-instance 'window :height 15 :width 50 :position '(5 5) :border t)))
             (setf (.background win) (make-instance 'complex-char :color-pair '(:red :blue)))
             (add-string win "Window 1")
             (refresh win)
             (get-char win)
             (close win))

           (clear scr)
           (refresh scr)
           (get-char scr))
      ;; close is defined by the gray stream interface.
      (close scr)))) 

;; test the gray streams interface.
(defun t08 ()
  (unwind-protect
       (let ((scr (make-instance 'screen)))
         (clear scr)

         (write-char #\a)     ; writes a to the repl. wont be visible until we quit ncurses.
         (write-char #\b scr) ; writes b to scr.

         (princ "hello")      ; repl. wont be visible until we quit ncurses.
         (princ "hello" scr)

         (terpri scr)         ; 3 calls, 3 new lines will be added.
         (terpri scr)
         (terpri scr)

         (princ "there" scr)

         (fresh-line scr)     ; we call it 3 times, but only one newline will be added.
         (fresh-line scr)
         (fresh-line scr)

         (format scr "--~%~r~%--" 1234) ; each % will add a newline.

         (write-char #\newline scr) ; this is the char that actually gets displayed each time.

         (refresh scr)
         (get-char scr))
    (end-screen)))

;; when we define windows as streams, we can rebind the *standard-output* to print to a ncurses window. 
;; this only works with standard lisp output functions, format, write-char, terpri, print, etc.
;; we still have to explicitely ncurses functions clear, close, refresh, etc. though.
(defun t08a ()
  (let ((scr (make-instance 'screen)))
    (unwind-protect
         (progn
           (clear scr)

           ;; writes explicitely to scr
           (write-char #\a scr)
           (terpri scr)
           (princ "hello" scr)
           (terpri scr)
           (format scr "~r" 1234)
           (terpri scr)

           ;; writes to *standard-output*, which here is scr.
           (let ((*standard-output* scr))
             (write-char #\b)
             (terpri)
             (princ "there")
             (terpri)
             (format t "~r" 5678)
             (terpri))

           (refresh scr)
           (get-char scr))
      (close scr))))

;; we now can use colors on *standard-output* also with standard lisp printing functions.
;; and we can use them when other windows exist.
(defun t08b ()
  (let* ((scr (make-instance 'screen :enable-colors t))
         (*standard-output* scr))
    (unwind-protect
         (progn
           (clear scr)

           (setf (.background scr) (make-instance 'complex-char :color-pair '(:black :white)))
           (format t "~r" 1984)

           (refresh scr)
           (get-char scr)

           ;; temporarily bind *standard-output* to a window.
           (let* ((win (make-instance 'window :height 15 :width 50 :position '(5 5)))
                  (*standard-output* win))
             (setf (.background win) (make-instance 'complex-char :color-pair '(:white :black)))
             (format t "~r" 1985)
             (refresh win)
             (get-char win)
             (close win))

           ;; *standard-output*is now again scr.
           (setf (.background scr) (make-instance 'complex-char :color-pair '(:black :white)))
           (terpri)
           (format t "~r" 1984)

           (refresh scr)
           (get-char scr))

      (close scr))))

;; Write complex chars to standard output using standard lisp output functions + gray streams.
(defun t08c ()
  (with-screen (scr)
    (let ((*standard-output* scr)
          (ch (make-instance 'complex-char :simple-char #\a :attributes '(:bold :underlined) :color-pair '(:green :black))))
      (write-char #\a)
      (terpri)
      (write-char ch)
      (terpri)
      (princ ch)
      (terpri)
      (print ch)
      (terpri)
      (format t "~%Format:~%~S~%~A~%~C" ch ch ch)
      (terpri))
    (refresh scr)
    (get-char scr)))

;; box, move
(defun t09 ()
  (let* ((scr (make-instance 'screen :enable-colors t)))
    (unwind-protect
         (progn
           (clear scr)

           (setf (.background scr) (make-instance 'complex-char :color-pair '(:black :white)))
           (box scr)
           (move scr 1 1)
           (princ 0 scr)

           (refresh scr)

           (let ((w1 (make-instance 'window :height 10 :width 30 :position '(3 5)))
                 (w2 (make-instance 'window :height 10 :width 30 :position '(6 10)))
                 (w3 (make-instance 'window :height 10 :width 30 :position '(9 15))))

             (setf (.background w1) (make-instance 'complex-char :color-pair '(:white :black)))
             (setf (.background w2) (make-instance 'complex-char :color-pair '(:black :white)))
             (setf (.background w3) (make-instance 'complex-char :color-pair '(:white :black)))

             (box w1)
             (box w2)
             (box w3)

             (move w1 1 1)
             (princ 1 w1)
             (move w2 1 1)
             (princ 2 w2)
             (move w3 1 1)
             (princ 3 w3)

             (refresh w1)
             (refresh w2)
             (refresh w3)

             (get-char w1)

             ;; todo: before we can refresh w2 to raise it, we have to "touch" it.
             ;; if we dont touch it, only the changed parts of it will be redrawn.
             (touch w2)
             (refresh w2)

             (get-char w1)

             ;; move the whole window 3. note that this doesnt refresh the windows below,
             ;; they have to be refreshed separately.
             (setf (.position w3) '(9 20))
             (refresh w3)
             (get-char w3)

             (close w1)
             (close w2)
             (close w3))

           (setf (.background scr) (make-instance 'complex-char :color-pair '(:black :white)))

           (refresh scr)
           (get-char scr))

      (close scr))))

;; the same as t09, but we can now raise the overlapping windows by hitting 1, 2 or 3.
(defun t09a ()
  (let* ((scr (make-instance 'screen :enable-colors t :input-blocking t :input-echoing nil :use-default-colors t :cursor-visibility nil)))
    (unwind-protect
         (progn
           (clear scr)

           (box scr)
           (refresh scr)

           ;; the default value for width or height is "to the end of the screen".
           (let ((w1 (make-instance 'window :height 10 :width 30 :position '(3 5)))
                 (w2 (make-instance 'window            :width 30 :position '(6 10)))
                 (w3 (make-instance 'window :height 10           :position '(9 15))))

             (box w1)
             (box w2)
             (box w3)

             (setf (.background w1) (make-instance 'complex-char :simple-char #\space :color-pair '(:white :black)))
             (setf (.background w3) (make-instance 'complex-char :simple-char #\space :color-pair '(:white :black)))
             
             ;; TODO: clear, refresh, etc, should take one or more windows as arguments.
             ;; so we can do (refresh w1 w2 w3) instead of:
             (refresh w1)
             (refresh w2)
             (refresh w3)

             (loop (let ((event (get-event scr)))
                     (when event
                       (case event
                         (#\1 (touch w1) (refresh w1))
                         (#\2 (touch w2) (refresh w2))
                         (#\3 (touch w3) (refresh w3))
                         (#\q (return))
                         (otherwise nil)))))

             (close w1)
             (close w2)
             (close w3)))

      (close scr))))

(defun t09b ()
  "Use with-screen, event-case and mapc to simplify t09a."
  (with-screen (scr :input-blocking t :input-echoing nil :enable-colors t :cursor-visibility nil)
    (box scr)
    (refresh scr)
    
    (let ((w1 (make-instance 'window :height 10 :width 30 :position '(3 5)  :border t))
          (w2 (make-instance 'window            :width 30 :position '(6 10) :border t))
          (w3 (make-instance 'window :height 10           :position '(9 15) :border t)))

      (setf (.background w1) (make-instance 'complex-char :simple-char #\space :color-pair '(:white :black))
            (.background w3) (make-instance 'complex-char :simple-char #\space :color-pair '(:white :black)))

      (mapc #'refresh (list w1 w2 w3))

      (event-case (scr event)
        (#\1 (touch w1) (refresh w1))
        (#\2 (touch w2) (refresh w2))
        (#\3 (touch w3) (refresh w3))
        (#\q (return-from event-case))
        (otherwise nil))

      (mapc #'close (list w1 w2 w3))

      ;; return nil explicitely, so it doesnt return the window list.
      nil)))

;; port of kletva/test05.
;; print the screen size.
;; print the keys and key codes of any key pressed.
;; TODO: when we reach the end of the screen, the ~% doesnt print newlines any more and the screen doesnt scroll.
;; see scroll.lisp. a lot of window options, including scrolling, arent closified yet.
;; enable-fkeys t: fkeys have codes 255+
;; enable-fkeys nil: fkeys are multi-char escape codes.
(defun t10 ()
  (let ((scr (make-instance 'screen :input-echoing nil :enable-fkeys t)))
    (unwind-protect
         (progn
           (clear scr)
           
           (format scr "~A lines high, ~A columns wide.~%~%" (.height scr) (.width scr))
           (refresh scr)

           (loop
              ;; act only on events, do nothing on non-events.
              (when (key-pressed-p scr)
                  (let ((ch (get-char scr)))
                    ;; quit on q, print everything else including function keys.
                    (cond ((equal (code-char ch) #\q) (return))
                          (t (format scr "Char: ~A, Code: ~A~%" ch (code-char ch))))))))
      (close scr))))

;; cleaner key printing.
;; prints key names instead of numeric char codes.
(defun t10a ()
  (with-screen (scr :input-echoing nil :input-blocking nil :enable-fkeys t)
    (format scr "~A lines high, ~A columns wide.~%~%" (.height scr) (.width scr))
    (loop (let ((event (get-event scr)))
            (when event
              (case event
                ;; printable chars
                (#\q (return))

                ;; non-printable ascii chars. (#\space and #\newline are standard, all others non-standard)
                (#\escape (format scr "escape char~%"))
                (#\tab (format scr "tab char~%"))
                (#\space (format scr "space char~%"))

                ;; NL can be either LF \n,CR \r,or CRLF \r\n, depending on the system. it is LF on ubuntu.
                ;; NL is the standard, system independent, portable way.
                (#\linefeed (format scr "enter/linefeed LF \n char~%"))
                (#\return (format scr "enter/return CR \r char~%"))
                (#\newline (format scr "enter/newline char~%")) 

                (#\rubout (format scr "rubout char~%")) ;; DEL, delete char 127
                (#\backspace (format scr "backspace char~%")) ;; BS, not the same as the :backspace key

                ;; function keys
                ;; the same as #\rubout, but different code.
                ;; ncurses bug: :backspace is returned for windows, #\rubout for stdscr.
                (:backspace (format scr "backspace key~%")) 

                (otherwise (format scr "Event: ~A~%" event))))))))


;; demonstrate scrolling and scrolling regions.
(defun t11 ()
  (let ((scr (make-instance 'screen)))
    (unwind-protect
         (progn
           (clear scr)
           (refresh scr)

           ;; (move scr 5 5)
           (setf (.cursor-position scr) '(5 5))

           ;; see inopts.
           (setf 

            ;; this is sufficient to make the whole window scroll.
            ;;(enable-scrolling scr t)
            ;;(%scrollok (.winptr scr) t)
            (.enable-scrolling scr) t 

            ;; to make only a few lines (5 to 10) scroll, we have to set a line-based region.
            ;;(set-scrolling-region scr 5 10)
            ;;(%wsetscrreg (.winptr scr) 5 10)
            (.scrolling-region scr) '(5 10))

           (loop for i from 0 to 30 do
                (format scr "~A~%" i)
                (refresh scr)
                (sleep 0.2)))

      (close scr))))

;; Display available ACS (alternative character set) pseudo-graphical characters.
(defun t12 ()
  (let ((scr (make-instance 'screen)))
    (unwind-protect
         (progn

(add-string scr "ACS_ULCORNER ") (add-char scr (acs :upper-left-corner    )) (add-string scr " upper left corner") (new-line scr) 
(add-string scr "ACS_LLCORNER ") (add-char scr (acs :lower-left-corner    )) (add-string scr " lower left corner ") (new-line scr)
(add-string scr "ACS_URCORNER ") (add-char scr (acs :upper-right-corner   )) (add-string scr " upper right corner ") (new-line scr)
(add-string scr "ACS_LRCORNER ") (add-char scr (acs :lower-right-corner   )) (add-string scr " lower right corner ") (new-line scr)
(add-string scr "ACS_LTEE     ") (add-char scr (acs :tee-pointing-right   )) (add-string scr " tee pointing right ") (new-line scr)
(add-string scr "ACS_RTEE     ") (add-char scr (acs :tee-pointing-left    )) (add-string scr " tee pointing left ") (new-line scr)
(add-string scr "ACS_BTEE     ") (add-char scr (acs :tee-pointing-up      )) (add-string scr " tee pointing up ") (new-line scr)
(add-string scr "ACS_TTEE     ") (add-char scr (acs :tee-pointing-down    )) (add-string scr " tee pointing down ") (new-line scr)
(add-string scr "ACS_HLINE    ") (add-char scr (acs :horizontal-line      )) (add-string scr " horizontal line ") (new-line scr)
(add-string scr "ACS_VLINE    ") (add-char scr (acs :vertical-line        )) (add-string scr " vertical line ") (new-line scr)
(add-string scr "ACS_PLUS     ") (add-char scr (acs :crossover-plus       )) (add-string scr " large plus or crossover ") (new-line scr)
(add-string scr "ACS_S1       ") (add-char scr (acs :scan-line-1          )) (add-string scr " scan line 1 ") (new-line scr)
(add-string scr "ACS_S3       ") (add-char scr (acs :scan-line-3          )) (add-string scr " scan line 3 ") (new-line scr)
(add-string scr "ACS_S7       ") (add-char scr (acs :scan-line-7          )) (add-string scr " scan line 7 ") (new-line scr)
(add-string scr "ACS_S9       ") (add-char scr (acs :scan-line-9          )) (add-string scr " scan line 9 ") (new-line scr)
(add-string scr "ACS_DIAMOND  ") (add-char scr (acs :diamond-symbol       )) (add-string scr " diamond ") (new-line scr)
(add-string scr "ACS_BOARD    ") (add-char scr (acs :board                )) (add-string scr " board of squares ") (new-line scr)
(add-string scr "ACS_CKBOARD  ") (add-char scr (acs :checker-board        )) (add-string scr " checker board (stipple) ") (new-line scr)
(add-string scr "ACS_DEGREE   ") (add-char scr (acs :degree-symbol        )) (add-string scr " degree symbol ") (new-line scr)
(add-string scr "ACS_PLMINUS  ") (add-char scr (acs :plus-minus           )) (add-string scr " plus/minus ") (new-line scr)
(add-string scr "ACS_BULLET   ") (add-char scr (acs :bullet-symbol        )) (add-string scr " bullet ") (new-line scr)
(add-string scr "ACS_LARROW   ") (add-char scr (acs :arrow-pointing-left  )) (add-string scr " arrow pointing left ") (new-line scr)
(add-string scr "ACS_RARROW   ") (add-char scr (acs :arrow-pointing-right )) (add-string scr " arrow pointing right ") (new-line scr)
(add-string scr "ACS_DARROW   ") (add-char scr (acs :arrow-pointing-down  )) (add-string scr " arrow pointing down ") (new-line scr)
(add-string scr "ACS_UARROW   ") (add-char scr (acs :arrow-pointing-up    )) (add-string scr " arrow pointing up ") (new-line scr)
(add-string scr "ACS_LANTERN  ") (add-char scr (acs :lantern-symbol       )) (add-string scr " lantern symbol ") (new-line scr)
(add-string scr "ACS_BLOCK    ") (add-char scr (acs :solid-square-block   )) (add-string scr " solid square block ") (new-line scr)
(add-string scr "ACS_LEQUAL   ") (add-char scr (acs :less-than-or-equal   )) (add-string scr " less/equal ") (new-line scr)
(add-string scr "ACS_GEQUAL   ") (add-char scr (acs :greater-than-or-equal)) (add-string scr " greater/equal ") (new-line scr)
(add-string scr "ACS_PI       ") (add-char scr (acs :pi                   )) (add-string scr " Pi ") (new-line scr)
(add-string scr "ACS_NEQUAL   ") (add-char scr (acs :not-equal            )) (add-string scr " not equal ") (new-line scr)
(add-string scr "ACS_STERLING ") (add-char scr (acs :uk-pound-sterling    )) (add-string scr " UK pound sign") (new-line scr)

           (get-char scr))
      (close scr))))

;; after adding gray streams and adding acs to add-char.
;; http://melvilletheatre.com/articles/ncurses-extended-characters/index.html
(defun t12a ()
  (with-screen (scr)
    (add-char scr :upper-left-corner    ) (format scr " ACS_ULCORNER / upper left corner      ~%")
    (add-char scr :lower-left-corner    ) (format scr " ACS_LLCORNER / lower left corner      ~%")
    (add-char scr :upper-right-corner   ) (format scr " ACS_URCORNER / upper right corner     ~%")
    (add-char scr :lower-right-corner   ) (format scr " ACS_LRCORNER / lower right corner     ~%")   
    (add-char scr :tee-pointing-right   ) (format scr " ACS_LTEE     / tee pointing right     ~%")
    (add-char scr :tee-pointing-left    ) (format scr " ACS_RTEE     / tee pointing left      ~%")   
    (add-char scr :tee-pointing-up      ) (format scr " ACS_BTEE     / tee pointing up        ~%")
    (add-char scr :tee-pointing-down    ) (format scr " ACS_TTEE     / tee pointing down      ~%")
    (add-char scr :horizontal-line      ) (format scr " ACS_HLINE    / horizontal line        ~%")   
    (add-char scr :vertical-line        ) (format scr " ACS_VLINE    / vertical line          ~%")
    (add-char scr :crossover-plus       ) (format scr " ACS_PLUS     / large plus or crossover~%")
    (add-char scr :scan-line-1          ) (format scr " ACS_S1       / scan line 1            ~%")
    (add-char scr :scan-line-3          ) (format scr " ACS_S3       / scan line 3            ~%")
    (add-char scr :scan-line-7          ) (format scr " ACS_S7       / scan line 7            ~%")
    (add-char scr :scan-line-9          ) (format scr " ACS_S9       / scan line 9            ~%")
    (add-char scr :diamond-symbol       ) (format scr " ACS_DIAMOND  / diamond                ~%")
    (add-char scr :board                ) (format scr " ACS_BOARD    / board of squares       ~%")
    (add-char scr :checker-board        ) (format scr " ACS_CKBOARD  / checker board (stipple)~%")
    (add-char scr :degree-symbol        ) (format scr " ACS_DEGREE   / degree symbol          ~%")
    (add-char scr :plus-minus           ) (format scr " ACS_PLMINUS  / plus/minus             ~%")
    (add-char scr :bullet-symbol        ) (format scr " ACS_BULLET   / bullet                 ~%")
    (add-char scr :arrow-pointing-left  ) (format scr " ACS_LARROW   / arrow pointing left    ~%")
    (add-char scr :arrow-pointing-right ) (format scr " ACS_RARROW   / arrow pointing right   ~%")
    (add-char scr :arrow-pointing-down  ) (format scr " ACS_DARROW   / arrow pointing down    ~%")
    (add-char scr :arrow-pointing-up    ) (format scr " ACS_UARROW   / arrow pointing up      ~%")
    (add-char scr :lantern-symbol       ) (format scr " ACS_LANTERN  / lantern symbol         ~%")
    (add-char scr :solid-square-block   ) (format scr " ACS_BLOCK    / solid square block     ~%")
    (add-char scr :less-than-or-equal   ) (format scr " ACS_LEQUAL   / less/equal             ~%")
    (add-char scr :greater-than-or-equal) (format scr " ACS_GEQUAL   / greater/equal          ~%")
    (add-char scr :pi                   ) (format scr " ACS_PI       / Pi                     ~%")
    (add-char scr :not-equal            ) (format scr " ACS_NEQUAL   / not equal              ~%")
    (add-char scr :uk-pound-sterling    ) (format scr " ACS_STERLING / UK pound sign          ~%")

    (refresh scr)
    (get-char scr)))

;; Smaller ACS example to explain how the ACS system works.
(defun t12b ()
  (with-screen (scr)
    ;; acs takes a keyword :ulcorner, translates it to a char #\l,
    ;; ncurses translates it at run-time to a terminal-dependent
    ;; integer value that doesnt correspond to a char code.
    (format scr "~A ~A ~A " #\l (char-code #\l) (acs :upper-left-corner))
    ;; We cant output it in a string, but have to use add-char.
    (add-char scr 4194412)
    (terpri scr)
    (refresh scr)
    (get-char scr)))

;; Show the longer symbol names next to the ACS chars in a more readable table view.
(defun t12c ()
  (with-screen (scr)
    (loop for i from 0 for symbol in
         '(:upper-left-corner
           :lower-left-corner
           :upper-right-corner
           :lower-right-corner
           :tee-pointing-right
           :tee-pointing-left
           :tee-pointing-up
           :tee-pointing-down
           :horizontal-line
           :vertical-line)
       do (progn (move scr (+ 2 (* i 2)) 2)
                 (add-char scr symbol)
                 (format scr "  ~S~%~%" symbol)))
    
    (loop for i from 0 for symbol in
         '(:crossover-plus
           :scan-line-1
           :scan-line-3
           :scan-line-7
           :scan-line-9
           :diamond-symbol
           :board
           :checker-board
           :degree-symbol
           :plus-minus
           :bullet-symbol)
       do (progn (move scr (+ 2 (* i 2)) 30)
                 (add-char scr symbol)
                 (format scr "  ~S~%~%" symbol)))

    (loop for i from 0 for symbol in
         '(:arrow-pointing-left
           :arrow-pointing-right
           :arrow-pointing-down
           :arrow-pointing-up
           :lantern-symbol
           :solid-square-block
           :less-than-or-equal
           :greater-than-or-equal
           :pi
           :not-equal
           :uk-pound-sterling)
       do (progn (move scr (+ 2 (* i 2)) 55)
                 (add-char scr symbol)
                 (format scr "  ~S~%~%" symbol)))
    (refresh scr)
    (get-char scr)))

;; Demonstrate flash and beep alerts.
;; It depends on the terminal emulator whether they will work for you.
;; They both worked in xterm for me.
(defun t13 ()
  (with-screen (scr :input-echoing nil :input-blocking nil :enable-fkeys t :cursor-visibility nil)
    (loop
       (let ((event (get-event scr)))
         (if event
             (case event
               (#\b (alert :beep))
               (#\f (alert :flash))
               (#\q (return)))
             (sleep 0.1))))))

;; minimal setting to get the mouse working.
;; reads and prints a single mouse event.
(defun t14 ()
  (let ((scr (make-instance 'screen :input-echoing nil :input-blocking t :enable-fkeys t)))
    (unwind-protect 
         (progn
           (%mousemask #b00000111111111111111111111111111 (null-pointer)) ; activate all mouse events.
           (get-char scr) ; here you have to click to generate a mouse event.
           (with-foreign-object (me '(:struct mevent)) ; create a pointer to the struct mevent.
             (%getmouse me) ; save the mouse event struct to the pointed position.
             (princ (mem-ref me '(:struct mevent)) scr) ; dereference the pointer, return a plist of the struct.
             (get-char scr)))
      (close scr))))

;; (cffi:convert-to-foreign '(id 1 x 1 y 2 z 3 bstate 2) '(:struct mevent))
;; (setf ev (convert-from-foreign (mem-ref bstate '(:struct mevent)) '(:struct mevent)))

;; mouse events are now detected in the event loop.
;; print the y x coordinates and the detected event.
(defun t14a ()
  (with-screen (scr :input-echoing nil :input-blocking t :enable-fkeys t :cursor-visibility nil)
    (set-mouse-event '(:button-1-clicked :button-2-clicked :button-3-clicked))
    (event-case (scr event y x)
      ((:button-1-clicked :button-2-clicked :button-3-clicked) (format scr "~3A ~3A ~A~%" y x event))
      (#\q (return-from event-case)))))

;; left click prints a 1, right click prints a 3.
(defun t14b ()
  (with-screen (scr :input-echoing nil :input-blocking t :enable-fkeys t :cursor-visibility nil)
    (set-mouse-event '(:button-1-clicked :button-3-clicked))
    (event-case (scr event mouse-y mouse-x)
      (:button-1-clicked (move scr mouse-y mouse-x) (princ "1" scr))
      (:button-3-clicked (move scr mouse-y mouse-x) (princ "3" scr))
      (#\q (return-from event-case)))))

;; resize event: the standard screen size is resized automatically.
(defun t15 ()
  (with-screen (scr :input-echoing nil :input-blocking nil :enable-fkeys t :cursor-visibility nil :enable-colors t)
    (add-string scr "Current standard screen geometry (Y x X):" :y 0 :x 0)
    (loop
       (let ((event (get-event scr)))
         (if event
             (case event
               (:resize (move scr 1 0)
                        (format scr "~A Y lines x ~A X columns." (.height scr) (.width scr))
                        (refresh scr))
               (#\q (return)))
             (progn
               (sleep 0.1)))))))

;; resize event: arrange window _positions_ relative to the screen size.
(defun t15a ()
  (with-screen (scr :input-echoing nil :input-blocking nil :enable-fkeys t :cursor-visibility nil :enable-colors t)
    (add-string scr "Current standard screen geometry (Y x X):" :y 0 :x 0)
    (setf (.background scr) (make-instance 'complex-char :simple-char #\. :color-pair '(:green :white)))
    (let ((time 0)
          ;; place a window in the center of the screen.
          (win (make-instance 'window :height 5 :width 10 :position (list (round (/ (.height scr) 2))
                                                                        (round (/ (.width scr) 2))))))
      (loop
         (let ((event (get-event scr)))
           (if event
               (case event
                 (:resize ;; if the scren is resized, relocate the window to the new center.
                          (move win (round (/ (.height scr) 2)) (round (/ (.width scr) 2)) :target :window) 
                          ;; better differentiation of types with methods.
                          (move win 0 0)
                          (format win "Y:~A X:~A" (.height scr) (.width scr))
                          ;; repaint all windows completely by touching them before refreshing.
                          ;; overlapping windows have to be refreshed in reverse stacking order.
                          (mapc #'(lambda (w) (touch w) (refresh w)) 
                                (list scr win)))
                 (#\q (return)))
               (progn
                 (sleep 0.01)
                 (move win 1 0)
                 (incf time 0.01)
                 (format win "~A" time)
                 (mapc #'(lambda (w) (touch w) (refresh w)) 
                       (list scr win)) ))))
      (close win))))

;; resize event: arrange window _sizes_ relative to the screen size.
(defun t15b ()
  (with-screen (scr :input-echoing nil :input-blocking nil :enable-fkeys t :cursor-visibility nil :enable-colors t)
    (add-string scr "Current standard screen geometry (Y x X):" :y 0 :x 0)
    (setf (.background scr) (make-instance 'complex-char :color-pair '(:black :white)))
    (let ((time 0)
          ;; make the window slightly smaller than the standard screen.
          (win (make-instance 'window :height (- (.height scr) 4) :width (- (.width scr) 6) :position '(2 3))))
      (loop
         (let ((event (get-event scr)))
           (if event
               (case event
                 (:resize ;; resize the window on every termina resize.
                          (resize win (- (.height scr) 4) (- (.width scr) 6))
                          (move win 0 0)
                          (format win "Y:~A X:~A" (.height scr) (.width scr))
                          ;; when updating several overlapping windows, using mark-for-refresh and 
                          ;; batch-refresh instead of several calls to refresh prevents flickering.
                          (mapc #'(lambda (w) (touch w) (mark-for-refresh w)) (list scr win))
                          (refresh-marked))
                 (#\q (return)))
               (progn
                 (sleep 0.01)
                 (move win 1 0)
                 (incf time 0.01)
                 (format win "~A" time)
                 (mapc #'(lambda (w) (touch w) (mark-for-refresh w)) (list scr win))
                 (refresh-marked) ))))
      (close win))))

;; test get-string: read a string, then output it.
;; the only function key we can use during input is backspace.
(defun t16 ()
  (unwind-protect
       (let ((scr (make-instance 'screen)))
         (clear scr)
         (move scr 0 0)
         (add-string scr "Type your name: ")
         (refresh scr)
         (let ((str (get-string scr 10)))
           (add-string scr "Your name is: ")
           (add-string scr str))
         (refresh scr)
         (get-char scr))
    (end-screen)))

;; read a single line of Lisp input (30 chars max) from the last line, 
;; evaluate it and print the result to the output window above.
(defun t16a ()
  (with-screen (scr :input-echoing t :input-blocking t :enable-fkeys t :cursor-visibility t :enable-colors nil)
    (let ((out (make-instance 'window :height (1- (.height scr)) :width (.width scr) :position '(0 0)))
          (in (make-instance 'window :height 1 :width (.width scr) :position (list (1- (.height scr)) 0))))

      (print (eval (read-from-string (get-string in 30))) out)
      (refresh out)

      ;; blocking is t, so wait till the next keypress before exiting.
      (get-char in) 

      ;; close windows and window streams.
      (close in)
      (close out))))

;; add a loop to the input, making it a simple repl.
(defun t16b ()
  (with-screen (scr :input-echoing t :input-blocking t :enable-fkeys t :cursor-visibility t :enable-colors nil)
    (let ((out (make-instance 'window :height (1- (.height scr)) :width (.width scr) :position '(0 0)))
          (in (make-instance 'window :height 1 :width (.width scr) :position (list (1- (.height scr)) 0))))
      (loop
         (let ((str (get-string in 30)))
           ;; if the input line is empty (length = 0), do nothing.
           (when (> (length str) 0)
                ;; a single blocking q exits the loop.
                (when (string= str "q") (return))
                ;; after the input is read, clear the input line.
                (clear in)
                (print (eval (read-from-string str)) out)
                (refresh out))))
      (close in)
      (close out))))

;; ncurses' get-string only allows the backspace key.
;; extend the functionality of the input line of the simple repl.
(defun t16c ()
  (with-screen (scr :input-echoing nil :cursor-visibility t :enable-colors t)
    (let* ((wout (make-instance 'window :height (1- (.height scr)) :width (.width scr) :position '(0 0) :enable-scrolling t))
           ;; input blocking is a property of every single window, not just of the global screen.
           (win (make-instance 'window :height 1 :width (.width scr) :position (list (1- (.height scr)) 0)
                               :enable-fkeys t :input-blocking t))
           (*standard-output* wout)
           (n 0)) ; no of chars in the input line.
      
      (event-case (win event)
        (:left 
         (when (> (cadr (.cursor-position win)) 0)
           (move-by win 0 -1)))
        (:right 
         (when (< (cadr (.cursor-position win)) n)
           (move-by win 0 1)))
        (#\newline ; RET key, C-j, C-m
           (when (> n 0) ; only print when the line is not empty.
             (let* ((strin (extract-string win :n n :y 0 :x 0)) 
                    (strout (eval (read-from-string strin))))
               (princ strin)
               (terpri)
               (format t "=> ~A~%~%" strout))
             (setf n 0)
             (clear win) ; empty the input line after evaluation.
             (refresh wout)))
        (:dc ; DEL key
         (when (> n (cadr (.cursor-position win)))
           (decf n)
           (delete-char win)))
        (:ic ; INS / Einfg key
         (format t "(.insert-enabled win) => ~A~%" (.insert-enabled win))
         (setf (.insert-enabled win) (not (.insert-enabled win)))
         (format t "(.insert-enabled win) => ~A~%" (.insert-enabled win))
         (refresh wout))
        (:backspace ; BS key
         (when (> (cadr (.cursor-position win)) 0)
           (decf n)
           (move-by win 0 -1)
           (delete-char win)))
        (#\q (return-from event-case))
        ((nil) ; when no key is hit at all
         ;; when there is no event, get-event will return nil.
         ;; this is the place for no-event code.
         ;; instead of doing nothing, set blocking to t.
         nil)
        (otherwise ; all other keys
         (when (and (characterp event)
                    (< (cadr (.cursor-position win)) (1- (.width win))))
           (incf n)
           (princ event win))))
      (close win)
      (close wout))))

;; creating sub-windows and how they share memory with the parent window.
;; leaving out the size of a window maxes it out to the right (win1) and to the bottom (win1, win3)
(defun t17 ()
  (with-screen (scr :input-echoing nil :input-blocking t :cursor-visibility nil :enable-colors t)
    (let* ((win1 (make-instance 'window :position '(2 2) :border t))
           (win2 (make-instance 'sub-window :parent win1 :height 5 :width 20 :position '(4 4) :border t))
           (win3 (make-instance 'sub-window :parent win1 :width 20 :position '(4 4) :border t :relative t)))
      (princ "win1" win1)
      (princ "win2" win2)
      (princ "win3 relative" win3)
      (mapc #'(lambda (w) (refresh w)) (list win1 win2 win3))
      (get-char win3)
      (mapc #'(lambda (w) (close w)) (list win2 win3))
      (refresh win1)
      ;; observe that the content from win 2 and 3 is still in win1 after they have been closed.
      (get-char win1)
      (close win1) )))

;; by default, the sub-window displays the part of the parent window it overlaps with.
;; we can change which part of the parent is displayed by changing the sub-windows source
;; we can change where it is displayed by changing the sub-windows position.
(defun t17a ()
  (with-screen (scr :input-echoing nil :input-blocking t :cursor-visibility nil :enable-colors t)
    (let ((win (make-instance 'sub-window :parent scr :height 5 :width 20 :position '(2 2) :border t :relative t)))
      ;; initial content written to subwin and thus to scr.
      (move win 1 1) (princ "subwin" win)

      ;; create two content areas in the parent window.
      (move scr 2 25) (princ "area1" scr)
      (move scr 3 29) (princ "area1" scr)
      (move scr 4 33) (princ "area1" scr)
      (move scr 5 37) (princ "area1" scr)
      (move scr 6 41) (princ "area1" scr)

      (move scr 2 50) (princ "area2" scr)
      (move scr 3 54) (princ "area2" scr)
      (move scr 4 58) (princ "area2" scr)
      (move scr 5 62) (princ "area2" scr)
      (move scr 6 66) (princ "area2" scr)

      (mapc #'(lambda (w) (touch w) (refresh w)) (list scr win))
      (get-char scr)

      ;; map area1 to subwin position
      ;; the mapping only _displays_ the content from the source area.
      ;; the mapping it doesnt _write_ it onto the displayed area of scr.
      (setf (.source win) '(2 25)) ; (%mvderwin (.winptr win) 2 25)
      (mapc #'(lambda (w) (touch w) (refresh w)) (list scr win))
      (get-char scr)

      ;; map area2 to subwin position
      (setf (.source win) '(2 50))
      (mapc #'(lambda (w) (touch w) (refresh w)) (list scr win))
      (get-char scr)

      ;;; now move subwin position
      ;; it still maps area2, but now to the new position.
      ;; the original content written to the subwin (and thus to scr because they share memory)
      ;; is now visible in scr, since the subwin overlay has moved.
      (setf (.position win) '(10 2))
      (mapc #'(lambda (w) (touch w) (refresh w)) (list scr win))
      (get-char scr)

      ;; map area1 again, but now to the new position.
      (setf (.source win) '(2 25))
      (mapc #'(lambda (w) (touch w) (refresh w)) (list scr win))
      (get-char scr)

      ;; writing to a sub-window writes to the _mapped_ position of the parent window
      ;; _not_ to the parent position below the subwindow.
      (clear win)
      (move win 0 0) (princ "writing to sub-win" win)
      (move win 1 0) (princ "writes to parent win" win)
      (mapc #'(lambda (w) (touch w) (refresh w)) (list scr win))
      (get-char scr)

      ;; delete the subwindow win
      ;; the mapped content isnt displayed any more.
      ;; now the content before the mapping should be displayed again.
      (mapc #'(lambda (w) (close w)) (list win))
      (refresh scr)
      (get-char scr) )))

;; Display misc system information
(defun t18 ()
  (with-screen (scr)
    (format scr "Lisp implementation type:    ~A~%" (lisp-implementation-type))
    (format scr "Lisp implementation version: ~A~%" (lisp-implementation-version))
    (format scr "Ncurses version:             ~A~%" (de.anvi.ncurses:%curses-version))
    (format scr "Terminal:                    ~A~%" (%termname))
    (format scr "Colors supported:            ~A~%" (%has-colors))
    (format scr "Color change supported:      ~A~%" (%can-change-color))
    (format scr "No of supported colors:      ~A~%" %colors)
    (format scr "No of supported color pairs  ~A~%" %color-pairs)
    (format scr "Can insert/delete chars:     ~A~%" (%has-ic))
    (format scr "Can insert/delete lines:     ~A~%" (%has-il))
    #+sbcl
    (progn
      (format scr "Terminal:                    ~A~%" (sb-ext:posix-getenv "TERM"))
      (format scr "Locale:                      ~A~%" (sb-ext:posix-getenv "LANG"))
      (format scr "Hostname:                    ~A~%" (sb-ext:posix-getenv "HOSTNAME"))
      (format scr "Username:                    ~A~%" (sb-ext:posix-getenv "USER"))
      (format scr "Operating system:            ~A~%" (sb-ext:posix-getenv "OSTYPE"))
      (format scr "CPU Type:                    ~A~%" (sb-ext:posix-getenv "MACHTYPE")))
    (format scr "uname -a:~%~A" (uiop:run-program "uname -a" :output :string))
    (refresh scr)
    (get-char scr)))

;; http://stackoverflow.com/questions/38684906/how-to-get-package-documentation-in-quicklisp
(defun t18a ()
  "Display misc quicklisp information."
  #+quicklisp
  (with-screen (scr)
    (let* ((dist (ql-dist:dist "quicklisp")) ; current dist
           ;; output to an empty broadcast stream is discraded like redirecting to /dev/null
           (*standard-output* (make-broadcast-stream))
           ;; Quicklisp prints its "Fetching xyz" messages to *trace-output*
           (*trace-output* *standard-output*)
           (installed-systems (mapcar #'ql-dist:name (ql-dist:installed-systems dist))))
      (format scr "Quicklisp dist name          ~A~%" (ql-dist:name dist))
      (format scr "Quicklisp dist version       ~A~%" (ql-dist:version dist))
      (format scr "Quicklisp available version  ~A~%" (caar (ql-dist:available-versions dist)))
      (format scr "~%Quicklisp installed releases ~{~A, ~}~%" (mapcar #'ql-dist:name (ql-dist:installed-releases dist)))
      (format scr "~%Quicklisp installed systems  ~{~A, ~}~%~%" installed-systems)
      (loop
         for i in installed-systems
         for sys = (asdf:find-system i nil)
         do 
           (when sys
             (format scr "~27@A | ~A~%"
                     i
                     (remove #\newline (asdf:system-description sys))))))
    (refresh scr)
    (get-char scr))
  #-quicklisp
  (princ "Quicklisp not installed."))

;; print a simple menu, let the user choose an item by arrow keys, return the chosen item.
;; https://www.gnu.org/software/guile-ncurses/manual/html_node/A-simple-key-usage-example.html
(defun t19 ()
  (with-screen (scr :input-echoing nil :input-blocking t :cursor-visibility nil :enable-colors t)
    (flet ((draw-menu (win choices i)
             (clear win)
             (loop for j from 0 to (1- (length choices)) do
                  (move win j 0)
                  (format win "~A~A" (nth j choices) (if (= i j) "*" ""))
                  (when (= i j)
                    (move win j 0)
                    (change-attributes win 9 '(:reverse)))
                  (refresh win))))
      (let* ((choices '("Choice 0" "Choice 1" "Choice 2" "Choice 3" "Choice 4" "Choice 5" "Choice 6"))
             (n (length choices))
             (i 0)) ; current choice
        (draw-menu scr choices i)
        (event-case (scr event)
          (:up (setf i (mod (1- i) n)) (draw-menu scr choices i))
          (:down (setf i (mod (1+ i) n)) (draw-menu scr choices i))
          (#\newline (return-from event-case (nth i choices)))
          (#\q (return-from event-case)))))))

;; an even simpler 2-item menu is a yes-no-dialog.
(defun t19a ()
  (with-screen (scr :input-echoing nil :input-blocking t :cursor-visibility nil :enable-colors t)
    (flet ((draw-menu (win choices i)
             (move win 2 0) (clear win :target :end-of-line)
             (loop for j from 0 to (1- (length choices)) do
                  (move win 2 (* 10 j))
                  (format win "~A~A" (car (nth j choices)) (if (= i j) "*" ""))
                  (refresh win))))
      (let* ((choices '(("Yes" . t) ("No" . nil)))
             (n (length choices))
             (i 0)) ; current choice
        (move scr 0 0) (format scr "User, do you want?") (refresh scr)
        (draw-menu scr choices i)
        (event-case (scr event)
          ((:up :down :left :right #\tab) (setf i (mod (1+ i) n)) (draw-menu scr choices i))
          (#\newline (return-from event-case (cdr (nth i choices)))))))))

;; use the menu class and draw-menu function.
(defun t19b ()
  (with-screen (scr :input-echoing nil :input-blocking t :cursor-visibility nil :enable-colors t)
    (let* ((choices '("Choice 0" "Choice 11" "Choice 222" "Choice 3333" "Choice 44444" "Choice 555555" "Choice 6666666"))
           (menu (make-instance 'menu-window :items choices :position (list 0 20) :title "t19b" :border t :enable-fkeys t)))
      (let ((result (select-item menu)))
        (format scr "You chose ~A" result)
        (touch scr)
        (refresh scr)
        (get-char scr))
      (close menu))))

(defun t19c ()
  "Improved t19b, the menu can be called repeatedly with the key a."
  (with-screen (scr :input-echoing nil :input-blocking t :cursor-visibility nil :enable-colors t)
    (let* ((choices '("Choice 0" "Choice 11" "Choice 222" "Choice 3333" "Choice 44444" "Choice 555555"
                      "Choice 6666666" "Choice 7" "Choice 88" "Choice 999"))
           (menu (make-instance 'menu-window :items choices :position (list 0 25) :scrolled-layout (list 6 1)
                                :title "t19c" :border t :enable-fkeys t)))
      (event-case (scr event)
        ;; "a" draws the menu and enters a new menu-only event loop
        (#\a (let ((result (select-item menu)))
               (format scr "You chose ~A~%" result)
               ;; we have to touch scr in order to make the menu disappear.
               (touch scr)
               (refresh scr)))
        (#\q (return-from event-case)))
      (close menu))))

(defun t19d ()
  "Use the arrow keys to pick a value from an 2D array menu, given as a layout parameter."
  (with-screen (scr :input-echoing nil :input-blocking t :cursor-visibility nil :enable-colors t)
    (let* ((items '("Item 0" "Item 1" "Item 2" "Item 3" "Item 4" "Item 5"))
           (menu (make-instance 'menu-window
                                :items items :position (list 0 20) :layout (list 2 3)
                                :title "t19d" :border t :enable-fkeys t)))
      (event-case (scr event)
        ;; "a" draws the menu and enters a new menu-only event loop
        (#\a (let ((result (select-item menu)))
               (format scr "You chose ~A~%" result)
               ;; we have to touch scr in order to make the menu disappear.
               (touch scr)
               (refresh scr)))
        (#\q (return-from event-case)))
      (close menu))))

;; A one-line menu without a title and border resembling a menu bar
(defun t19e ()
  (with-screen (scr :input-echoing nil :input-blocking t :cursor-visibility nil :enable-colors t)
    (let* ((items '("Item 0" "Item 1" "Item 2" "Item 3" "Item 4" "Item 5" "Item 6" "Item 7" "Item 8" "Item 9"))
           (menu (make-instance 'menu-window :input-blocking t :items items :position (list 0 0)
                                :layout (list 1 (length items)) :scrolled-layout (list 1 4)
                                :max-item-length 10 :width (.width scr) :border t :enable-fkeys t)))
      ;; start the output below the menu
      (move scr 4 0)
      ;; exit the infinite loop by exiting the menu with q.
      (loop named menu-case
         do
           (let ((result (select-item menu)))
             (unless result (return-from menu-case))
             (format scr "You chose ~A~%" result)
             (refresh scr)))
      (close menu))))

(defun t19f ()
  "A more fancy version of t19a, a yes-no dialog using the class dialog-window."
  (with-screen (scr :input-echoing nil :input-blocking t :cursor-visibility nil :enable-colors t)
    (let* ((items (list "Yes" "No" "Maybe"))
           (menu (make-instance 'dialog-window
                                :input-blocking t
                                :items items :position (list 5 15) :layout (list 1 3)
                                :max-item-length 10
                                :current-item-mark "> "
                                :width 60 :border t :enable-fkeys t
                                :title "this is my dialog"
                                :message-height 2
                                :message-text "Press <- or -> to choose. Enter to confirm choice.~%Press q to exit.")))
      (setf (.background scr) (make-instance 'complex-char :color-pair '(:white :black)))
      (refresh scr)
      (loop named menu-case
         do
           (let ((result (select-item menu)))
             (unless result (return-from menu-case))
             (format scr "You chose ~A~%" result)
             (refresh scr)))
      (close menu))))

;; Passing the color attribute directly to a character.
(defun t20 ()
  "Display a randomly created carpet of the seven default colors, except for black."
  (with-screen (scr :input-echoing nil :input-blocking nil :enable-colors t :cursor-visibility nil)
    (let ((width  (.width scr))
          (height (.height scr))
          (colors '(:red :green :yellow :blue :magenta :cyan :white)))
      (event-case (scr event)
        (#\q (return-from event-case))
        ((nil)
         (sleep 0.01)
         (echo scr #\space
                   :y (random height)
                   :x (random width)
                   :color-pair (list :black (nth (random 7) colors))))))))

;; Tests for insert-char, insert-string, extract-complex-char.
(defun t21 ()
  (with-screen (scr :cursor-visibility nil)
    (move scr 0 0) (add-char scr #\a)
    ;; overwrite b over a
    (move scr 0 0) (add-char scr #\b)
    ;; insert pi before b
    (move scr 0 0) (insert-char scr :pi :color-pair '(:yellow :red))
    ;; insert d before pi
    (move scr 0 0) (insert-string scr "d ")
    ;; change the attributes of the d
    (move scr 0 0) (change-attributes scr 1 '(:underline) :color-pair '(:green :black))
    ;; extract the complex d from the window, then print its properties.
    (let ((e (extract-complex-char scr)))
      (move scr 1 0)
      ;; format uses print-object specialized on complex-chars
      (format scr "~S~%~S ~S ~S" e (.simple-char e) (.attributes e) (.color-pair e))
      ;; print back the extracted char
      (move scr 3 0)
      (add-char scr (.simple-char e) :attributes (.attributes e) :color-pair (.color-pair e))
      ;; directly print the complex char
      (add-char scr e :y 5 :x 10 :n -1)
      ;; print strings with attributes and colors
      ;; attributes dont work (yet) when a long string gets wrapped around the last column.
      (add-string scr "string with attributes" :y 11 :x 10 :attributes '(:underline) :color-pair '(:black :green))
      ;; test adding both chars and strings with a single wrapper routine.
      (add scr #\a :y 13 :x 0 :attributes '(:underline) :color-pair '(:yellow :red) :n -1)
      (add scr "string with attributes" :y 15 :x 0 :n 11 :attributes '(:underline :bold) :color-pair '(:yellow :green)))
    ;; test print-object specialisation on complex-string
    (let ((str (make-instance 'complex-string :string "complex-string" :color-pair '(:blue :white) :attributes '(:underline))))
      (format scr "~%~S~%" str)
      (add-string scr str :y 18 :x 73 :n -1)
      (add scr str :y 19 :x 70 :n -1))
    ;; extract the first line as a complex-string, then reprint it at line 22
    (let ((str (extract-complex-string scr :y 0 :x 0 :n 4)))
      (move scr 22 1)
      (add scr str :n -1))
    ;; extract the first line as a simple string, then reprint it at line 23
    (let ((str (extract-string scr :y 0 :x 0 :n 4)))
      (move scr 23 1)
      (add scr str))
    (refresh scr)
    (get-char scr)))

;; temporarily end the main screen, write something to the repl, and then return to the screen.
;; the reader .closed-p can be used to check whether the screen has been temporarily closed.
(defun t22 ()
  (let ((scr (make-instance 'screen :input-blocking t :input-echoing nil)))
    (unwind-protect
         (progn
           (clear scr)
           ;; should yield NIL, since we didnt endwin yet
           (format scr "1. screen before ending: ~A~%" (.closed-p scr))
           (get-char scr)

           (close scr)

           ;; this shouldnt be visible, but it is visible after the later refresh
           ;; obviously ncurses doesnt destroy the screen window after it is closed
           (format scr "2. screen after ending, we dont see it till after the refresh: ~A~%" (.closed-p scr))

           ;; should yield T, because we closed the screen
           (format t "3. in the repl, after ending the screen: ~A~%" (.closed-p scr))
           (force-output *standard-output*)
           (sleep 5)

           (refresh scr)
           ;; should yield NIL, since we refreshed
           (format scr "4. screen after refreshing: ~A~%" (.closed-p scr))
           
           (get-char scr))
      (close scr))))

(defun t23 ()
  "Use save-excursion to return the cursor to its initial position."
  (with-screen (scr :input-echoing nil :input-blocking t :cursor-visibility t :enable-colors t)
    (move scr 0 0)
    (princ "1. hello" scr)
    (save-excursion scr
      (move scr 5 5)
      (princ "2. there" scr))
    (princ "3. dear john" scr)
    (get-char scr)))

(defun t24 ()
  "Test usage of insert-line and delete-line."
  (with-screen (scr :input-echoing nil :input-blocking t :cursor-visibility t :enable-colors t)
    (loop for i from 0 to (- (.height scr) 1)
       do
         (move scr i 0)
         (format scr "~A" i))
    (refresh scr)
    (event-case (scr event)
      (:up   (move-to scr :up)   (refresh scr))
      (:down (move-to scr :down) (refresh scr))
      (#\d   (delete-line scr)   (refresh scr))
      (#\i   (insert-line scr)   (refresh scr))
      (#\q   (return-from event-case)))
    (get-char scr)))

(defun t25 ()
  "Test initialisation and refreshing of pads."
  (with-screen (scr :input-blocking t :cursor-visibility nil :enable-colors t)
    (let ((p (make-instance 'pad :height 100 :width 100)))

      ;; populate the pad with numbers.
      (loop for j from 0 to 99
         do (loop for i from 0 to 99
               do
                 (move p j i)
                 (format p "~D" (mod (* i j) 10))))

      (setf (.background p) (make-instance 'complex-char :color-pair '(:green :white)))

      (let ((pad-min-y     2)
            (pad-min-x     2)
            (screen-min-y  5)
            (screen-min-x  5)
            (screen-max-y 10)
            (screen-max-x 15))

      ;; the background screen has to be touched and refreshed on every move,
      ;; otherwise we will see parts of the previously displayed pad still there.
      (touch scr)
      (refresh scr)
      (refresh p pad-min-y pad-min-x screen-min-y screen-min-x screen-max-y screen-max-x)

      (event-case (scr event)
        (:up    (decf pad-min-y) (decf screen-min-y) (decf screen-max-y) (touch scr) (refresh scr)
                (refresh p pad-min-y pad-min-x screen-min-y screen-min-x screen-max-y screen-max-x))
        (:down  (incf pad-min-y) (incf screen-min-y) (incf screen-max-y) (touch scr) (refresh scr)
                (refresh p pad-min-y pad-min-x screen-min-y screen-min-x screen-max-y screen-max-x))
        (:left  (decf pad-min-x) (decf screen-min-x) (decf screen-max-x) (touch scr) (refresh scr)
                (refresh p pad-min-y pad-min-x screen-min-y screen-min-x screen-max-y screen-max-x))
        (:right  (incf pad-min-x) (incf screen-min-x) (incf screen-max-x) (touch scr) (refresh scr)
                 (refresh p pad-min-y pad-min-x screen-min-y screen-min-x screen-max-y screen-max-x))
        (#\q   (return-from event-case))
        (otherwise nil)))

      (close p))))
