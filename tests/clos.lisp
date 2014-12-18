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

(defun matrix ()
  (with-screen (scr :input-echoing nil :input-blocking nil :cursor-visibility nil)
    (let* ((width (.width scr))
           (height (.height scr))
           (positions (loop repeat width collect (random height)))
           (speeds (loop repeat width collect (random 4))))
      (loop
         (let ((event (get-event scr)))
           (if event
               (case event
                 (#\q (return)))
               (progn
                 (sleep 0.05)
                 (loop for column from 0 to (1- width) do
                      (loop repeat (nth column speeds) do
                           (setf (.color-pair scr) '(:white :black))
                           (add-char scr (+ 64 (random 58)) :y (mod (nth column positions) height) :x column)
                           (setf (.color-pair scr) '(:green :black))
                           (add-char scr (+ 64 (random 58)) :y (mod (- (nth column positions) 1) height) :x column)
                           (add-char scr (+ 64 (random 58)) :y (mod (- (nth column positions) 3) height) :x column)
                           (add-char scr (char-code #\space) :y (mod (- (nth column positions) 15) height) :x column)
                           (refresh scr)
                           (setf (nth column positions) (mod (1+ (nth column positions)) height)))))))))))

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

;; read and display chars until a q is pressed, non-blocking version.
;; wait for keyboard using get-char makes no sense in non-blocking code because it doesnt wait.
(defun t03a ()
  (with-screen (scr :input-echoing nil :input-blocking nil)
    (clear scr)
    (add-string scr "Type chars. Type q to quit. ")
    (refresh scr)

    (loop for ch = (get-char scr)
       while (or (= ch -1) (not (equal (code-char ch) #\q)))
       do (unless (= ch -1) (add-char scr ch)))))

;; read and display chars until a q is pressed, non-blocking version.
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

         (let ((win (make-instance 'window :height 15 :width 50 :origin '(5 5))))
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

           (let ((win (make-instance 'window :height 15 :width 50 :origin '(5 5))))
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
           (let* ((win (make-instance 'window :height 15 :width 50 :origin '(5 5)))
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

           (let ((w1 (make-instance 'window :height 10 :width 30 :origin '(3 5)))
                 (w2 (make-instance 'window :height 10 :width 30 :origin '(6 10)))
                 (w3 (make-instance 'window :height 10 :width 30 :origin '(9 15))))

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
             (setf (.origin w3) '(9 20))
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
  (let* ((scr (make-instance 'screen :enable-colors t :input-blocking nil :input-echoing nil)))
    (unwind-protect
         (progn
           (clear scr)

           (box scr)
           (refresh scr)

           (let ((w1 (make-instance 'window :height 10 :width 30 :origin '(3 5)))
                 (w2 (make-instance 'window :height 10 :width 30 :origin '(6 10)))
                 (w3 (make-instance 'window :height 10 :width 30 :origin '(9 15))))

             (box w1)
             (box w2)
             (box w3)

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
                (#\q (return))
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

(add-string scr "ACS_ULCORNER ") (add-char scr (acs :ulcorner)) (add-string scr " upper left corner") (new-line scr) 
(add-string scr "ACS_LLCORNER ") (add-char scr (acs :llcorner)) (add-string scr " lower left corner ") (new-line scr)
(add-string scr "ACS_URCORNER ") (add-char scr (acs :urcorner)) (add-string scr " upper right corner ") (new-line scr)
(add-string scr "ACS_LRCORNER ") (add-char scr (acs :lrcorner)) (add-string scr " lower right corner ") (new-line scr)
(add-string scr "ACS_LTEE     ") (add-char scr (acs :ltee    )) (add-string scr " tee pointing right ") (new-line scr)
(add-string scr "ACS_RTEE     ") (add-char scr (acs :rtee    )) (add-string scr " tee pointing left ") (new-line scr)
(add-string scr "ACS_BTEE     ") (add-char scr (acs :btee    )) (add-string scr " tee pointing up ") (new-line scr)
(add-string scr "ACS_TTEE     ") (add-char scr (acs :ttee    )) (add-string scr " tee pointing down ") (new-line scr)
(add-string scr "ACS_HLINE    ") (add-char scr (acs :hline   )) (add-string scr " horizontal line ") (new-line scr)
(add-string scr "ACS_VLINE    ") (add-char scr (acs :vline   )) (add-string scr " vertical line ") (new-line scr)
(add-string scr "ACS_PLUS     ") (add-char scr (acs :plus    )) (add-string scr " large plus or crossover ") (new-line scr)
(add-string scr "ACS_S1       ") (add-char scr (acs :s1      )) (add-string scr " scan line 1 ") (new-line scr)
(add-string scr "ACS_S9       ") (add-char scr (acs :s9      )) (add-string scr " scan line 9 ") (new-line scr)
(add-string scr "ACS_DIAMOND  ") (add-char scr (acs :diamond )) (add-string scr " diamond ") (new-line scr)
(add-string scr "ACS_CKBOARD  ") (add-char scr (acs :ckboard )) (add-string scr " checker board (stipple) ") (new-line scr)
(add-string scr "ACS_DEGREE   ") (add-char scr (acs :degree  )) (add-string scr " degree symbol ") (new-line scr)
(add-string scr "ACS_PLMINUS  ") (add-char scr (acs :plminus )) (add-string scr " plus/minus ") (new-line scr)
(add-string scr "ACS_BULLET   ") (add-char scr (acs :bullet  )) (add-string scr " bullet ") (new-line scr)
(add-string scr "ACS_LARROW   ") (add-char scr (acs :larrow  )) (add-string scr " arrow pointing left ") (new-line scr)
(add-string scr "ACS_RARROW   ") (add-char scr (acs :rarrow  )) (add-string scr " arrow pointing right ") (new-line scr)
(add-string scr "ACS_DARROW   ") (add-char scr (acs :darrow  )) (add-string scr " arrow pointing down ") (new-line scr)
(add-string scr "ACS_UARROW   ") (add-char scr (acs :uarrow  )) (add-string scr " arrow pointing up ") (new-line scr)
(add-string scr "ACS_BOARD    ") (add-char scr (acs :board   )) (add-string scr " board of squares ") (new-line scr)
(add-string scr "ACS_LANTERN  ") (add-char scr (acs :lantern )) (add-string scr " lantern symbol ") (new-line scr)
(add-string scr "ACS_BLOCK    ") (add-char scr (acs :block   )) (add-string scr " solid square block ") (new-line scr)
(add-string scr "ACS_S3       ") (add-char scr (acs :s3      )) (add-string scr " scan line 3 ") (new-line scr)
(add-string scr "ACS_S7       ") (add-char scr (acs :s7      )) (add-string scr " scan line 7 ") (new-line scr)
(add-string scr "ACS_LEQUAL   ") (add-char scr (acs :lequal  )) (add-string scr " less/equal ") (new-line scr)
(add-string scr "ACS_GEQUAL   ") (add-char scr (acs :gequal  )) (add-string scr " greater/equal ") (new-line scr)
(add-string scr "ACS_PI       ") (add-char scr (acs :pi      )) (add-string scr " Pi ") (new-line scr)
(add-string scr "ACS_NEQUAL   ") (add-char scr (acs :nequal  )) (add-string scr " not equal ") (new-line scr)
(add-string scr "ACS_STERLING ") (add-char scr (acs :sterling)) (add-string scr " UK pound sign") (new-line scr)

           (get-char scr))
      (close scr))))

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
(defun t14 ()
  (let ((scr (make-instance 'screen :input-echoing nil :input-blocking t :enable-fkeys t)))
    (unwind-protect 
         (progn
           (%mousemask #b00000111111111111111111111111111 (null-pointer)) ; activate all mouse events.
           (get-char scr)
           (with-foreign-object (me '(:struct mevent)) ; create a pointer to the struct mevent.
             (%getmouse me) ; save the mouse event struct to the pointed position.
             (princ (mem-ref me '(:struct mevent)) scr) ; dereference the pointer, return a plist of the struct.
             (get-char scr)))
      (close scr))))

(defun t14a ()
  (with-screen (scr :input-echoing nil :input-blocking nil :enable-fkeys t :cursor-visibility nil)
    (%mousemask #b00000111111111111111111111111111 (null-pointer))
    (flet ((handle-mouse-event ()
             (with-foreign-object (me '(:struct mevent))
               (%getmouse me)
               (let* ((ev (mem-ref me '(:struct mevent)))
                      (x (getf ev 'de.anvi.ncurses::x))
                      (y (getf ev 'de.anvi.ncurses::y))
                      (b (getf ev 'de.anvi.ncurses::bstate))
                      (l (get-mouse-events b)))
                 (format scr "x:~A y:~A b:~32,'0b l:~A~%" x y b l)))))
      (loop
         (let ((event (get-event scr)))
           (if event
               (case event
                 (:mouse (handle-mouse-event))
                 (#\q (return)))
               (sleep 0.01)))))))

(defun t14b ()
  (with-screen (scr :input-echoing nil :input-blocking nil :enable-fkeys t :cursor-visibility nil)
    (%mousemask #b00000111111111111111111111111111 (null-pointer))
    (flet ((handle-mouse-event ()
             (with-foreign-object (me '(:struct mevent))
               (%getmouse me)
               (let* ((ev (mem-ref me '(:struct mevent)))
                      (x (getf ev 'de.anvi.ncurses::x))
                      (y (getf ev 'de.anvi.ncurses::y))
                      (b (getf ev 'de.anvi.ncurses::bstate))
                      (l (get-mouse-events b)))
                 (when (member :button-1-clicked l)
                   (move scr y x)
                   (princ "*" scr))))))
      (loop
         (let ((event (get-event scr)))
           (if event
               (case event
                 (:mouse (handle-mouse-event))
                 (#\q (return)))
               (sleep 0.1)))))))

;; (cffi:convert-to-foreign '(id 1 x 1 y 2 z 3 bstate 2) '(:struct mevent))
;; (setf ev (convert-from-foreign (mem-ref bstate '(:struct mevent)) '(:struct mevent)))
