(in-package :de.anvi.croatoan.test)

(defun snake ()
  (labels ((display-snake (scr body)
             (mapc #'(lambda (pair) (add-char scr (char-code #\*) :y (car pair) :x (cadr pair))) body)))
    (with-screen (scr :input-echoing nil :input-blocking nil :enable-function-keys t :cursor-visible nil)
      (let* ((body '((0 7) (0 6) (0 5) (0 4) (0 3) (0 2) (0 1) (0 0)))
             (xpos (cadar body))
             (ypos (caar body))
             (dir #c(1 0)))
        (clear scr)
        (display-snake scr body)
        (refresh scr)
        (loop
          (let ((event (event-key (get-event scr))))
            (if event
                ;; when the event is different from nil
                (case event
                  (:up    (setf dir #c( 0  1)))
                  (:down  (setf dir #c( 0 -1)))
                  (:right (setf dir #c( 1  0)))
                  (:left  (setf dir #c(-1  0)))
                  (#\q    (return)))
                ;; when event is nil because of input-blocking nil
                (progn
                  (sleep 0.1)
                  (add-char scr (char-code #\space) :y (caar (last body)) :x (cadar (last body)))
                  (setf body (cons (list (incf ypos (- (imagpart dir)))
                                         (incf xpos (realpart dir)))
                                   (butlast body)))
                  (display-snake scr body)
                  (refresh scr)))))))))

(defun snake2 ()
  "Use event-case for event handling. Use lists instead of complex numbers for directions."
  (with-screen (scr :input-echoing nil :input-blocking nil :enable-function-keys t :cursor-visible nil)
    (let* ((body '((0 7) (0 6) (0 5) (0 4) (0 3) (0 2) (0 1) (0 0)))
           (head (car body))
           (tail (car (last body)))
           ;; initial direction
           (dir (get-direction :right)))
      (flet ((draw-snake (win body)
               (mapc (lambda (pos) (add win #\* :position pos)) body)))
        (clear scr)
        (draw-snake scr body)
        (refresh scr)
        (event-case (scr event)
          (#\q
           (return-from event-case))
          ((:right :left :up :down)
           (setq dir (get-direction (event-key event))))
          ((nil)
           (sleep 0.05)
           ;; snake moves = erase last body pair by overwriting it with space
           (add scr #\space :position tail)
           (setq body (cons (mapcar #'+ head dir) (butlast body)))
           (setq head (car body))
           (setq tail (car (last body)))
           (draw-snake scr body)
           (refresh scr)))))))

(defun snake3 ()
  "Use bind and run-event-loop for event handling."
  (with-screen (scr :input-echoing nil :input-blocking nil :enable-function-keys t :cursor-visible nil)
    (let* ((body '((0 7) (0 6) (0 5) (0 4) (0 3) (0 2) (0 1) (0 0)))
           (head (car body))
           (tail (car (last body)))
           ;; initial direction
           (dir (get-direction :right)))
      (flet ((draw-snake (win body)
               (mapc (lambda (pos) (add win #\* :position pos)) body))
             (set-dir (win event)
               (declare (ignore win))
               (setq dir (get-direction (event-key event)))))
        (bind scr #\q 'exit-event-loop)
        (bind scr '(:right :left :up :down) #'set-dir)
        (bind scr nil (lambda (w e)
                        (declare (ignore w e))
                        ;; snake moves = erase last body pair by overwriting it with space
                        (add scr #\space :position tail)
                        (setq body (cons (mapcar #'+ head dir) (butlast body)))
                        (setq head (car body))
                        (setq tail (car (last body)))
                        (draw-snake scr body)
                        (refresh scr)))
        (clear scr)
        (setf (frame-rate scr) 20)
        (run-event-loop scr)))))

;; https://github.com/skydrome/random/blob/master/shell/screensaver.sh
;; https://github.com/pipeseroni/pipes.sh/blob/master/pipes.sh
;; https://www.youtube.com/watch?v=T4n87IIa--U
(defun pipes ()
  (with-screen (scr :input-echoing nil :input-blocking nil :enable-function-keys t :cursor-visible nil)
    (let* ((pos (center-position scr))
           (dirs '((1 0) (-1 0) (0 -1) (0 1)))
           (dir (nth (random 4) dirs))
           (dir-prev dir)
           (ch nil)
           (i 0))
      (clear scr)
      (box scr)
      (refresh scr)
      (event-case (scr event)
        (#\q (return-from event-case))
        ((nil)
         (sleep 0.1)
         (goto scr pos)
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
         (let ((pos-trial (mapcar #'+ pos dir)))
           (if (or (= (car pos-trial)  0)
                   (= (cadr pos-trial) 0)
                   (= (car pos-trial)  (1- (height scr)))
                   (= (cadr pos-trial) (1- (width scr))))
               (setf pos (center-position scr))
               (setf pos pos-trial)))
         (if (> i (random 5))
             (progn (setf i 0)
                    (setf dir-prev dir)
                    (setf dir (nth (random 3) (remove (list (- (car dir)) (- (cadr dir))) dirs :test #'equal))))
             (progn (incf i)
                    (setf dir-prev dir)))
         (refresh scr))))))

(defun matrix ()
  (with-screen (scr :input-echoing nil :input-blocking nil :cursor-visible nil)
    (let* ((width (width scr))
           (height (height scr))
           (positions (loop repeat width collect (random height)))
           (speeds (loop repeat width collect (random 4))))
      (event-case (scr event)
        (#\q (return-from event-case))
        ((nil)
         (sleep 0.05)
         (loop for column from 0 to (1- width) do
              (loop repeat (nth column speeds) do
                   (setf (attributes scr) '(:bold))
                   (setf (color-pair scr) '(:white :black))
                   (add-char scr (+ 64 (random 58)) :y (mod (nth column positions) height) :x column)
                   (setf (color-pair scr) '(:green :black))
                   (add-char scr (+ 64 (random 58)) :y (mod (- (nth column positions) 1) height) :x column)
                   (add-char scr (+ 64 (random 58)) :y (mod (- (nth column positions) 2) height) :x column)
                   (setf (attributes scr) '())
                   (add-char scr (+ 64 (random 58)) :y (mod (- (nth column positions) 3) height) :x column)
                   (add-char scr (char-code #\space) :y (mod (- (nth column positions) (floor height 2)) height) :x column)
                   (refresh scr)
                   (setf (nth column positions) (mod (1+ (nth column positions)) height)))))))))

(defun matrix2 ()
  (with-screen (scr :input-echoing nil :input-blocking nil :cursor-visible nil)
    (let* ((width (width scr))
           (height (height scr))
           ;; start at a random height in each column.
           (positions (loop repeat width collect (random height)))
           ;; run each column at a random speed.
           (speeds (loop repeat width collect (random 4))))
      (flet ((randch () (+ 64 (random 58))))
        ;; hit the q key to exit the main loop.
        (bind scr #\q 'exit-event-loop)
        (bind scr nil
          (lambda (win event)
            (declare (ignore event))
            ;; generate a random ascii char
            (loop for column from 0 to (1- width) do
                 (loop repeat (nth column speeds) do
                      ;; position of the first point in the current column
                      (let ((pos (nth column positions)))
                        (setf (attributes win) '(:bold))
                        (setf (color-pair win) '(:white :black))
                        (add win (randch) :y (mod pos height) :x column)
                        (setf (color-pair win) '(:green :black))
                        (add win (randch) :y (mod (- pos 1) height) :x column)
                        (add win (randch) :y (mod (- pos 2) height) :x column)
                        (setf (attributes win) '())
                        (add win (randch) :y (mod (- pos 3) height) :x column)
                        ;; overwrite the last char with a space
                        (add win #\space  :y (mod (- pos (floor height 3)) height) :x column)
                        (refresh win)
                        ;; increment the column positions
                        (setf (nth column positions) (mod (1+ pos) height)))))))))
    ;; after the handlers have been defined, run the main event loop at 20 fps.
    (setf (frame-rate scr) 20)
    (run-event-loop scr)))

(defun matrix3 ()
  "Test character styles and color pair completion."
  (with-screen (scr :input-echoing nil :input-blocking nil :cursor-visible nil)
    (let* ((width (width scr))
           (height (height scr))
           (positions (loop repeat width collect (random height)))
           (speeds (loop repeat width collect (random 4)))
           (s1 (list :attributes '(:bold) :fgcolor :white))
           (s2 (list :attributes '(:bold) :fgcolor :green :bgcolor nil))
           (s3 (list :attributes nil      :fgcolor :green :bgcolor :black)))
      (flet ((randch () (+ 64 (random 58))))
        (bind scr #\q 'exit-event-loop)
        (bind scr #\r
          (lambda (win event)
            (declare (ignore win event))
            (setf (getf s2 :fgcolor) :red
                  (getf s3 :fgcolor) :red)))
        (bind scr nil
          (lambda (win event)
            (declare (ignore event))
            (loop for column from 0 to (1- width) do
                 (loop repeat (nth column speeds) do
                      (let ((pos (nth column positions)))
                        (add win (randch) :y (mod pos height) :x column :style s1)
                        (add win (randch) :y (mod (- pos 1) height) :x column :style s2)
                        (add win (randch) :y (mod (- pos 2) height) :x column :style s2)
                        (add win (randch) :y (mod (- pos 3) height) :x column :style s3)
                        (add win #\space  :y (mod (- pos (floor height 3)) height) :x column :style s3)
                        (refresh win)
                        (setf (nth column positions) (mod (1+ pos) height)))))))))
    (setf (frame-rate scr) 20)
    (run-event-loop scr)))

(defun matrix4 ()
  "Test passing colors with fgcolor without passing them as pairs."
  (with-screen (scr :input-echoing nil :input-blocking nil :cursor-visible nil)
    (let* ((width (width scr))
           (height (height scr))
           (positions (loop repeat width collect (random height)))
           (speeds (loop repeat width collect (random 4))))
      (flet ((randch () (+ 64 (random 58))))
        (bind scr #\q 'exit-event-loop)
        (bind scr nil
          ;; setting callback-type :object means only the window is passed to the handler
          (lambda (win)
            (loop for column from 0 to (1- width) do
                 (loop repeat (nth column speeds) do
                      (let ((pos (nth column positions)))
                        (setf (attributes win) '(:bold))
                        (setf (fgcolor win) :green)
                        (add win (randch) :y (mod pos height) :fgcolor :white :x column)
                        (add win (randch) :y (mod (- pos 1) height) :x column)
                        (add win (randch) :y (mod (- pos 2) height) :x column)
                        (setf (attributes win) '())
                        (add win (randch) :y (mod (- pos 3) height) :x column)
                        (add win #\space  :y (mod (- pos (floor height 3)) height) :x column)
                        (refresh win)
                        (setf (nth column positions) (mod (1+ pos) height)))))))))
    (setf (frame-rate scr) 20
          ;; here, the event handlers are passed one argument, the object
          ;; if the type is :object-event, handlers are passed two arguments
          (callback-type scr) :object)
    (run-event-loop scr)))

(defun robots ()
  (with-screen (scr :input-echoing nil :input-blocking t :enable-function-keys t :cursor-visible nil)
    (let ((pos (center-position scr))
          (keys '((#\q . :up-left)
                  (#\w . :up)
                  (#\e . :up-right)
                  (#\a . :left)
                  (#\d . :right)
                  (#\y . :down-left)
                  (#\x . :down)
                  (#\c . :down-right)))
          (robots (loop repeat 10 collect (random-position scr))))
      (labels ((move-pos (key)
                 (setq pos (mapcar #'+ pos (get-direction (cdr (assoc key keys))))))
               (manhattan-distance (pos1 pos2)
                 (+ (abs (- (car pos1) (car pos2)))
                    (abs (- (cadr pos1) (cadr pos2)))))
               (robot-alive-p (r)
                 (not (> (loop for r2 in robots count (equalp r r2)) 1)))
               (player-alive-p ()
                 (every #'null (loop for r2 in robots collect (equalp pos r2))))
               (new-robot-position (r)
                 (cdar (sort (loop for key in keys
                                   for new-r = (mapcar #'+ r (get-direction (cdr key)))
                                   collect (cons (manhattan-distance pos new-r) new-r))
                             '< :key #'car)))
               (update-robots ()
                 (setq robots (loop for r in robots collect (if (robot-alive-p r) (new-robot-position r) r)))
                 (unless (player-alive-p) (return-from robots 'you-lose))
                 (when (every #'null (mapcar #'robot-alive-p robots))
                   (return-from robots 'you-win)))
               (draw-board (w)
                 (clear w)
                 (mapc #'(lambda (r) (add w (if (robot-alive-p r) #\A #\#) :position r)) robots)
                 (add w #\@ :position pos)
                 (refresh w)))
        (bind scr #\l 'exit-event-loop)
        (bind scr '(#\q #\w #\e #\a #\d #\y #\x #\c)
              (lambda (w e) (move-pos (event-key e)) (update-robots) (draw-board w)))
        (bind scr #\t (lambda (w e) (setq pos (random-position scr)) (draw-board w)))
        (draw-board scr)
        (run-event-loop scr)))))

(defun pong ()
  (with-screen (scr :input-echoing nil :input-blocking nil :enable-function-keys t :cursor-visible nil)
    (let* ((h (height scr))
           (w (width scr))
           runp     ; ball moving flag
           y x      ; ball
           y1 x1    ; paddle 1
           y2 x2    ; paddle 2
           (n 10)   ; paddle length
           dy dx)   ; increment
      (labels ((draw-paddle (win y x)
                 (draw-line win (list y x) (list (+ y n) x) #\|))
               (draw-game (win)
                 (clear win) (draw-paddle win y1 x1) (draw-paddle win y2 x2) (add-char win #\O :y y :x x) (refresh win))
               (rand () (if (zerop (random 2)) -1 1))
               (reset-game (win)
                 (setq runp nil y (floor h 2) x (floor w 2) y1 5 x1 1 y2 5 x2 (- w 2) dy (rand) dx (rand))
                 (draw-game scr))
               (move-paddle (win event)
                 (case (event-key event)
                   (#\w (when (> y1 0)       (decf y1)))
                   (#\s (when (< (+ y1 n) h) (incf y1)))
                   (#\p (when (> y2 0)       (decf y2)))
                   (#\l (when (< (+ y2 n) h) (incf y2))))
                 (draw-game win)))
        (bind scr #\q 'exit-event-loop)
        (bind scr #\space (lambda (win e) (setq runp t)))
        (bind scr '(#\w #\p #\s #\l) #'move-paddle)
        (bind scr nil (lambda (win e)
                        (when runp
                          (setq y (+ y dy) x (+ x dx))
                          (draw-game win)
                          (when (or (= y 0) (= y (1- h)))
                            (setq dy (- dy)))
                          (when (or (and (= x x1) (<= y1 y (+ y1 n)))
                                    (and (= x x2) (<= y2 y (+ y2 n))))
                            (setq dx (- dx)))
                          (when (or (= x 0) (= x (1- w)))
                            (reset-game win)))))
        (reset-game scr)
        (setf (frame-rate scr) 30)
        (run-event-loop scr)))))

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
         (clear scr)
         (move scr 0 0)

         ;; normal, unrendered chars in default colors.
         (add-string scr "hello there!")

         ;; the text will be red on yellow.
         ;; this affects only new characters, not the whole window.
         (setf (color-pair scr) '(:red :yellow))

         (move scr 5 5)
         (add-string scr "dear john!")
         (refresh scr)

         ;; wait for keypress, works only in blocking mode, which is the default.
         (get-char scr)

         ;; set the background character for new characters
         ;; the optional argument nil prevents it from being applied to every window cell.
         ;; a newline sets the background till the end of the line.
         (setf (background scr nil) (make-instance 'complex-char :simple-char #\- :color-pair '(:white :green)))
         (format scr "~%Hello again!~%")
         (refresh scr)
         (get-char scr)

         ;; finally, set the background for the whole window.
         ;; the change is applied only to empty cells and to
         ;; characters that have no already set attributes or colors.
         (setf (background scr) (make-instance 'complex-char :simple-char #\. :color-pair '(:green :white)))
         (refresh scr)
         (get-char scr)
         (setf (background scr) (make-instance 'complex-char :simple-char #\- :color-pair '(:red :white)))
         (refresh scr)

         ;; wait for the next keypress, then end.
         (get-char scr))

    ;; unwind protect makes sure that ncurses is ended at all cost.
    (end-screen)))

;; the same as t01, but hides the window creation and ncurses ending by utilizing the with-screen macro.
(defun t02 ()
  (with-screen (scr :color-pair (list :yellow :red))
    (clear scr)
    (move scr 0 0)
    (add-string scr "hello there!")
    (move scr 3 6)
    (add-string scr "dear john!")
    (setf (color-pair scr) '(:red :yellow))
    (move scr 3 3 :relative t)
    (add-string scr "call me maybe!")
    ;; setting the cursor position directly instead of using move
    (setf (cursor-position scr) (list 9 12))
    (add-string scr "welcome to tijuana")
    (refresh scr)
    (get-char scr)

    (setf (background scr) (make-instance 'complex-char :simple-char #\. :color-pair '(:green :white)))
    (refresh scr)
    (get-char scr)

    (setf (background scr) (make-instance 'complex-char :simple-char #\, :color-pair '(:white :green)))
    (refresh scr)
    (get-char scr) ))

(defun t02a ()
  "Separately set the window foreground and background color pairs."
  (with-screen (scr :fgcolor :yellow :bgcolor :red)
    (clear scr)
    (move scr 0 0)
    (add-string scr "hello there!")

    (setf (color-pair scr) '(:red :yellow))
    (move scr 3 6)
    (add-string scr "dear john!")

    (setf (fgcolor scr) :yellow
          (bgcolor scr) :red)
    (move scr 3 3 :relative t)
    (add-string scr "call me maybe!")

    ;; setting the cursor position directly instead of using move
    (setf (cursor-position scr) (list 9 12))
    (add-string scr "welcome to tijuana")
    (refresh scr)
    (get-char scr)

    ;; setting fg to green, will set the bg to the default black
    (setf (background scr) (make-instance 'complex-char :simple-char #\. :fgcolor :green))
    (refresh scr)
    (get-char scr)
    ;; setting bg to green will set the fg to the default white
    (setf (background scr) (make-instance 'complex-char :simple-char #\, :bgcolor :green))

    ;; text will still use the window color pair
    (move scr 3 3 :relative t)
    (add scr "hasta siempre")

    (refresh scr)
    (get-char scr) ))

(defun t02b ()
  "Set and get a wide character background."
  (with-screen (scr)
    ;; #x2592 = 9618
    (setf (background scr) (make-instance 'complex-char :simple-char #x2592 :color-pair '(:yellow :red)))
    (move scr 0 0)
    ;; the low-level function returns the code.
    (let ((ch (de.anvi.croatoan::get-background-cchar_t scr)))
      (format scr "ch: ~A~%" ch)
      (format scr "~A ~A ~A" (char-code (simple-char ch)) (attributes ch) (color-pair ch)))
    (move scr 2 0)
    ;; the high level interface returns what was set by the high-level setf.
    ;; TODO: when we set background to :board, should it return :board or the numeric code point?
    (let ((ch (background scr)))
      (if ch
          (format scr "~A ~A ~A" (simple-char ch) (attributes ch) (color-pair ch))
          (format scr "ch: ~A" ch)))
    (refresh scr)
    (get-char scr)))

(defun t02c ()
  (with-screen (scr :input-blocking t :input-echoing nil :enable-colors t :use-terminal-colors t)
    ;; simple chars added to a window without a rendered style.
    (add-string scr "Hello there!")
    (fresh-line scr) (refresh scr) (get-char scr)

    ;; color-pair applies to newly added text.
    (setf (color-pair scr) '(:red :yellow))
    (add-string scr "Dear John!")
    (fresh-line scr) (refresh scr) (get-char scr)

    ;; removing the color pair puts back the output into the default state
    ;; (terminal colors instead of ncurses white on black).
    (setf (color-pair scr) '())
    (add-string scr "Open the pod bay door.")
    (fresh-line scr) (refresh scr) (get-char scr)

    ;; the background style renders simple text, but it doesnt change the text with a set color pair
    (setf (background scr) (make-instance 'complex-char :simple-char #\. :fgcolor :black :bgcolor :magenta))
    (add-string scr "I can feel it.")
    (fresh-line scr) (refresh scr) (get-char scr)

    ;; remove the background char, set back to the default state.
    (setf (background scr) nil)
    (add-string scr "My mind is going.")
    (fresh-line scr) (refresh scr) (get-char scr)

    ;; the empty cells of the last foreground will be overwritten by the next background call.
    (setf (background scr) (make-instance 'complex-char :simple-char #\_ :color-pair '(:black :yellow)))
    (format scr "I'm sorry, Dave.")
    (fresh-line scr) (refresh scr) (get-char scr)))

(defun t02d ()
  "Test precedence of the background char. Compare with nctest9."
  (with-screen (scr :input-echoing nil :cursor-visible t :input-blocking t)
    (let* ((win1 (make-instance 'window :height 10 :width 20 :position (list 3  5) :border t))
           (win2 (make-instance 'window :height 10 :width 20 :position (list 3 40) :border t)))

      (setf (background win1) (make-instance 'complex-char :simple-char :board :fgcolor :yellow)
            (background win2) (make-instance 'complex-char :simple-char #\.    :bgcolor :red))

      ;; the background char is displayed instead of space
      (add-wide-char win1 #\space                  :y 1 :x 1 :n 10)
      ;; the background color is used
      (add-wide-char win1 #\.                      :y 3 :x 1 :n 10)
      ;; the space character is displayed when used with a fgcolor
      (add-wide-char win1 #\space :fgcolor :red    :y 5 :x 1 :n 10)
      ;; the fgcolor overrides the background color
      (add-wide-char win1 #\.     :fgcolor :red    :y 7 :x 1 :n 10)
      ;; the space character is displayed when used with a fgcolor
      (add-wide-char win2 #\space :bgcolor :yellow :y 1 :x 1 :n 10)
      (refresh win1)
      (refresh win2)
      (get-char win1)
      (close win1)
      (close win2))))

(defun t02d2 ()
  "Test setting window properties from the window style."
  (with-screen (scr :input-echoing nil :cursor-visible t :input-blocking t)
    (let* ((win1 (make-instance 'window :height 10 :width 20 :y 3 :x 5 :border t))
           (st2 '(:foreground (:fgcolor :black :bgcolor :white :attributes (:bold))
                  :background (:simple-char #\, :fgcolor :blue :bgcolor :yellow :attributes ())))
           (win2 (make-instance 'window :height 10 :width 20 :position (list 3 40) :border t :style st2)))

      (setf (style win1) '(:foreground (:fgcolor :white :bgcolor :black :attributes (:underline))
                           :background (:simple-char #\. :fgcolor :yellow :attributes ())))

      (add-string win1 "test" :y 2 :x 2)
      (add-string win2 "test" :y 2 :x 2)
      (add-string win2 "best" :y 4 :x 4 :fgcolor :cyan)

      (refresh win1)
      (refresh win2)
      (get-char win1)
      (close win1)
      (close win2))))

(defun t02e ()
  (with-screen (scr :input-echoing nil :cursor-visible t :input-blocking t)
    (let ((win (make-instance 'panel :height 10 :width 20 :position (list 3 5) :border t :border-width 1 :shadow t)))
      (with-slots (border-win shadow-win) win
        (setf (style win)
              '(:border (;:foreground  (:fgcolor :black :bgcolor :white)  ; this would be the style of the drawn border
                         :background  (:fgcolor :blue :bgcolor :white))   ; this of the rest of the border window
                :shadow (:background  (:fgcolor :black :bgcolor :black))
                :foreground (:fgcolor :blue :bgcolor :cyan)
                :background (:simple-char #\. :fgcolor :black :bgcolor :white)))

        (setf (background scr) (make-instance 'complex-char :simple-char :board :fgcolor :cyan))
        (refresh scr)
        (move win 1 1) (add-string win "hello there")
        ;; since a panel is a window, it can be used with stream output functions
        (move win 3 3) (format win "dear john")
        (refresh win)
        (get-char scr)
        (close win)))))

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

;; test which integer is returned by ncurses on a non-blocking nil event.
;; when get-char is used, -1 is returned,
;; when get-wide-char is used, 0 is returned.
(defun t03a2 ()
  (with-screen (scr :input-echoing nil :input-blocking nil)
    (clear scr)
    (add-string scr "Type chars. Type q to quit. ")
    (refresh scr)
    (loop for ch = (get-char scr)
       while (not (equal ch 113)) ; 113 = q
       do (princ ch scr))))

(defun t03b ()
  "Read and display chars until a q is pressed.

Non-blocking version (leads to 100% CPU usage), uses get-event for event handling."
  (with-screen (scr :input-echoing nil :input-blocking nil)
    (clear scr)
    (add-string scr "Type chars. Type q to quit. ")
    (refresh scr)
    (loop (let ((event (event-key (get-event scr))))
            (when event
              (case event
                (#\q (return))
                (otherwise (add-char scr (char-code event)))))))))

;; 200119
(defun t03b1 ()
  "Show the integer key code of an event and the event key type."
  (with-screen (scr :input-echoing nil :input-blocking t :enable-scrolling t)
    (clear scr)
    (format scr "get-char:~%")
    (refresh scr)
    (loop
      (let ((event (get-event scr)))
        (with-accessors ((key event-key) (code event-code)) event
          (when key
            (case key
              (#\q (return))
              (otherwise (format scr "~A ~A ~A~%" key code (type-of key))))))))
    (clear scr)
    (format scr "get-wide-char:~%")
    (refresh scr)
    (loop
      (let ((event (get-wide-event scr)))
        (with-accessors ((key event-key) (code event-code)) event
          (when key
            (case key
              (#\q (return))
              (otherwise (format scr "~A ~A ~A~%" key code (type-of key))))))))))

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
      (otherwise (add-char scr (event-code event))))))

;; slightly improved t03b2 pasted as an example to the cliki croatoan page
(defun t03b3 ()
  (with-screen (scr :input-echoing nil :input-blocking t :enable-colors t)
    (clear scr)
    (move scr 2 0)
    (format scr "Type chars. Type q to quit.~%~%")
    (refresh scr)
    (setf (color-pair scr) '(:yellow :red)
          (attributes scr) '(:bold))
    (event-case (scr event)
      (#\q (return-from event-case))
      (otherwise (princ (event-key event) scr)
                 (refresh scr)))))

;; read and display chars until a q is pressed, blocking + gray stream version.
;; the stream reading function wont work in non-blocking mode and with non-char keys.
;; stream reading and writing uses wide-char functions.
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

(defun t03d ()
  "Read and display wide (multi-byte) characters until q is pressed."
  (with-screen (scr :input-echoing nil :input-blocking t :enable-colors t :cursor-visible nil)
    (clear scr)
    (refresh scr)
    (loop for ch = (get-wide-char scr)
       while (not (equal (code-char ch) #\q))
       do
         (clear scr)
         ;; display the human-readable version of a wide char.
         (add-wide-char scr ch             :attributes (list :underline) :fgcolor :yellow :bgcolor :red  :y 0 :x 0)
         (add-wide-char scr (code-char ch) :attributes (list :bold)      :color-pair (list :yellow :red) :y 0 :x 2)
         ;; extract the wide char again.
         (let ((ch2 (extract-wide-char scr :y 0 :x 0)))
           ;; display the lisp-readable version of the extracted complex wide char
           (move scr 1 0)
           ;; TODO: prin1, print and ~S should print unreadable #<..>
           ;; only princ and ~A should render complex chars
           (prin1 (simple-char ch2) scr)
           ;; print the slots of the extracted complex wide char
           (princ (attributes ch2) scr)
           (princ (color-pair ch2) scr)
           ;; display the rendered complex wide char again
           (add-wide-char scr ch2 :y 3 :x 0) ))))

;; gray stream version of t03d
;; we can not use ~C and write-char to write complex-chars, but it works for wide chars, which are normal lisp chars.
;; princ and ~A should work, because they rely on specialized print-object.
;; also see t08c
(defun t03d2 ()
  "Use gray stream functions to read and display wide (multi-byte) characters until q is pressed."
  (with-screen (scr :input-echoing nil :input-blocking t :enable-colors t :cursor-visible nil)
    (clear scr)
    (refresh scr)
    (loop for ch = (read-char scr)
       while (not (equal ch #\q))
       do
         (clear scr)
         (move scr 0 0)
         ;; we only can write-char if it is a lisp character.
         (write-char ch scr)
         ;; the extracted "char" is not a lisp character any more, but a complex-char
         (let ((ch2 (extract-wide-char scr :y 0 :x 0))
               (*standard-output* scr))
           ;; this soon will not work with sbcl and complex chars.
           ;; write-char only takes characters, no other objects.
           ;; even if we specialize stream-write-char to complex-chars
           ;;(move scr 1 0)
           ;;(write-char ch2 scr)
           ;; this will work because it uses print-objectm, since print-object can be
           ;; specialized on complex-chars.
           (move scr 1 0)
           (princ ch scr)
           (princ ch)
           (move scr 2 0)
           (princ ch2 scr)
           (princ ch2)
           ;; aestethic ~A can be used, since it uses princ underneath
           ;; standard ~S can not  because it cant be read back in
           ;; character ~C can not be used because it requires characters.
           (move scr 3 0)
           (format scr "~A ~A" ch ch2)
           (move scr 4 0)
           (format t "~A ~A" ch ch2)
           (refresh scr) ))))

;;           | cooked | cbreak | raw
;; ----------+--------+--------+-----
;; buffering | t      | nil    | nil
;; ----------+--------+--------+-----
;; control   | t      | t      | nil
;;
(defun t03e ()
  "Test switching between input modes and control char processing."
  (with-screen (scr :input-echoing nil :input-blocking t :input-buffering nil :process-control-chars nil)
    (with-accessors ((input-buffering input-buffering-p) (process-control-chars process-control-chars-p)) scr
      (clear scr)
      (format scr "buffering ~A process-control-chars ~A (raw)~%" input-buffering process-control-chars)
      (format scr "ch1: ~A~%" (get-char scr))
      (setf (process-control-chars-p scr) t)
      (format scr "buffering ~A process-control-chars ~A (cbreak)~%" input-buffering process-control-chars)
      (format scr "ch2: ~A~%" (get-char scr))
      (setf (input-buffering-p scr) t)
      (format scr "buffering ~A process-control-chars ~A (cooked)~%" input-buffering process-control-chars)
      (format scr "ch3: ~A~%" (get-char scr))
      ;; ignore the typed enter key
      (get-char scr)
      ;; wait for one keypress before exiting.
      (get-char scr))))

;; take a function given as symbol name and display its docstring. press q to exit.
;; Example: (a:t04 'cdr)
(defun t04 (&optional (name 'car))
  (unwind-protect
       (let ((scr (make-instance 'screen)))
         (clear scr)
         (refresh scr)

         (setf (attributes scr) '(:bold :reverse))
         (add-string scr "Docstring")

         (setf (attributes scr) '())
         (add-string scr " --> ")

         (setf (attributes scr) '(:bold :underline))
         (add-string scr (format nil "~A~%~%" name))

         (setf (attributes scr) '())
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

         (add-string scr (write-to-string (attributes scr)))
         (add-char scr (char-code #\newline))

         (pushnew :bold (attributes scr))
         (add-string scr (write-to-string (attributes scr)))
         (add-char scr (char-code #\newline))

         (pushnew :underline (attributes scr))
         (add-string scr (write-to-string (attributes scr)))
         (add-char scr (char-code #\newline))

         (pushnew :reverse (attributes scr))
         (add-string scr (write-to-string (attributes scr)))
         (add-char scr (char-code #\newline))

         (setf (attributes scr) (remove :reverse (attributes scr)))
         (add-string scr (write-to-string (attributes scr)))
         (add-char scr (char-code #\newline))

         (setf (attributes scr) (remove :underline (attributes scr)))
         (add-string scr (write-to-string (attributes scr)))
         (add-char scr (char-code #\newline))

         (setf (attributes scr) (remove :bold (attributes scr)))
         (add-string scr (write-to-string (attributes scr)))
         (add-char scr (char-code #\newline))

         (refresh scr)
         (get-char scr))
    (end-screen)))

;; a more concise way to write t04a
(defun t04b ()
  (with-screen (scr :enable-colors t :cursor-visible nil)
    (clear scr)

    (print (attributes scr) scr)

    (add-attributes scr '(:bold))
    (print (attributes scr) scr)

    (add-attributes scr '(:underline))
    (print (attributes scr) scr)

    (add-attributes scr '(:reverse))
    (print (attributes scr) scr)

    (remove-attributes scr '(:bold :underline :reverse))
    (print (attributes scr) scr)

    (add-attributes scr '(:bold :underline :reverse))
    (print (attributes scr) scr)

    (setf (attributes scr) nil)
    (print (attributes scr) scr)
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
;; get into the debugger, but only after we are back to the repl.
(defun t06 ()
  (let ((*debugger-hook* #'(lambda (c h)
                             (declare (ignore c)) (declare (ignore h)) (end-screen))))
    (unwind-protect
         (let ((scr (make-instance 'screen)))
           (add-string scr "press a char to signal an error. ncurses will be ended before going to the debugger.")
           (refresh scr)
           (get-char scr)
           (error "zu huelf!"))
      (end-screen))))

;; The debugger hook is added to with-screen.
;; When an error is signalled in a ncurses app, we cleanly exit ncurses first, we dont get
;; into the debugger and we merely print the signalled condition to the REPL.
(defun t06a ()
  (with-screen (scr :bind-debugger-hook nil)
    (add-string scr "press a char to signal an error. ncurses will be ended before going to the debugger.")
    (refresh scr)
    (get-char scr)
    (error "zu huelf!")))

;; display a new window on the stadard screen.
;; problem: deleting different windows is not standardized. (problem solved, example obsolete)
(defun t07 ()
  (unwind-protect
       (let ((scr (make-instance 'screen :enable-colors t)))
         (clear scr)
         (add-string scr "Standard screen")
         (refresh scr)
         (get-char scr)

         (let ((win (make-instance 'window :height 15 :width 50 :position '(5 5))))
           (setf (background win) (make-instance 'complex-char :color-pair '(:red :blue)))
           (add-string win "Window 1")
           (refresh win)
           (get-char win)))
           ;(delete-window win) ; this is missing, we have to properly delete windows manually.

    ;; this will be executed after we somhow return from the debugger.
    (end-screen)))

;; close now closes both windows and the main screen.
;; but the creation of a window/screen now has to be outside the unwind-protect form.
(defun t07a ()
  (let ((scr (make-instance 'screen :enable-colors t :cursor-visible nil)))
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
             (setf (background win) (make-instance 'complex-char :simple-char #\: :color-pair '(nil :blue)))
             (add-string win "Window 1" :y 2 :x 4 :fgcolor :red :bgcolor :yellow)
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
         (terpri scr)

         (princ "hello")      ; writes to the repl. wont be visible until we quit ncurses.

         (princ "hello" scr)
         (terpri scr)         ; 3 calls, 3 new lines will be added.
         (terpri scr)
         (terpri scr)

         (princ "there" scr)

         (terpri scr)
         (write-string "dear john" scr :start 0 :end 4)
         (terpri scr)
         (write-string "dear john" scr :start 5)

         (fresh-line scr)     ; we call it 3 times, but only one newline will be added.
         (fresh-line scr)
         (fresh-line scr)

         (format scr "--~%~r~%--" 1234) ; each % will add a newline.

         (write-char #\newline scr) ; this is the char that actually gets displayed each time terpri is called.

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

(defun t08b ()
  "Use colors on *standard-output* with standard lisp printing functions.

Test whether a window (stream) was closed."
  (let* ((scr (make-instance 'screen :enable-colors t))
         (*standard-output* scr))
    (unwind-protect
         (progn
           (clear scr)
           (setf (background scr) (make-instance 'complex-char :color-pair '(:black :white)))
           (format t "~r" 1984)
           (refresh scr)
           (get-char scr)

           ;; temporarily bind *standard-output* to a window.
           (let* ((win (make-instance 'window :height 15 :width 50 :position '(5 5)))
                  (*standard-output* win))
             (setf (background win) (make-instance 'complex-char :color-pair '(:white :black)))
             (format t "~r" 1985)
             (refresh win)
             (get-char win)
             (format t "~%before close: streamp: ~A open-stream-p: ~A" (streamp win) (open-stream-p win))
             (refresh win)
             (get-char win)
             (format scr "~%after close: close: ~A streamp: ~A open-stream-p: ~A" (close win) (streamp win) (open-stream-p win))
             (refresh scr)
             (get-char scr))

           ;; *standard-output*is now again scr.
           (setf (background scr) (make-instance 'complex-char :color-pair '(:black :white)))
           (terpri)
           (format t "~r" 1984)
           (refresh scr)
           (get-char scr))
      (close scr))))

;; Write complex chars to standard output using standard lisp output functions + gray streams.
;; According to SBCL devs, write-char and ~C are not supposed to work, because according to the
;; standard, only lisp characters should be accepted s arguments by write-char, and any other
;; object like a complex-char should signal an error.
(defun t08c ()
  (with-screen (scr)
    (let ((*standard-output* scr)
          (ch (make-instance 'complex-char :simple-char #\a :attributes '(:bold :underline) :color-pair '(:green :black))))
      (write-char #\a)
      (terpri)
      ;;(write-char ch)
      (terpri)
      (princ ch)
      (terpri)
      (print ch)
      (terpri)
      (format t "~%Format:~%~S~%~A" ch ch)
      (terpri))
    (refresh scr)
    (get-char scr)))

;; box, move
(defun t09 ()
  (let* ((scr (make-instance 'screen :enable-colors t)))
    (unwind-protect
         (progn
           (clear scr)

           (setf (background scr) (make-instance 'complex-char :color-pair '(:black :white)))
           (box scr)
           (move scr 1 1)
           (princ 0 scr)

           (refresh scr)

           (let ((w1 (make-instance 'window :height 10 :width 30 :position '(3 5)))
                 (w2 (make-instance 'window :height 10 :width 30 :position '(6 10)))
                 (w3 (make-instance 'window :dimensions '(10 30) :position '(9 15))))

             (setf (background w1) (make-instance 'complex-char :color-pair '(:white :black)))
             (setf (background w2) (make-instance 'complex-char :color-pair '(:black :white)))
             (setf (background w3) (make-instance 'complex-char :color-pair '(:white :black)))

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
             (setf (widget-position w3) '(9 20))
             (refresh w3)
             (get-char w3)

             (close w1)
             (close w2)
             (close w3))

           (setf (background scr) (make-instance 'complex-char :color-pair '(:black :white)))

           (refresh scr)
           (get-char scr))

      (close scr))))

;; the same as t09, but we can now raise the overlapping windows by hitting 1, 2 or 3.
(defun t09a ()
  (let* ((scr (make-instance 'screen :enable-colors t :input-blocking t :input-echoing nil ;;:use-terminal-colors t
                             :cursor-visible nil)))
    (unwind-protect
         (progn
           (clear scr)
           (box scr)
           (refresh scr)
           ;; the default value for width or height is "to the end of the screen".
           (let ((w1 (make-instance 'window :height 10 :width 30 :position '(3 5)))
                 (w2 (make-instance 'window            :width 30 :position '(6 10)))
                 (w3 (make-instance 'window :height 10           :position '(9 15))))
             (mapc #'box (list w1 w2 w3))
             ;; after we define a default pair, :terminal -1 refers to these colors.
             ;;(setf (default-color-pair scr) (list :yellow :red))
             (setf (use-terminal-colors-p scr) t)

             (setf (background w1) (make-instance 'complex-char :color-pair '(:default-fg :yellow)))
             ;; window w2 uses the :default fg and bg color of the terminal, because use-terminal-colors is t.
             (setf (background w3) (make-instance 'complex-char :color-pair '(:yellow :terminal)))

             ;; print currently active color pairs to w3
             (format w3 "~A" de.anvi.croatoan::*color-pair-alist*)

             (mapc #'refresh (list w1 w2 w3))
             (loop (let ((event (event-key (get-event scr))))
                     (when event
                       (case event
                         (#\1 (touch w1) (refresh w1))
                         (#\2 (touch w2) (refresh w2))
                         (#\3 (touch w3) (refresh w3))
                         (#\q (return))
                         (otherwise nil)))))
             (mapc #'close (list w1 w2 w3))))
      (close scr))))

(defun t09b ()
  "Use with-screen, event-case and mapc to simplify t09a."
  (with-screen (scr :input-blocking t :input-echoing nil :enable-colors t :cursor-visible nil)
    ;; we cant set :border t for a screen because with-screen calls clear bofore executing the body
    (box scr)

    ;; omitting width or height implies stretching that dimension to the end of the screen
    (let ((w1 (make-instance 'window :height 10 :width 30 :position '(3 5)  :border t))
          (w2 (make-instance 'window            :width 30 :position '(6 10) :border t))
          (w3 (make-instance 'window :height 10           :position '(9 15) :border t)))
      ;; we dont have to explicitely give a simple char, a space will be assumed if the char is missing
      (setf (background w1) (make-instance 'complex-char :simple-char #\+ :color-pair '(:white :yellow))
            (background w3) (make-instance 'complex-char :simple-char #\. :color-pair '(:white :black)))
      (mapc #'touch (list w1 w2 w3))
      (mapc #'refresh (list w1 w2 w3))

      ;; press 123 to raise the window.
      ;; if windows are overlapping, we need to touch+refresh a window to raise it to the top.
      ;; without a touch, it isnt completely redrawn.
      (event-case (w1 event)
        (#\1 (touch w1) (refresh w1))
        (#\2 (touch w2) (refresh w2))
        (#\3 (touch w3) (refresh w3))
        (#\q (return-from event-case))
        (otherwise nil))
      (mapc #'close (list w1 w2 w3))
      ;; return nil explicitely, so it doesnt return the window list.
      nil)))

(defun t09c ()
  "Use a window stack to manage overlapping windows."
  (with-screen (scr :input-blocking t :input-echoing nil :enable-colors t :enable-function-keys t :cursor-visible nil)
    (box scr)
    (setf (background scr) (make-instance 'complex-char :simple-char #\. :color-pair '(:black :white)))
    (refresh scr)
    (let ((n 0) ; current position in the stack
          (stack (make-instance 'stack)))
      ;; create 8 windows (with 8 different background colors), push them to the stack
      (dotimes (i 8)
        (stack-push (make-instance 'window :height 10 :width 30
                                           ;;:position (list (+ 3 (* i 1)) (+ 3 (* i 3)))
                                           :y (+ 3 (* i 1))
                                           :x (+ 3 (* i 3))
                                           :border t :visible t
                                           :background (make-instance 'complex-char :fgcolor :black :bgcolor (list :number i)))
                    stack))
      (refresh stack)
      (event-case (scr event)
        ;; type 0-7 to pick a window
        ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7)
         ;; show the chosen window number in the upper right screen corner
         (add-char scr (event-key event) :y 1 :x 78 :color-pair (list :white :black))
         (setf n (- (event-code event) 48)))
        ;; move the chosen window in the stack
        (#\t (stack-move n :top stack)    (setq n 0)              (refresh stack)) ; raise a window to the top
        (#\r (stack-move n :up stack)     (when (> n 0) (decf n)) (refresh stack)) ; raise window n one position
        (#\l (stack-move n :down stack)   (when (< n 7) (incf n)) (refresh stack)) ; lower window n one position
        (#\b (stack-move n :bottom stack) (setq n 7)              (refresh stack)) ; lower window n to the bottom
        ;; type v to toggle window visibility
        (#\v (setf (visiblep (nth n (items stack)))
                   (not (visiblep (nth n (items stack)))))
             ;; this is the equivalent of (refresh *main-stack*), but only if scr is on the main stack, i.e. :stacked t
             ;; if it isnt, we have to manually touch+refresh scr to prevent partial redraws.
             (touch scr) (refresh scr)
             (refresh stack))
        (#\q (return-from event-case)))
      ;; close the ncurses windows, then empty the stack
      (mapc #'close (items stack))
      (empty-stack stack))
    ;; return nil explicitely, so it doesnt return the window list.
    nil))

;; enable-function-keys t: fkeys have codes 255+
;; enable-function-keys nil: fkeys are multi-char escape codes.
(defun t10 ()
  "Print the screen size. Print the char and code of every narrow char pressed.

enable-function-keys t: fkeys have codes 255+

Here, function keys are translated by ncurses to integers but not
given keyword names by get-char, that is done by get-event.

enable-function-keys nil: fkeys are printed as multi-char escape codes.

Wide chars are not recognized and are displayed byte by byte.

When the end of the screen is reached, the ~% directive doesnt print
newlines any more and the screen doesnt scroll."
  (let ((scr (make-instance 'screen :input-echoing nil :enable-function-keys t)))
    (unwind-protect
         (progn
           (clear scr)
           (format scr "~A lines high, ~A columns wide.~%~%" (height scr) (width scr))
           (refresh scr)
           (loop
              ;; act only on events, do nothing on non-events.
              (when (key-pressed-p scr)
                  (let ((ch (get-char scr)))
                    ;; quit on q, print everything else including function keys.
                    (cond ((equal (code-char ch) #\q) (return))
                          (t (format scr "Code: ~A, Char: ~A~%" ch (code-char ch))))))))
      (close scr))))

;; https://rosettacode.org/wiki/Keyboard_input/Keypress_check#Common_Lisp
(defun t10a1 ()
  (with-screen (scr :input-echoing nil :input-blocking nil :input-buffering nil :enable-function-keys t)
    (loop
      ;; Determine if a key has been pressed ...
      (if (key-pressed-p scr)
          ;; ... and store this in a variable.
          (let ((ch (get-char scr)))
            ;; exit the loop by pressing q.
            (if (eql (code-char ch) #\q)
                (return)
                (princ (code-char ch) scr)))
          (progn
            ;; If no key has been pressed, the program should continue without waiting.
            (princ #\. scr)
            (refresh scr)
            ;; we wait anyway to spare the CPU.
            (sleep 0.15))))))

(defun t10a3 ()
  (with-screen (scr :input-echoing nil :input-blocking nil :input-buffering nil :enable-function-keys t)
    ;; If no key has been pressed, the main loop produces a single tick.
    (bind scr nil (lambda (win event)
                    (declare (ignore event))
                    (princ #\. win)
                    (refresh scr)))
    ;; If a key was pressed, it is processed immediately.
    (bind scr t (lambda (win event)
                  (princ (event-key event) win)
                  (refresh win)))
    (bind scr #\q 'exit-event-loop)
    ;; a frame rate of 5 ticks per second sets a blocking delay of 1000/5 = 200 milliseconds.
    (setf (frame-rate scr) 5)
    (run-event-loop scr)))

(defun t10a ()
  "Print the screen size. Scrolling is enabled.

get-event recognizes the function key codes and returns their keyword
names.

get-event returns only single bytes, so multibyte characters are
returned byte by byte as with get-char."
  (with-screen (scr :input-echoing nil :input-blocking t :enable-function-keys t
                    :input-buffering nil :process-control-chars nil :enable-newline-translation t)
    (setf (scrolling-enabled-p scr) t)
    (format scr "~A lines high, ~A columns wide.~%~%" (height scr) (width scr))
    ;; get-event explicitely gets single-byte events, not wide events.
    (loop (let ((event (event-key (get-event scr))))
            (when event
              (case event
                (#\q (return))
                ;; non-printable ascii chars. (#\space and #\newline are standard, all others non-standard)
                (#\escape (format scr "escape char ESC ^[ \e~%"))
                (#\tab (format scr "horizontal tab char HT \t~%"))
                (#\space (format scr "space char~%"))

                ;; NL can be either LF \n,CR \r,or CRLF \r\n, depending on the system. it is LF on ubuntu.
                ;; NL is the standard, system independent, portable way.
                (#\linefeed (format scr "enter/linefeed LF ^J \n char~%"))
                (#\return (format scr "enter/return CR ^M \r char~%"))
                (#\newline (format scr "enter/newline char~%")) ; = #\linefeed, code 10.

                ;; use %nonl to prevent ^M (CR) from automatically being translated to ^J (LF)
                (#\n (setf (newline-translation-enabled-p scr)
                           (not (newline-translation-enabled-p scr)))
                     (if (newline-translation-enabled-p scr)
                         (format scr "%nl newline translation enabled: RET => NL (= LF)~%")
                         (format scr "%nonl newline translation disabled: RET => CR~%")))
                ;; DEL, ^?, delete char 127
                (#\rubout (format scr "rubout char ^?~%"))
                ;; BS, \b, ^H, code 8, not the same as the :backspace key
                (#\backspace (format scr "backspace char ^H~%"))

                ;; function keys, the same as #\rubout, but different code.
                ;; ncurses bug: :backspace is returned for windows, #\rubout for stdscr.
                (:backspace (format scr "backspace key <--~%"))
                ;; printable chars (graphic and control chars)
                (otherwise
                 (add-string scr (format nil "~A ~S~%" event event))
                 (format scr "Event: ~A ~S ~A~%" event event (type-of event)))))))))

(defun t10b ()
  (with-screen (scr :input-echoing nil :input-blocking t :enable-function-keys t :enable-scrolling t
                    :input-buffering nil :process-control-chars nil :enable-newline-translation t)
    ;; add existing, but unportable keys to the key alist. (here: xterm 322 on ncurses 6.2)
    (mapc
     (lambda (a)
       (add-function-key (car a) (cdr a)))

     '((:alt-delete          . 517)
       (:shift-alt-delete    . 518)
       (:ctrl-delete         . 519)
       (:shift-ctrl-delete   . 520)
       ;;(:shift-ctrl-alt-delete . xxx) ^[[3;8~

       (:alt-end             . 528)
       (:shift-alt-end       . 529)
       (:ctrl-end            . 530)
       (:shift-ctrl-end      . 531)
       (:ctrl-alt-end        . 532)
       ;;(:shift-ctrl-alt-end . xxx) ^[[1;8F

       (:alt-home            . 533)
       (:shift-alt-home      . 534)
       (:ctrl-home           . 535)
       (:shift-ctrl-home     . 536)
       (:ctrl-alt-home       . 537)
       ;;(:shift-ctrl-alt-home . xxx) ^[[1;8H

       (:alt-insert          . 538)
       ;; :shift-insert = middle mouse button paste, probably 513, hijacked by xterm.
       (:ctrl-insert         . 540)
       (:ctrl-alt-insert     . 542)

       (:alt-npage           . 548)
       (:ctrl-npage          . 550)
       (:ctrl-alt-npage      . 552)

       (:alt-ppage           . 553)
       (:ctrl-ppage          . 555)
       (:ctrl-alt-ppage      . 557)

       ;; :shift-up = :sr = 337
       (:alt-up              . 564)
       (:shift-alt-up        . 565)
       (:ctrl-up             . 566)
       (:shift-ctrl-up       . 567)
       ;; (:shift-alt-ctrl-up . xxx)

       ;; :shift-down = :sf = 336
       (:alt-down            . 523)
       (:shift-alt-down      . 524)
       (:ctrl-down           . 525)
       (:shift-ctrl-down     . 526)
       ;; (:shift-alt-ctrl-down . xxx) ;; hijacked by the ubuntu unity wm.

       (:alt-left            . 543)
       (:shift-alt-left      . 544)
       (:ctrl-left           . 545)
       (:shift-ctrl-left     . 546)

       (:alt-right           . 558)
       (:shift-alt-right     . 559)
       (:ctrl-right          . 560)
       (:shift-ctrl-right    . 561)))

    ;; define new keys by their escape control sequences
    ;; the automatically generated new codes start at 1024 to avoid conflicts with codes generated by ncurses.

    ;; gnome-terminal, linux console
    (define-function-key :alt-1 (list #\esc #\1))
    (define-function-key :alt-n (list #\esc #\n))

    ;; xterm
    ;;                      f1  - f12
    ;; shift-f1           = f13 - f24
    ;; ctrl-f1            = f25 - f36
    ;; shift-ctrl-f1      = f37 - f48
    ;; alt-f1             = f49 - f60 some hijacked by ubuntu unity
    ;; shift-alt-f1       = f61 - f63 bindings f4-f12 not defined
    ;; ctrl-alt-f1        = linux virtual terminals
    ;; shift-ctrl-alt-f1  = not defined

    ;; xterm
    (define-function-key :shift-alt-f4 (list #\esc #\[ #\1 #\; #\4 #\S))
    (define-function-key :shift-alt-f5 (list #\esc #\[ #\1 #\5 #\; #\4 #\~))
    (define-function-key :shift-alt-f6 (list #\esc #\[ #\1 #\7 #\; #\4 #\~))
    (define-function-key :shift-alt-f7 (list #\esc #\[ #\1 #\8 #\; #\4 #\~))
    (define-function-key :shift-alt-f8 (list #\esc #\[ #\1 #\9 #\; #\4 #\~))
    (define-function-key :shift-alt-f9 (list #\esc #\[ #\2 #\0 #\; #\4 #\~))
    (define-function-key :shift-alt-f10 (list #\esc #\[ #\2 #\1 #\; #\4 #\~))
    (define-function-key :shift-alt-f11 (list #\esc #\[ #\2 #\3 #\; #\4 #\~))
    (define-function-key :shift-alt-f12 (list #\esc #\[ #\2 #\4 #\; #\4 #\~))

    (define-function-key :shift-ctrl-alt-f1 (list #\esc #\[ #\1 #\; #\8 #\P))
    (define-function-key :shift-ctrl-alt-f2 (list #\esc #\[ #\1 #\; #\8 #\Q))
    (define-function-key :shift-ctrl-alt-f3 (list #\esc #\[ #\1 #\; #\8 #\R))
    (define-function-key :shift-ctrl-alt-f4 (list #\esc #\[ #\1 #\; #\8 #\S))
    (define-function-key :shift-ctrl-alt-f5 (list #\esc #\[ #\1 #\5 #\; #\8 #\~))
    (define-function-key :shift-ctrl-alt-f6 (list #\esc #\[ #\1 #\7 #\; #\8 #\~))
    (define-function-key :shift-ctrl-alt-f7 (list #\esc #\[ #\1 #\8 #\; #\8 #\~))
    (define-function-key :shift-ctrl-alt-f8 (list #\esc #\[ #\1 #\9 #\; #\8 #\~))
    (define-function-key :shift-ctrl-alt-f9 (list #\esc #\[ #\2 #\0 #\; #\8 #\~))
    (define-function-key :shift-ctrl-alt-f10 (list #\esc #\[ #\2 #\1 #\; #\8 #\~))
    (define-function-key :shift-ctrl-alt-f11 (list #\esc #\[ #\2 #\3 #\; #\8 #\~))
    (define-function-key :shift-ctrl-alt-f12 (list #\esc #\[ #\2 #\4 #\; #\8 #\~))

    (define-function-key :shift-ctrl-alt-home (list #\esc #\[ #\1 #\; #\8 #\H))
    (define-function-key :shift-ctrl-alt-end (list #\esc #\[ #\1 #\; #\8 #\F))
    (define-function-key :shift-ctrl-alt-delete (list #\esc #\[ #\3 #\; #\8 #\~))

    (bind scr #\q 'exit-event-loop)
    (bind scr t (lambda (w e) (format w "Event: ~A ~A~%" (event-key e) (event-code e))))
    (run-event-loop scr)))

(defun t11 ()
  "Demonstrate automatic scrolling and scrolling regions."
  (let ((scr (make-instance 'screen)))
    (unwind-protect
         (progn
           (clear scr)
           (refresh scr)
           (setf
            ;; identical to move
            (cursor-position scr) '(0 0)
            ;; make the whole window scroll when the last line is reached.
            (scrolling-enabled-p scr) t
            ;; to make only a few lines (5 to 10) scroll, set a line-based region.
            (scrolling-region scr) '(5 10))
           (dotimes (i 20)
             (format scr "~A~%" i)
             (refresh scr)
             (sleep 0.5)))
      (close scr))))

(defun t11a ()
  "Demonstrate the manual use of the scroll function for the screen."
  (with-screen (scr :input-echoing nil :input-blocking t :enable-function-keys t :enable-scrolling t)
    (dotimes (i (height scr))
      (put scr i 0 (format nil "~A" i)))
    (refresh scr)
    (event-case (scr event)
      (#\q (return-from event-case))
      (:up (scroll scr 1) (refresh scr))
      (:down (scroll scr -1) (refresh scr)))))

(defun t11b ()
  "Demonstrate the manual use of the scroll function for a window."
  (with-screen (scr :input-echoing nil :input-blocking t :enable-colors t)
    (with-window (win :position '(3 6) :dimensions '(10 20) :enable-function-keys t :enable-scrolling t)
      (setf (background win) (make-instance 'complex-char :color-pair '(:black :white)))
      (dotimes (i (height win))
        (put win i 0 (format nil "~A" i)))
      (refresh win)
      (event-case (win event)
        (#\q (return-from event-case))
        (:up (scroll win 1) (refresh win))
        (:down (scroll win -1) (refresh win))))))

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

(defun t12c2 ()
  "ACS chars not available on all terminals: thick and double line drawing characters.

Tested with xterm, gnome-terminal, st. Doesn't work in the linux console, aterm, eterm."
  (with-screen (scr)
    (loop for i from 0 for symbol in
         '(:thick-upper-left-corner
           :thick-lower-left-corner
           :thick-upper-right-corner
           :thick-lower-right-corner
           :thick-tee-pointing-right
           :thick-tee-pointing-left
           :thick-tee-pointing-up
           :thick-tee-pointing-down
           :thick-horizontal-line
           :thick-vertical-line
           :thick-crossover-plus)
       do (progn (move scr (+ 1 (* i 2)) 2)
                 (add-char scr symbol)
                 (format scr "  ~S~%~%" symbol)))

    (loop for i from 0 for symbol in
         '(:double-upper-left-corner
           :double-lower-left-corner
           :double-upper-right-corner
           :double-lower-right-corner
           :double-tee-pointing-right
           :double-tee-pointing-left
           :double-tee-pointing-up
           :double-tee-pointing-down
           :double-horizontal-line
           :double-vertical-line
           :double-crossover-plus)
       do (progn (move scr (+ 1 (* i 2)) 40)
                 (add-char scr symbol)
                 (format scr "  ~S~%~%" symbol)))
    (refresh scr)
    (get-char scr)))

(defun t12d ()
  "Print all graphic alternative chars supported by the non-wide acs_map[128] array.

Only the chars 33 to 126 are graphic, and not all are accessible through the
keywords provided by ncurses, and the supported chars are terminal dependent."
  (with-screen (scr)
    (let* ((ptr (cffi:foreign-symbol-pointer "acs_map"))
           (code (cffi:mem-aref ptr :unsigned-int (char-code #\l))))
      (format scr "~A ~A ~A ~A " #\l (char-code #\l) (acs :upper-left-corner) code)
      ;; l 108 4194412 4194412 ***
      ;; 4194412 is nothing more than 108 with the A_ALTCHARSET bit turned on.
      (add-char scr code)
      (add-char scr (acs :upper-left-corner))
      (add-char scr #\l :attributes (list :altcharset))
      (terpri scr)
      ;; so we do not really need to access acs_map, like acs is doing.
      ;; we just need to translate chars like #\l to :upper-left-corner
      (loop for i from 33 to 126 do
           (add-char scr (code-char i))
           (add-char scr #\space))
      (terpri scr)
      (loop for i from 33 to 126 do
           (add-char scr (cffi:mem-aref ptr :unsigned-int i))
           (add-char scr #\space))
      (refresh scr)
      (get-char scr))))

;; Demonstrate flash and beep alerts.
;; It depends on the terminal emulator whether they will work for you.
;; They both worked in xterm for me.
(defun t13 ()
  (with-screen (scr :input-echoing nil :input-blocking nil :enable-function-keys t :cursor-visible nil)
    (loop
       (let ((event (event-key (get-event scr))))
         (if event
             (case event
               (#\b (alert :beep))
               (#\f (alert :flash))
               (#\q (return)))
             (sleep 0.1))))))

(defun t14 ()
  "Minimal setting to get the mouse working, read and print a single mouse event."
  (let ((scr (make-instance 'screen :input-echoing nil :input-blocking t :enable-function-keys t)))
    (unwind-protect
         (progn
           ;; activate all mouse events.
           (ncurses:mousemask #b00000111111111111111111111111111 (cffi:null-pointer))
           ;; click to generate a mouse event.
           (get-char scr)
           ;; create a pointer to the struct mevent.
           (cffi:with-foreign-object (me '(:struct ncurses:mevent))
             ;; save the mouse event struct to the pointed position.
             (ncurses:getmouse me)
             ;; dereference the pointer, return a plist of the struct.
             (princ (cffi:mem-ref me '(:struct ncurses:mevent)) scr)
             (get-char scr)))
      (close scr))))

;; (cffi:convert-to-foreign '(id 1 x 1 y 2 z 3 bstate 2) '(:struct mevent))
;; (setf ev (convert-from-foreign (mem-ref bstate '(:struct mevent)) '(:struct mevent)))

(defun t14a ()
  "Mouse events are now detected in the event loop, print the y x coordinates and the event."
  (with-screen (scr :input-echoing nil :input-blocking t :enable-function-keys t :cursor-visible nil)
    ;; Set the events to be reported.
    (set-mouse-event '(:button-1-clicked :button-2-clicked :button-3-clicked))
    (event-case (scr event)
      ((:button-1-clicked :button-2-clicked :button-3-clicked)
       (format scr "~3A ~3A ~A ~A~%" (position-y event) (position-x event) (event-key event) (event-modifiers event)))
      (#\q
       (return-from event-case)))))

(defun t14b ()
  "A left click prints a 1, right click prints a 3, use event-case to handle mouse events."
  (with-screen (scr :input-echoing nil :input-blocking t :enable-function-keys t :cursor-visible nil)
    (set-mouse-event '(:button-1-clicked :button-3-clicked))
    (event-case (scr event)
      (:button-1-clicked
       (move scr (position-y event) (position-x event)) (princ "1" scr))
      (:button-3-clicked
       (move scr (position-y event) (position-x event)) (princ "3" scr))
      (#\q
       (return-from event-case)))))

(defun t14b1 ()
  "A left click prints a 1, right click prints a 3, use bind/run-event-loop to handle mouse events."
  (with-screen (scr :input-echoing nil :input-blocking t :enable-function-keys t :cursor-visible nil)
    (set-mouse-event '(:button-1-clicked :button-3-clicked))
    (bind scr :button-1-clicked
          (lambda (w e)
            (goto w (event-position e)) (princ "1" w)))
    (bind scr :button-3-clicked
          (lambda (w e)
            (move w (position-y e) (position-x e)) (princ "3" w)))
    (bind scr #\q 'exit-event-loop)
    (run-event-loop scr)))

(defun t14c ()
  "Print mouse event details, use event-case for event handling."
  (with-screen (scr :input-echoing nil :input-blocking t :enable-function-keys t :cursor-visible nil)
    (ncurses:mousemask #b00000111111111111111111111111111 (cffi:null-pointer))
    (event-case (scr event)
      (#\q (return-from event-case))
      (t (when (= (cursor-position-y scr) (1- (height scr)))
           (clear scr))
         (if (typep event 'mouse-event)
             (format scr "~3A ~3A ~A~%" (position-y event) (position-x event) (event-key event))
             (format scr "~A ~A ~A~%" event (event-key event) (event-code event)))))))

(defun t14c1 ()
  "Print mouse event details with bind/run-event-loop for event handling."
  (with-screen (scr :input-echoing nil :input-blocking t :enable-function-keys t :cursor-visible nil)
    ;; :all-mouse-events is a bitmask for button events
    ;; :report-mouse-position has to be activated explicitely
    (set-mouse-event '(:all-mouse-events :report-mouse-position))
    (bind scr #\q 'exit-event-loop)
    (bind scr t
          (lambda (w e)
            (when (= (cursor-position-y w) (1- (height w)))
              (clear w))
            (if (typep e 'mouse-event)
                (format w "~3A ~3A ~A ~A~%" (position-y e) (position-x e) (event-key e) (event-modifiers e))
                (format w "~A ~A ~A~%" e (event-key e) (event-code e)))))
    (run-event-loop scr)))

;; resize event: the standard screen size is resized automatically.
(defun t15 ()
  (with-screen (scr :input-echoing nil :input-blocking nil :enable-function-keys t :cursor-visible nil :enable-colors t)
    (add-string scr "Current standard screen geometry (Y x X):" :y 0 :x 0)
    (loop
       (let ((event (event-key (get-event scr))))
         (if event
             (case event
               (:resize (move scr 1 0)
                        (format scr "~A Y lines x ~A X columns." (height scr) (width scr))
                        ;; the environment variables get updated on a resize event too.
                        (format scr "~%~A Y lines x ~A X cols." ncurses:LINES ncurses:COLS)
                        (refresh scr))
               (#\q (return)))
             (progn
               (sleep 0.1)))))))

;; resize event: arrange window _positions_ relative to the screen size.
(defun t15a ()
  (with-screen (scr :input-echoing nil :input-blocking nil :enable-function-keys t :cursor-visible nil :enable-colors t)
    (add-string scr "Current standard screen geometry (Y x X):" :y 0 :x 0)
    (setf (background scr) (make-instance 'complex-char :simple-char #\. :color-pair '(:green :white)))
    (let ((time 0)
          ;; place a window in the center of the screen.
          (win (make-instance 'window :height 5 :width 10 :position (list (round (/ (height scr) 2))
                                                                          (round (/ (width scr) 2))))))
      (loop
         (let ((event (event-key (get-event scr))))
           (if event
               (case event
                 (:resize ;; if the screen is resized, relocate the window to the new center.
                          (move-window win (round (/ (height scr) 2)) (round (/ (width scr) 2)))
                          ;; better differentiation of types with methods.
                          (move win 0 0)
                          (format win "Y:~A X:~A" (height scr) (width scr))
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
  (with-screen (scr :input-echoing nil :input-blocking nil :enable-function-keys t :cursor-visible nil :enable-colors t)
    (add-string scr "Current standard screen geometry (Y x X):" :y 0 :x 0)
    (setf (background scr) (make-instance 'complex-char :color-pair '(:black :white)))
    (let ((time 0)
          ;; make the window slightly smaller than the standard screen.
          (win (make-instance 'window :height (- (height scr) 4) :width (- (width scr) 6) :position '(2 3))))
      (loop
         (let ((event (event-key (get-event scr))))
           (if event
               (case event
                 (:resize ;; resize the window on every terminal resize.
                          (resize win (- (height scr) 4) (- (width scr) 6))
                          (move win 0 0)
                          (format win "Y:~A X:~A" (height scr) (width scr))
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
  (with-screen (scr :input-echoing t :input-blocking t :enable-function-keys t :cursor-visible nil :enable-colors nil)
    (let ((out (make-instance 'window :height (1- (height scr)) :width (width scr) :position '(0 0)))
          (in (make-instance 'window :height 1 :width (width scr) :position (list (1- (height scr)) 0))))

      (print (eval (read-from-string (get-string in 30))) out)
      (refresh out)

      ;; blocking is t, so wait till the next keypress before exiting.
      (get-char in)

      ;; close windows and window streams.
      (close in)
      (close out))))

;; add a loop to the input, making it a simple repl.
(defun t16b ()
  (with-screen (scr :input-echoing t :input-blocking t :enable-function-keys t :cursor-visible t :enable-colors nil)
    (let ((out (make-instance 'window :height (1- (height scr)) :width (width scr) :position '(0 0)))
          (in (make-instance 'window :height 1 :width (width scr) :position (list (1- (height scr)) 0))))
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
  (with-screen (scr :input-echoing nil :cursor-visible t :enable-colors t)
    (let* ((wout (make-instance 'window :height (1- (height scr)) :width (width scr) :position '(0 0) :enable-scrolling t))
           ;; input blocking is a property of every single window, not just of the global screen.
           (win (make-instance 'window :height 1 :width (width scr) :position (list (1- (height scr)) 0)
                               :enable-function-keys t :input-blocking t))
           (*standard-output* wout)
           (n 0)) ; no of chars in the input line.
      (event-case (win event)
        (:left
         (when (> (cadr (cursor-position win)) 0)
           (move win 0 -1 :relative t)))
        (:right
         (when (< (cadr (cursor-position win)) n)
           (move win 0 1 :relative t)))
        (#\newline ; RET key, C-j, C-m
           (when (> n 0) ; only print when the line is not empty.
             (let* ((strin (extract-wide-string win :n n :y 0 :x 0))
                    (strout (eval (read-from-string strin))))
               (print (length strin))
               (princ strin)
               (terpri)
               (add-string wout (format nil "=> ~A~%~%" strout)))
             (setf n 0)
             (clear win) ; empty the input line after evaluation.
             (refresh wout)))
        (:dc ; DEL key
         (when (> n (cadr (cursor-position win)))
           (decf n)
           (delete-char win)))
        (:ic ; INS / Einfg key
         (format t "(insert-mode-p win) => ~A~%" (insert-mode-p win))
         (setf (insert-mode-p win) (not (insert-mode-p win)))
         (format t "(insert-mode-p win) => ~A~%" (insert-mode-p win))
         (refresh wout))
        (:backspace ; BS key
         (when (> (cadr (cursor-position win)) 0)
           (decf n)
           (move win 0 -1 :relative t)
           (delete-char win)))
        (#\q (return-from event-case))
        ((nil) ; when no key is hit at all
         ;; when there is no event, get-event will return a nil event.
         ;; this is the place for no-event code.
         ;; instead of doing nothing, set blocking to t.
         nil)
        ;; non-function keys, i.e. normal character keys
        (otherwise
         (when (and (characterp (event-key event))
                    (< (cadr (cursor-position win)) (1- (width win))))
           (incf n)
           ;; insert-mode-p does not insert if we do not use gray stream functions
           ;;(add-wide-char win event))))
           (write-char (event-key event) win)))) ; calls stream-write-char
           ;; (princ event win) ; calls print-object
      (close win)
      (close wout))))

;; buffer: (3 2 1)
;; screen: 123
;; in the buffer, elements are added to the left and counted from the left.
;; on the screen, the list is displayed in reverse.
(defun t16d ()
  "Use an input buffer instead of extracting the string from the window. Create windows using the with-windows macro."
  (with-screen (scr :input-echoing nil :cursor-visible t :enable-colors t)
    (with-windows ((wout :height (1- (height scr)) :width (width scr) :position '(0 0) :enable-scrolling t)
                   (win  :height 1                 :width (width scr) :position (list (1- (height scr)) 0) :enable-function-keys t :input-blocking t))
      (let ((*standard-output* wout)
            (inbuf nil) ; input buffer character list
            (inptr 0))  ; position of the next character in the buffer
        (event-case (win event)
          (#\q (return-from event-case))
          (:left
           (when (> inptr 0) (decf inptr))
           (move win 0 inptr))
          (:right
           (when (< inptr (length inbuf)) (incf inptr))
           (move win 0 inptr))
          (#\newline
           (when (> (length inbuf) 0)
             (format t "~A~%" (coerce (reverse inbuf) 'string)) (refresh wout)
             (setf inbuf nil inptr 0)
             (clear win)
             (move win 0 inptr)
             (refresh win)))
          (:dc
           (when (> (length inbuf) inptr)
             (setf inbuf (remove-nth (- (length inbuf) (1+ inptr)) inbuf))
             (clear win)
             (add-string win (coerce (reverse inbuf) 'string))
             (move win 0 inptr)
             (refresh win)))
          (:backspace
           (when (> inptr 0)
             (decf inptr)
             (setf inbuf (remove-nth (- (length inbuf) 1 inptr) inbuf))
             (clear win)
             (add-string win (coerce (reverse inbuf) 'string))
             (move win 0 inptr)
             (refresh win)))
          (:ic
           (format t "(insert-mode-p win) => ~A~%" (insert-mode-p win))
           (setf (insert-mode-p win) (not (insert-mode-p win)))
           (format t "(insert-mode-p win) => ~A~%" (insert-mode-p win))
           (refresh wout))
          (otherwise
           (if (= inptr (length inbuf))
               (setf inbuf (cons (event-key event) inbuf))
               (if (insert-mode-p win)
                   (setf inbuf (insert-nth (- (length inbuf) inptr) (event-key event) inbuf))
                   (setf inbuf (replace-nth (- (length inbuf) (1+ inptr)) (event-key event) inbuf))))
           (incf inptr)
           (clear win)
           (add-string win (coerce (reverse inbuf) 'string))
           (move win 0 inptr)
           (refresh win)))))))

(defun t16e ()
  "Edit a single input field, not part of a form."
  (with-screen (scr :input-echoing nil :cursor-visible t :enable-colors t :enable-function-keys t :input-blocking t)
    (let ((*standard-output* scr)
          (s1 (list :attributes '(:underline)))
          (s2 (list :simple-char #\.))
          (field (make-instance 'field :position (list 3 20) :width 10 :window scr)))

      (setf (style field) (list :foreground s1 :background s2))

      (bind field #\newline 'de.anvi.croatoan::debug-print-field-buffer)

      ;; pressing ^A (for "accept") exits the edit mode (for now)
      ;; the field variables are not reset when edit is exited, we can access them after the edit.
      (if (edit field)
          ;; accept returns t
          (progn
            (clear scr)
            ;; display the contents of the input buffer of the field
            (format t "buffer: ~A~%" (buffer field))
            (format t "string: ~A" (value field)))
          ;; when cancel returns nil
          (progn
            (clear scr)
            (format t "field edit canceled.")))

      (refresh scr)

      ;; wait for keypress, then exit
      (get-char scr) )))

(defun t16e1 ()
  "Set the background color to the value read from a field.

Adding hex values requires 256color support in the terminal."
  (with-screen (scr :input-echoing nil :cursor-visible t :enable-colors t :enable-function-keys t :input-blocking t)
    (let ((s1 (list :attributes '(:underline)))
          (s2 (list :simple-char #\.))
          (field (make-instance 'field :position (list 3 20) :width 10 :window scr)))
      (setf (style field) (list :foreground s1 :background s2))
      ;; read a color (keyword or hex) from the field then set the background
      (bind field #\newline (lambda (w e)
                              (declare (ignore w e))
                              (setf (background scr) (make-instance 'complex-char :simple-char #\space :bgcolor (read-from-string (value field))))
                              (refresh scr)))
      (edit field))))

(defun t16e2 ()
  "Display a textarea (a multiline field) without a parent form.

Supported control keys: left, right, newline

Scrolling is enabled by default and the buffer is unlimited.

C-a (^A) accepts the entry and exits the edit loop returning the
contents of the area as a single string."
  (with-screen (scr :input-echoing nil :cursor-visible t :input-blocking t)
    (let* ((win  (make-instance 'window   :position '(5 5) :dimensions '(8 21) :border t :enable-function-keys t))
           (area (make-instance 'textarea :position '(1 1) :dimensions '(6 19) :window win)))
      (setf (background win) (make-instance 'complex-char :attributes '(:reverse)))
      (refresh win)
      ;; add some optional initial content to the input buffer
      (setf (value area) "hello there")
      (edit area)
      (close win)
      ;; return the edited contents of the area as a string (including newlines)
      (value area))))

(defun t16e3 ()
  "Show a textarea, test how the window background char interacts with space characters."
  (with-screen (scr :input-echoing nil :cursor-visible t :input-blocking t)
    (let* ((win  (make-instance 'window   :position '(5 5)  :dimensions '(8 17) :border t :enable-function-keys t))
           (win1 (make-instance 'window   :position '(5 50) :dimensions '(8 17) :border t))
           (area (make-instance 'textarea :position '(1 1)  :dimensions '(6 15) :window win)))
      (setf (background win)  (make-instance 'complex-char :simple-char :board :fgcolor :yellow)
            (background win1) (make-instance 'complex-char :simple-char #\. :bgcolor :red ))

      ;; adding a simple space to a window with a background char only displays the background char.
      (add-wide-char win1 #\space                                   :y 1 :x 1 :n 10)
      ;; adding a complex character with either a foreground...
      (add-wide-char win1 #\space :fgcolor :yellow                  :y 2 :x 1 :n 10)
      ;; ...or background character actually overwrites the background char with a visible space char.
      (add-wide-char win1 #\space                  :bgcolor :yellow :y 3 :x 1 :n 10)

      (refresh win)
      (refresh win1)
      ;; add some optional initial content to the input buffer
      (setf (value area) "hello there")
      (edit area)
      (close win)
      (close win1)
      ;; return the edited contents of the area as a string (including newlines)
      (value area))))

(defun t16e4 ()
  "Initially, the buffer of the textarea is just a list of characters.

For smll buffer sizes, this is sufficient, but at some later point, it
will be more efficient to use a character array, a string."
  (with-screen (scr :input-echoing nil :cursor-visible t :enable-colors t :enable-function-keys t :input-blocking t)
    (let ((area (make-instance 'textarea :position '(5 5) :dimensions '(3 20) :window scr
                               :buffer (concatenate 'list
                                                    (coerce "Voy bien, Camilo?" 'list)
                                                    (list #\newline)
                                                    (coerce "Vas bien, Fidel!" 'list)
                                                    (list #\newline)
                                                    (coerce "Hello there" 'list)
                                                    (list #\newline)
                                                    (coerce "Dear john" 'list)
                                                    (list #\newline)
                                                    (coerce "Open the pod bay door." 'list)
                                                    (list #\newline)
                                                    (coerce "Will you stop, Dave?" 'list)))))
      (edit area)
      ;; Return the buffer contents as a string.
      ;; This is done more conveniently by the value accessor.
      (coerce (buffer area) 'string))))

(defun t16e5 ()
  "Textarea embedded in a form."
  (with-screen (scr :input-echoing nil :cursor-visible t :enable-colors t :enable-function-keys t :input-blocking t)
    (let* ((s1 (list :attributes '(:underline)))
           (s2 (list :simple-char #\.))
           (s3 (list :attributes '(:underline) :fgcolor :yellow))
           (s4 (list :simple-char #\. :fgcolor :red))
           (s5 (list :foreground s1 :background s2 :selected-foreground s3 :selected-background s4))
           (fs (list 'field s5 'textarea s5))
           (field1 (make-instance 'field  :position '(3 5)  :name :f1 :width 10))
           (area (make-instance 'textarea :position '(8 5)  :name :a1 :dimensions '(6 15)))
           (button (make-instance 'button :position '(16 5) :name :b1 :title "Accept" :callback 'accept))
           (form (make-instance 'form :elements (list field1 area button) :style fs :window scr)))
      (refresh scr)
      (if (prog1 (edit form) (clear scr))
          (loop for el in (elements form)
                when (activep el)
                  do (format scr "~A ~A~%" (name el) (value el)))
          (format scr "nil")))
    (refresh scr)
    (get-char scr)))

(defun t16f ()
  "Group several input fields and buttons to a form."
  (with-screen (scr :input-echoing nil :cursor-visible t :enable-colors t :enable-function-keys t :input-blocking t)
    (let* ((s1 (list :fgcolor nil :bgcolor nil :attributes nil))
           (s2 (list :simple-char #\_))
           (s3 (list :fgcolor :yellow :bgcolor :red :attributes '(:underline :bold :italic)))
           (s4 (list :fgcolor :blue :bgcolor :white :attributes nil))

           ;; a style is a plist interpreted by the element drawing functions.
           (s5 (list :foreground s1 :background s2 :selected-foreground s3 :selected-background s4))

           (field1 (make-instance 'field :name :f1 :position (list 3 20) :width 10 :style s5 :max-buffer-length 5))
           (field2 (make-instance 'field :name :f2 :position (list 5 20) :width 10 :style s5))
           (field3 (make-instance 'field :name :f3 :position (list 7 20) :width 10 :style s5 :max-buffer-length 15))

           (s6 (list :foreground s1 :selected-foreground s4))

           (button1 (make-instance 'button :position (list 10 10) :title "Hello"  :style s6))
           (button2 (make-instance 'button :position (list 10 20) :title "Accept" :style s6))
           (button3 (make-instance 'button :position (list 10 30) :title "Cancel" :style s6))

           ;; a window is associated with the parent form, and can be accessed by the elements.
           (form (make-instance 'form :elements (list field1 field2 field3 button1 button2 button3) :window scr)))

      ;; for debugging, return prints the content of the buffer and then deletes the buffer
      (bind form :f4 'de.anvi.croatoan::debug-print-field-buffer)
      (bind (find-keymap 'field-map) :f3 'de.anvi.croatoan::debug-print-field-buffer)

      ;; Functions to be called when the button is activated by #\newline or #\space.
      (setf (callback button1) (lambda (b e)
                                 (declare (ignore b e))
                                 (save-excursion scr (move scr 0 0) (format scr "Hello there"))))
      (setf (callback button2) 'accept)
      (setf (callback button3) 'cancel)

      ;; pressing ^A or C-a (for "accept") exits the edit mode
      ;; TAB, up and down cycles the fields and buttons
      (if (prog1 (edit form) (clear scr))
          ;; display the contents of the input buffer of all fields of the form
          ;; use field-buffer-to-string to get the contents of the field buffer as a string instead of a list of chars.
          (loop for el in (elements form)
             do (when (typep el 'field)
                  (format scr "~A ~A ~A ~%" (name el) (buffer el) (value el))))
          ;; user did not accept the form (default) or called cancel-form.
          (format scr "nil"))

      (refresh scr)
      ;; wait for keypress, then exit
      (get-char scr) )))

(defun t16g ()
  "Use the element default style of the form."
  (with-screen (scr :input-echoing nil :cursor-visible t :enable-colors t :enable-function-keys t :input-blocking t)
    (let* (;; character styles that can be referenced in element styles
           (ch1 (list :fgcolor :blue :bgcolor :black))
           (ch2 (list :simple-char #\_))
           (ch3 (list :fgcolor :yellow :bgcolor :red :attributes '(:bold :italic)))
           (ch4 (list :simple-char #\_ :fgcolor :white :bgcolor :blue))
           (ch5 (list :fgcolor :yellow :simple-char #\.))

           ;; element styles reference previousy defined character styles.
           (s1 (list :foreground ch1 :background ch2 :selected-foreground ch3 :selected-background ch4))
           (s2 (list :foreground ch4 :selected-foreground ch3))
           (s3 (list :foreground ch1 :background ch5))

           ;; the form style consists of default styles of form elements.
           (sf1 (list 'field s1 'button s2 'label s3 'checkbox s1))

           (field1 (make-instance 'field :name :f1 :title "Forename" :position (list 3 20) :width 15 :max-buffer-length 5))
           (field2 (make-instance 'field :name :f2 :title "Surname"  :position (list 5 20) :width 15))
           (field3 (make-instance 'field :name :f3                   :position (list 7 20) :width 15 :max-buffer-length 20))

           (cb1 (make-instance 'checkbox :name :c1 :title "Employed" :position (list 9 20)))

           (m1-style (list :foreground (list :fgcolor :blue)
                           :selected-foreground (list :fgcolor :yellow :attributes (list :bold))))

           (m1 (make-instance 'checklist :name :m1 :title "checklist" :items (list 'a 'b 'c 'd 'e 'f) :layout (list 2 3)
                              :max-item-length 6 :position (list 11 20) :style m1-style))

           (label1 (make-instance 'label :name :l1 :reference :f1 :width 18 :position (list 3 1)))
           (label2 (make-instance 'label :name :l2 :reference :f2 :width 18 :position (list 5 1)))
           (label3 (make-instance 'label :name :l3 :title "Age"             :position (list 7 1)))
           (label4 (make-instance 'label :name :l4 :reference :c1 :width 18 :position (list 9 1)))
           (label5 (make-instance 'label :name :l5 :reference :m1 :width 18 :position (list 11 1)))

           (button1 (make-instance 'button :name :b1 :title "Hello"  :position (list 14 10)))
           (button2 (make-instance 'button :name :b2 :title "Accept" :position (list 14 20)))
           (button3 (make-instance 'button :name :b3 :title "Cancel" :position (list 14 30)))

           (form (make-instance 'form
                                :elements (list field1 field2 field3 cb1 label1 label2 label3 label4 label5
                                                m1 button1 button2 button3)
                                :style sf1 :window scr)))

      ;; for debugging, return prints the content of the buffer and then deletes the buffer
      (bind form :f4 'crt::debug-print-field-buffer)

      ;; remove q and newline from the menu map because enter exits the main menu loop
      (unbind (find-keymap 'menu-map) #\q)
      (unbind (find-keymap 'menu-map) #\newline)

      ;; Functions to be called when the button is activated by #\newline or #\space.
      (setf (callback button1) (lambda (b e)
                                 (declare (ignore b e))
                                 (save-excursion scr (move scr 0 0) (format scr "Hello there"))))
      (setf (callback button2) 'accept)
      (setf (callback button3) 'cancel)

      ;; The set value shouldnt be longer than the max-buffer-length
      (setf (value field2) "hello"
            (value field3) "dear john")

      ;; Access fields by their name instead of looping through the elements list.
      (let ((val (edit form)))
        (clear scr)
        (format scr "~S~%" val)
        (if val
            ;; edit returned t, which means the user accepted the form
            (progn
              (mapc #'(lambda (name)
                        (let ((element (find-element form name)))
                          (format scr "~5A ~10A ~20A~%" name (title element) (value element))))
                    (list :f1 :f2 :f3))
              ;; display the state of the checkbox
              (format scr "~5A ~10A ~20A~%" (name cb1) (title cb1) (checkedp cb1))
              (format scr "~5A ~10A ~20A~%" (name m1) (title m1) (value m1)))
            ;; edit returned nil, which means the user canceled the form
            (format scr "nil")))
      (refresh scr)
      ;; wait for keypress, then exit
      (get-char scr) )))

(defun t16h ()
  "Create a form window."
  (with-screen (scr :input-echoing nil :cursor-visible t :enable-colors t :enable-function-keys t :input-blocking t)
    (let* ((ch1 (list :fgcolor :black :bgcolor :white))
           (ch2 (list :simple-char #\_ :fgcolor :black :bgcolor :white))
           (ch3 (list :fgcolor :yellow :bgcolor :red :attributes '(:underline :bold :italic)))
           (ch4 (list :simple-char #\_ :fgcolor :white :bgcolor :blue))

           (style1 (list :foreground ch1 :background ch2 :selected-foreground ch3 :selected-background ch4))
           (style2 (list :foreground ch4 :selected-foreground ch3))
           (style3 (list 'field style1 'button style2))

           (field1 (make-instance 'field :name :f1 :title "Forename" :position (list 3 3) :width 20 :max-buffer-length 5))
           (field2 (make-instance 'field :name :f2 :title "Surname"  :position (list 5 3) :width 20))
           (field3 (make-instance 'field :name :f3 :title "Age"      :position (list 7 3) :width 20 :max-buffer-length 20))

           (button1 (make-instance 'button :name :b1 :title "Say Hello" :position (list 10 7)))
           (button2 (make-instance 'button :name :b1 :title "Cancel"    :position (list 10 20)))
           (button3 (make-instance 'button :name :b2 :title "Accept"    :position (list 10 30)))

           (form (make-instance 'form-window :elements (list field1 field2 field3 button1 button2 button3)
                                :style style3 :enable-function-keys t :input-blocking t :title "form window"
                                :border t :height 15 :width 50 :position (list 5 15))))

      (setf (background scr) (make-instance 'complex-char :simple-char #\space))
      (setf (background form) (make-instance 'complex-char :simple-char #\space :color-pair '(:black :white)))
      (refresh scr)

      ;; Functions to be called when the button is activated by #\newline or #\space.
      (setf (callback button1) (lambda (b e)
                                 (declare (ignore b e))
                                 (save-excursion scr (move scr 0 0) (format scr "Hello there") (refresh scr))))
      (setf (callback button2) 'cancel)
      (setf (callback button3) 'accept)

      ;; The set value shouldnt be longer than the max-buffer-length (not checked yet)
      (setf (value field1) "hello"
            (value field2) "there"
            (value field3) "dear john")

      (event-case (scr event)
        (#\q (return-from event-case))
        ;; display the form window input box by pressing a.
        (#\a
         (if (edit form)
             ;; edit returns t if the form is accepted
             (progn
               (clear scr)
               ;; Access fields by their name instead of looping through the elements list.
               (mapc #'(lambda (name)
                         (let ((field (find-element form name)))
                           (format scr "~5A ~10A ~20A~%" name (title field) (value field))))
                     (list :f1 :f2 :f3))
               (refresh scr))

             ;; and nil if it is cancelled.
             (progn
               (clear scr)
               (princ nil scr)
               (refresh scr)))))

        ;; close the ncurses windows before exiting.
        (close form))))

(defun t16i ()
  "A simple input form window."
  (let ((value nil))
    (with-screen (scr :input-echoing nil :cursor-visible t :enable-colors t :enable-function-keys t :input-blocking t)
      (let* ((ch1 (list :fgcolor :black :bgcolor :white))
             (ch2 (list :simple-char #\_ :fgcolor :black :bgcolor :white))
             (ch3 (list :fgcolor :yellow :bgcolor :red :attributes '(:underline :bold :italic)))
             (ch4 (list :simple-char #\_ :fgcolor :white :bgcolor :blue))
             (ch5 (list :simple-char #\. :fgcolor :black :bgcolor :white))

             (style1 (list :foreground ch1 :background ch2 :selected-foreground ch3 :selected-background ch4))
             (style2 (list :foreground ch1 :selected-foreground ch4))
             (style3 (list :foreground ch1 :background ch5))

             ;; form element default styles
             (style4 (list 'field style1 'button style2 'label style3))

             (label1 (make-instance 'label :name :l1 :reference :f1 :width 20 :position (list 1 1)))
             (field1 (make-instance 'field :name :f1 :title "Forename" :position (list 1 22) :width 20 :max-buffer-length 25))

             (label2 (make-instance 'label :name :l2 :reference :f2 :width 20 :position (list 3 1)))
             (field2 (make-instance 'field :name :f2 :title "Surname" :position (list 3 22) :width 20 :max-buffer-length 25))

             (button1 (make-instance 'button :name :b3 :title "Reset"  :position (list 6 15)))
             (button2 (make-instance 'button :name :b2 :title "Cancel" :position (list 6 25)))
             (button3 (make-instance 'button :name :b2 :title "Accept" :position (list 6 34)))

             (form (make-instance 'form-window :elements (list field1 field2 label1 label2 button1 button2 button3)
                                  :style style4 :enable-function-keys t :input-blocking t :name :fw1 :title "form window"
                                  :border t :height 9 :width 45 :position (list 5 15))))

        (setf (background form) (make-instance 'complex-char :simple-char #\space :color-pair '(:black :white)))
        (refresh scr)

        (setf (callback button1) 'reset-form)
        (setf (callback button2) 'cancel)
        (setf (callback button3) 'accept)

        (if (edit form)
            ;; if accepted, create an alist as the value to be returned by t16i
            (setq value (pairlis (list (title field1) (title field2))
                                 (list (value field1) (value field2))))
            (setq value nil))
        (close form)))

    ;; return an alist (input values) or nil.
    value))

(defun t16j ()
  "Perform long-running calculations in worker threads to prevent freezing the ncurses UI."
  (with-screen (scr :input-echoing nil :cursor-visible t :enable-colors t :enable-function-keys t :input-blocking 200)
    (let* ((field1 (make-instance 'field :name :f1 :title "Forename" :position (list 3 20) :width 15))
           (field2 (make-instance 'field :name :f2 :title "Surname"  :position (list 5 20) :width 15))
           (field3 (make-instance 'field :name :f3 :title "Nickname" :position (list 7 20) :width 15))
           (l1 (make-instance 'label :name :l1 :reference :f1 :width 18 :position (list 3 1)))
           (l2 (make-instance 'label :name :l2 :reference :f2 :width 18 :position (list 5 1)))
           (l3 (make-instance 'label :name :l3 :reference :f3 :width 18 :position (list 7 1)))
           (b1 (make-instance 'button :name :b1 :title "Calc main"   :position (list 14 1)))
           (b2 (make-instance 'button :name :b2 :title "Calc worker" :position (list 14 14)))
           (b3 (make-instance 'button :name :b3 :title "Cancel" :position (list 14 29)))
           (b4 (make-instance 'button :name :b4 :title "Accept" :position (list 14 39)))
           ;; a thread-safe queue
           (queue (make-instance 'queue))
           (form (make-instance 'form :window scr
                                :elements (list field1 field2 field3 l1 l2 l3 b1 b2 b3 b4) )))

      ;; a long-running function running in the main thread freezes the ncurses UI.
      (setf (callback b1) (lambda (b e)
                            (declare (ignore b e))
                            (sleep 10)
                            (setf (value field3) (concatenate 'string (value field1) " " (value field2)))
                            (draw form)))

      ;; to avoid freezing non-thread-safe ncurses, move the calculation to a worker thread,
      ;; then return the result through a thread-safe queue
      ;; poll the queue within the nil event and handle the result returned by the worker.
      (setf (callback b2) (lambda (b e)
                            (declare (ignore b e))
                            (setf (value field3) "Calculating...")
                            (draw form)
                            (let ((v1 (value field1))
                                  (v2 (value field2)))
                              (bt:make-thread (lambda ()
                                                ;; simulate a long-running calculation running in a worker thread
                                                (sleep 10)
                                                (enqueue (concatenate 'string v1 " " v2) queue))))))

      ;; cancel the form, which makes the edit function return nil
      (setf (callback b3) 'cancel)
      ;; accept the form, which makes the edit function return t
      (setf (callback b4) 'accept)

      ;; initial value of the fields 1 & 2.
      ;; the value of f3 will be calculated from 1 and 2.
      (setf (value field1) "hello"
            (value field2) "there")

      ;; how to set a timer that polls the queue also when blocking is t? (tkinter has the after function)
      (bind form nil (lambda (f e)
                       (declare (ignore f e))
                       ;; pop an item off the queue and set it as the result of the worker thread calculation to field3.
                       ;; we do not need the loop here since we only have one item in the queue,
                       ;; but in general the polling of a longer queue should be done by a loop.
                       (loop
                          for i = (dequeue queue)
                          while i do
                            (setf (value field3) i)
                            (draw form))))
      (if (edit form)
          ;; edit returned t, which means the user accepted the form
          (progn
            (clear scr)
            (format scr "Form accepted. edit returns t.~%~%")
            (mapc #'(lambda (name)
                      (let ((element (find-element form name)))
                        (format scr "~5A ~10A ~20A~%" name (title element) (value element))))
                  (list :f1 :f2 :f3)))
          ;; edit returned nil, which means the user canceled the form
          (progn
            (clear scr)
            (format scr "Form canceled. edit returns nil.")))

      (refresh scr)
      ;; temporarily set blocking to t, then wait for a keypress
      (wait-for-event scr) )))

(defun t16j1 ()
  "Test different callbacks to exit the event loop and return values from a form."
  (with-screen (scr :input-echoing nil :cursor-visible t :enable-colors t :enable-function-keys t :input-blocking t)
    (let* ((field1 (make-instance 'field  :name :f1 :title "Forename" :position (list 3 20) :width 15))
           (field2 (make-instance 'field  :name :f2 :title "Surname"  :position (list 5 20) :width 15))
           (field3 (make-instance 'field  :name :f3 :title "Nickname" :position (list 7 20) :width 15))
           (l1     (make-instance 'label  :name :l1 :reference :f1    :position (list 3 1)  :width 18))
           (l2     (make-instance 'label  :name :l2 :reference :f2    :position (list 5 1)  :width 18))
           (l3     (make-instance 'label  :name :l3 :reference :f3    :position (list 7 1)  :width 18))
           (b1     (make-instance 'button :name :b1 :title "Button1"  :position (list  9  1) :callback 'return-element-value :value 1))
           (b2     (make-instance 'button :name :b2 :title "Button2"  :position (list  9 20) :callback 'return-form-values   :value 2))
           (b3     (make-instance 'button :name :b3 :title "Cancel"   :position (list 11  1) :callback 'cancel))
           (b4     (make-instance 'button :name :b4 :title "Accept"   :position (list 11 20) :callback 'accept))
           (form   (make-instance 'form :window scr :elements (list field1 field2 field3 l1 l2 l3 b1 b2 b3 b4)
                                        :style '(field (:background (:attributes (:reverse))
                                                        :selected-background (:bgcolor :blue))
                                                 button (:foreground (:attributes (:reverse))
                                                         :selected-foreground (:bgcolor :blue))))))
      (format scr "Edit returned:~%~S" (prog1 (multiple-value-list (edit form)) (clear scr)))
      (refresh scr)
      (wait-for-event scr))))

(defun t16j2 ()
  "Pass elements to a form within a grid layout to automatically calculate their positions."
  (with-screen (scr :input-echoing nil :cursor-visible t :enable-colors t :enable-function-keys t :input-blocking t)
    (let* ((buttons (loop for i from 0 to 9 collect
                      (make-instance 'button :title (format nil "~R" (random 200)) :callback 'accept)))
           (field (make-instance 'field :name :f1 :width 25))
           (area (make-instance 'textarea :name :a1 :dimensions '(5 20)))
           (menu1 (make-instance 'menu :name :m1 :items '(a b c d e f g h) :layout '(4 2) :max-item-length 3))
           ;; show 6 labels in a grid nested within the main grid.
           (labels1
             ;; The padding is the number of spaces added to each element, (top bottom left right).
             (make-instance 'layout :grid-height 3 :grid-width 2 :padding '(0 1 0 1) :elements
               (loop for i from 0 to 5 collect
                 (make-instance 'label :active nil :title (format nil "hello ~r" (random 20))))))
           (form (make-instance 'form
                                :window scr
                                ;; instead of passing the elements in a element list, pass them within a layout object.
                                ;; this will automatically calculate their positions during form initialization.
                                ;; "nil" elements are represented as empty cells in the grid.
                                :layout (make-instance 'layout :grid-height 6 :grid-width 3
                                                               :padding '(0 1 0 1) :position '(2 4)
                                                               :elements (append (list menu1 nil) buttons (list area nil labels1 nil field)))
                                :style '(field (:background (:attributes (:reverse))
                                                :selected-background (:bgcolor :blue))
                                         textarea (:background (:attributes (:reverse))
                                                   :selected-background (:bgcolor :blue))
                                         label (:foreground (:fgcolor :red :bgcolor :yellow))
                                         button (:foreground (:attributes (:reverse))
                                                 :selected-foreground (:bgcolor :blue))))))
      (edit form))))

(defparameter *t16k-message*
  "This is my textarea. There are many like it, but this one is mine. My textarea is my best
friend. It is my life. I must master it as I must master my life.")

(defun t16k ()
  "An message-dialog featuring a inactive textarea and an OK button."
  (with-screen (scr :input-echoing nil :cursor-visible t :enable-colors t :enable-function-keys t :input-blocking t)
    (let* ((ch1 (list :fgcolor :black :bgcolor :white))
           (ch3 (list :fgcolor :white :bgcolor :blue :attributes '(:bold)))
           (style1 (list :foreground ch1 :background ch1 :selected-foreground ch1 :selected-background ch1))
           (style2 (list :foreground ch1 :selected-foreground ch3))
           (style4 (list 'textarea style1 'button style2))
           (area1 (make-instance 'textarea :position '(1 1) :dimensions '(4 50) :active nil))
           (area2 (make-instance 'textarea :position '(6 1) :dimensions '(4 50) :active nil))
           (button1 (make-instance 'button :name :b3 :title "  OK  "  :position (list 10 20)))
           (form (make-instance 'form-window
                                :elements (list area1 area2 button1)
                                :style style4 :enable-function-keys t :input-blocking t
                                :title "Form window"
                                :border t :height 13 :width 54
                                :position (list 1 1))))
      ;; first area without string wrapping
      (setf (value area1) *t16k-message*
            ;; second area with string wrapping
            (value area2) (wrap-string *t16k-message* 49))
      (setf (crt::activep area1) nil)
      (setf (background form) (make-instance 'complex-char :simple-char #\space :color-pair '(:black :white)))
      (refresh scr)
      (setf (callback button1) 'accept)
      (edit form)
      (close form))))

(defparameter *t16k1-style*
  ;; default element styles, since the msgbox is a form
  '(button   (:selected-foreground (:fgcolor :white :bgcolor :blue))
    textarea (:foreground (:fgcolor :blue :bgcolor :white)
              :background (:fgcolor :blue :bgcolor :green :simple-char #\.))
    ;; but since it is also a form-window, it has additional properties that can be styled
    :title (:fgcolor :black :bgcolor :yellow :attributes (:bold))
    :foreground (:fgcolor :red :bgcolor :yellow)
    :background (:fgcolor :white :bgcolor :black :simple-char #\*)))

(defun t16k1 ()
  "Use the msgbox class directly."
  (with-screen (scr :input-echoing nil :cursor-visible t :enable-colors t :enable-function-keys t :input-blocking t)
    (let ((msgbox (make-instance 'msgbox :title "This is a msgbox dialog" :message *t16k-message*
                                         :center t :height 10 :width 50 :style *t16k1-style*)))
      ;; chars will be added in the fg style of the form window
      (add-char (sub-window msgbox) #\x :y 0 :x 1)
      (add-char msgbox #\x :y 4 :x 0)

      (setf (cursor-visible-p scr) nil)
      (edit msgbox)
      (setf (cursor-visible-p scr) t)
      (close msgbox))))

;; creating sub-windows and how they share memory with the parent window.
;; leaving out the size of a window maxes it out to the right (win1) and to the bottom (win1, win3)
(defun t17 ()
  "Show how to creating sub-windows and how they share memory with the parent window."
  (with-screen (scr :input-echoing nil :input-blocking t :cursor-visible nil :enable-colors t)
    ;; Leaving out the size of a window maxes it out to the right (win1) and to the bottom (win1, win3).
    (let* ((win1 (make-instance 'window :position '(2 2) :border t))
           (win2 (make-instance 'sub-window :parent win1 :dimensions '(5 20) :position '(4 4) :border t))
           (win3 (make-instance 'sub-window :parent win1           :width 20 :position '(4 4) :border t :relative t)))
      (princ "win1" win1)
      (princ "win2" win2)
      (princ "win3 relative" win3)
      (move scr 0 0) (format scr "Dimensions: scr ~A win1 ~A win2 ~A win3 ~A"
                             (dimensions scr) (dimensions win1) (dimensions win2) (dimensions win3))
      (move scr 1 0) (format scr "Positions:  scr ~A win1 ~A win2 ~A win3 ~A"
                             (widget-position scr) (widget-position win1) (widget-position win2) (widget-position win3))
      (mapc #'refresh (list scr win1 win2 win3))
      (get-char win3)
      (mapc #'close (list win2 win3))
      (refresh win1)
      ;; observe that the content from win 2 and 3 is still in win1 after they have been closed.
      (get-char win1)
      (close win1) )))

;; by default, the sub-window displays the part of the parent window it overlaps with.
;; we can change which part of the parent is displayed by changing the sub-windows source-position
;; we can change where it is displayed by changing the sub-windows position.
(defun t17a ()
  (with-screen (scr :input-echoing nil :input-blocking t :cursor-visible nil :enable-colors t)
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
      (setf (source-position win) '(2 25)) ; (%mvderwin (winptr win) 2 25)
      (mapc #'(lambda (w) (touch w) (refresh w)) (list scr win))
      (get-char scr)

      ;; map area2 to subwin position
      (setf (source-position win) '(2 50))
      (mapc #'(lambda (w) (touch w) (refresh w)) (list scr win))
      (get-char scr)

      ;;; now move subwin position
      ;; it still maps area2, but now to the new position.
      ;; the original content written to the subwin (and thus to scr because they share memory)
      ;; is now visible in scr, since the subwin overlay has moved.
      (setf (widget-position win) '(10 2))
      (mapc #'(lambda (w) (touch w) (refresh w)) (list scr win))
      (get-char scr)

      ;; map area1 again, but now to the new position.
      (setf (source-position win) '(2 25))
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
;; https://github.com/rudolfochrist/dotfiles/blob/master/.rc.lisp
(defun t18 ()
  (with-screen (scr)
    (format scr "Lisp implementation type:    ~A~%" (lisp-implementation-type))
    (format scr "Lisp implementation version: ~A~%" (lisp-implementation-version))
    (format scr "Machine type (Arch)          ~A~%" (machine-type))
    (format scr "Machine version (CPU)        ~A~%" (machine-version))
    (format scr "Software type (OS)           ~A~%" (software-type))
    (format scr "Software version (OS)        ~A~%" (software-version))
    (format scr "Machine instance (hostname)  ~A~%" (machine-instance))
    (format scr "User HOME                    ~A~%" (user-homedir-pathname))
    (format scr "Ncurses version:             ~A~%" (ncurses:curses-version))
    (format scr "Ncurses name:                ~A~%" ncurses:*library-name*)
    (format scr "Ncurses file name:           ~A~%" ncurses:*library-file-name*)
    (format scr "Terminal:                    ~A~%" (ncurses:termname))
    (format scr "Colors supported:            ~A~%" (ncurses:has-colors))
    (format scr "Color change supported:      ~A~%" (ncurses:can-change-color))
    (format scr "No of supported colors:      ~A~%" ncurses:colors)
    (format scr "No of supported color pairs  ~A~%" ncurses:color-pairs)
    (format scr "Can insert/delete chars:     ~A~%" (ncurses:has-ic))
    (format scr "Can insert/delete lines:     ~A~%" (ncurses:has-il))
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
           (*error-output* *standard-output*)
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
  (with-screen (scr :input-echoing nil :input-blocking t :cursor-visible nil :enable-colors t)
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
  (with-screen (scr :input-echoing nil :input-blocking t :cursor-visible nil :enable-colors t)
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

(defun t19b ()
  "Use the menu class, draw-menu and select functions."
  (with-screen (scr :input-echoing nil :input-blocking t :cursor-visible nil :enable-colors t)
    (let* ((choices '("Choice 0" "Choice 11" "Choice 222" "Choice 3333" "Choice 44444" "Choice 555555" "Choice 6666666"))
           (menu (make-instance 'menu-window :items choices :position (list 0 20) :title "t19b"
                                             :cyclic t :border t :enable-function-keys t)))
      (let ((result (select menu)))
        (format scr "You chose ~A" result)
        (touch scr)
        (refresh scr)
        (get-char scr))
      (close menu))))

(defun t19b1 ()
  "Use a fancy styled menu-panel to select an item."
  (with-screen (scr :input-echoing nil :input-blocking t :cursor-visible nil :enable-colors t :stacked t)
    (let* ((choices '("Choice 0" "Choice 11" "Choice 222" "Choice 3333" "Choice 44444" "Choice 555555" "Choice 6666666"))
           (sub-menu (make-instance 'menu-panel :items choices :position (list 5 27) :enable-function-keys t :border t :shadow t :title "sub-menu"))
           (menu (make-instance 'menu-panel :items (cons sub-menu choices)
                                            :position (list 5 10)
                                            :title "t19b1"
                                            :cyclic t
                                            :border t
                                            :border-width 2
                                            :shadow t
                                            :enable-function-keys t)))
      (setf (background scr) (make-instance 'complex-char :simple-char :board :fgcolor :cyan))
      (refresh scr)
      (setf (style menu)
            '(:border (:background  (:fgcolor :white :bgcolor :blue))
              :shadow (:background  (:fgcolor :black :bgcolor :black))
              :selected-foreground (:attributes (:reverse))
              :background (:fgcolor :black :bgcolor :cyan)
              :selected-background (:attributes (:reverse))))
      (let ((result (select menu)))
        (format scr "You chose ~A" result)
        (touch scr)
        (refresh scr)
        (get-char scr))
      (close menu))))

(defun t19b2 ()
  "Display a menu associated with a separate window. Display submenus ranger-style to the right of the parent."
  (with-screen (scr :input-echoing nil :input-blocking t :cursor-visible nil :enable-colors t)
    (let* ((items3 '("Choice 0" "Choice 11" :choice22 "Choice 3333" "Choice 44444" "Choice 555555" "Choice 6666666" "Choice 7" "Choice 88"))
           (menu3 (make-instance 'menu :items items3 :name :sm3 :title "sub-menu 3" :max-item-length 20 :position (list 7 50) :menu-type :checklist))
           (items2 (list "Item 0" menu3 "Item 1" "Item 2" "Item 3" "Item 4" "Item 5" "Item 6" "Item 7" "Item 8"))
           (menu2 (make-instance 'menu :items items2 :name :sm2 :title "sub-menu 2" :max-item-length 20 :position (list 6 30)))
           (items1 (list "Item 00" menu2 "Item 01" "Item 02" "Item 03" "Item 04" "Item 05" "Item 06" "Item 07" "Item 08" "Item 09" "Item 10"))
           (menu1 (make-instance 'menu :items items1 :name :t19b2 :title "t19b2" :max-item-length 20 :position (list 5 10))))

      ;; associate the same window with all three menus.
      (setf (window menu1) scr
            (window menu2) scr
            (window menu3) scr)

      ;; select an item and return it.
      (select menu1))))

(defun t19b3 ()
  "Display submenus in-place, on top of the parent menu."
  (with-screen (scr :input-echoing nil :input-blocking t :cursor-visible nil :enable-colors t)
    (let* ((items3 '("Choice 0" "Choice 11" :choice22 "Choice 3333" "Choice 44444" "Choice 555555" "Choice 6666666" "Choice 7" "Choice 88" "Choice 999" ))
           (menu3 (make-instance 'menu :items items3 :name :sm1 :title "sub-menu 1" :max-item-length 20 :position (list 5 10) :menu-type :checklist :layout '(5 2)))
           (items2 (list "Item 0" menu3 "Item 1" "Item 2" "Item 3" "Item 4" "Item 5" "Item 6" "Item 7" "Item 8"))
           (menu2 (make-instance 'menu :items items2 :name :sm2 :title "sub-menu 2" :max-item-length 20 :position (list 5 10)))
           (items1 (list "Item 00" menu2 "Item 01" "Item 02" "Item 03" "Item 04" "Item 05" "Item 06" "Item 07" "Item 08" "Item 09" "Item 10"))
           (menu1 (make-instance 'menu :items items1 :name :t19b2 :title "t19b2" :max-item-length 20 :position (list 5 10) :layout '(4 3))))

      ;; associate the same underlying window with all three menus.
      (setf (window menu1) scr
            (window menu2) scr
            (window menu3) scr)

      ;; in order to display a submenu in place of the parent, the parent has to be cleared first.
      ;; clear after a submenu is selected (in menu.lisp accept-selection)
      (hook menu1 'before-submenu-hook (lambda (obj) (clear obj)))
      (hook menu2 'before-submenu-hook (lambda (obj) (clear obj)))

      ;; select an item and return it.
      (select menu1))))

(defun t19c ()
  "Improved t19b, the menu can be called repeatedly with the key a."
  (with-screen (scr :input-echoing nil :input-blocking t :cursor-visible nil :enable-colors t :stacked t)
    (let* ((choices '("Choice 0" "Choice 11" "Choice 222" "Choice 3333" "Choice 44444" "Choice 555555"
                      "Choice 6666666" "Choice 7" "Choice 88" "Choice 999"))
           ;; a menu style is a plist describing four different menu item components.
           ;; every style component is again a plist similar to a complex-char.
           (menu-style (list :foreground (list :fgcolor :yellow)
                             :background (list :fgcolor :yellow)
                             :selected-foreground (list :attributes (list :reverse))
                             :selected-background (list :attributes (list :reverse))))
           (menu (make-instance 'menu-window :items choices :position (list 0 25) :scrolled-layout (list 6 1)
                                             :title "t19c" :border t :enable-function-keys t :enable-scrolling t
                                             :style menu-style)))
      (event-case (scr event)
        ;; "a" draws the menu and enters a new menu-only event loop
        (#\a (let ((result (select menu)))
               (format scr "You chose ~A~%" result)))
               ;; we have to touch scr in order to make the menu disappear.
               ;;(touch scr)
               ;;(refresh scr)
               ;; the same is accomplished by setting :stacked t to the underlying screen/windows.
               ;; because the main stack is refreshed by select before the menu stack
        (#\q (return-from event-case)))
      (close menu))))

(defun t19c2 ()
  "Test the menu-item class for submenus."
  (with-screen (scr :input-echoing nil :input-blocking t :cursor-visible nil :enable-colors t :stacked t)
    (let* ((fun1 (make-instance 'menu-item :name :func1 :title t :value (lambda () (clear scr))))
           (choices (list "Choice 0" 'choice11 fun1 "Choice 222" "Choice 3333" "Choice 44444" 5
                          "Choice 6666666" "Choice 7" "Choice 88" "Choice 999"))
           ;; First, create a menu
           (sub-menu2 (make-instance 'menu-window
                                     :items choices ;; here we only have strings
                                     :position (list 2 57) :scrolled-layout (list 6 1) :enable-scrolling t
                                     ;; for hex triplets to work, we need to start sbcl with:TERM=xterm-256color lisp.sh
                                     ;;:color-pair (list :black #x666666)
                                     :bgcolor :red
                                     :name :sub2-name :title t :border t :enable-function-keys t))
           ;; then add that sub-menu menu as an item to the next menu, and so on.
           (sub-menu1 (make-instance 'menu-window
                                     :items (cons sub-menu2 choices) ;; first item is a submenu
                                     :position (list 1 41) :scrolled-layout (list 6 1) :enable-scrolling t
                                     ;;:color-pair (list :black #x999999)
                                     :fgcolor :green
                                     :name :sub1 :title "Sub1 title" :border t :enable-function-keys t))
           ;; finally, create the main menu containing sub-menu1 as an item
           (menu      (make-instance 'menu-window
                                     :items (cons sub-menu1 choices)  ;; first item is a submenu
                                     :position (list 0 25) :enable-scrolling t
                                     ;; the visible grid region replaces :scrolled-layout (list 6 1)
                                     :region-height 6 :region-width 1
                                     ;;:color-pair (list :black #xcccccc)
                                     :fgcolor :blue :bgcolor :yellow :border nil
                                     :name :menu :border nil :enable-function-keys t)))
      (setf (background scr) (make-instance 'complex-char :simple-char :board :color-pair (list :black :white)))
      (refresh scr)

      (event-case (scr event)
        ;; "a" draws the menu and enters a new menu-only event loop by calling select
        (#\a (let ((result (select menu)))
               (format scr "You chose ~A~%" result)
               (format scr "~A~%" (color-pair menu))
               (format scr "Stack ~A~%" (length (items *main-stack*)))))
        ;; this only works if we set :border t during menu init
        (#\b (setf (color-pair sub-menu2) (list :red :black))
             (box sub-menu2))
        ;; "q" exits the function and all menus and submenus.
        (#\q (return-from event-case)))
      (close menu)
      (close sub-menu1)
      (close sub-menu2))))

(defun t19c3 ()
  "Menu with checkbox items."
  (with-screen (scr :input-echoing nil :input-blocking t :cursor-visible nil :enable-colors t)
    (let* ((choices '("Choice 0" "Choice 11" "Choice 222" "Choice 3333" "Choice 44444" "Choice 555555"
                      "Choice 6666666" "Choice 7" "Choice 88" "Choice 999"))
           (menu (make-instance 'menu-window :items choices :position (list 0 25)
                                :scrolled-layout (list 6 1) :enable-scrolling t
                                :title "t19c3" :border t :enable-function-keys t
                                :menu-type :checklist
                                :max-item-length 20
                                :color-pair (list :yellow :red) )))
      (event-case (scr event)
        ;; "a" draws the menu and enters a new menu-only event loop
        (#\a (let ((result (select menu)))
               (format scr "You chose ~A~%" (mapcar #'value result))
               ;; we have to touch scr in order to make the menu disappear, or add :stacked t to scr
               ;; the *main-stack* is called by select before the menu value is returned
               (touch scr)
               (refresh scr)))
        (#\q (return-from event-case)))
      (close menu))))

(defun t19d ()
  "Use the arrow keys to pick a value from an 2D array menu, given as a layout parameter."
  (with-screen (scr :input-echoing nil :input-blocking t :cursor-visible nil :enable-colors t)
    (let* ((items (loop for i below 200 collect (format nil "Item ~A" i)))
           (menu (make-instance 'menu-window
                                :items items :position (list 0 0) :layout (list 20 10)
                                :scrolled-layout (list 10 4) :enable-scrolling t
                                :cyclic nil :max-item-length 9 :title "t19d" :border t
                                :enable-function-keys t)))
      (event-case (scr event)
        ;; "a" draws the menu and enters a new menu-only event loop
        (#\a (let ((result (select menu)))
               (format scr "You chose ~A~%" result)
               ;; we have to touch scr in order to make the menu disappear.
               (touch scr)
               (refresh scr)))
        (#\q (return-from event-case)))
      (close menu))))

(defun t19e ()
  "A one-line menu without a title and border resembling a menu bar."
  (with-screen (scr :input-echoing nil :input-blocking t :cursor-visible nil :enable-colors t)
    (let* ((items '("Item 0" "Item 1" "Item 2" "Item 3" "Item 4" "Item 5" "Item 6" "Item 7" "Item 8" "Item 9"))
           (menu (make-instance 'menu-window :input-blocking t :items items :position (list 0 0)
                                :layout (list 1 (length items))
                                :scrolled-layout (list 1 6) :enable-scrolling t
                                ;;:color-pair (list :black :yellow)
                                :max-item-length 10 :width (width scr) :border t :enable-function-keys t)))
      ;; start the output below the menu
      (move scr 4 0)
      ;; exit the infinite loop by exiting the menu with q.
      (loop named menu-case
         do
           (let ((result (select menu)))
             (unless result (return-from menu-case))
             (format scr "You chose ~A~%" result)
             (refresh scr)))
      (close menu))))

(defun t19e2 ()
  "A menu bar and submenus."
  (with-screen (scr :input-echoing nil :input-blocking t :cursor-visible nil :enable-colors t)
    (let* ((items1 (list "Choice 0" "Choice 11" "Choice 222" "Choice 3333" "Choice 44444" "Choice 555555"
                         "Choice 6666666" "Choice 7" "Choice 88" "Choice 999"))
           (sub-menu1 (make-instance 'menu-window :items items1 :position (list 2 30) :scrolled-layout (list 6 1) :enable-scrolling t
                                     :name :sub1-name :title nil :border t :enable-function-keys t :visible nil :menu-type :selection))
           (sub-menu2 (make-instance 'menu-window :items items1 :position (list 2 45) :scrolled-layout (list 6 1) :enable-scrolling t
                                     :max-item-length 20 :name :sub2 :title "Sub2 title" :border t :enable-function-keys t :visible nil
                                     :menu-type :checklist))
           (fun1 (make-instance 'menu-item :name :fun1 :title "fun1" :value (lambda () (clear scr) (move scr 4 0))))
           (items2 (list "Item 0" fun1 sub-menu1 sub-menu2))
           (menu (make-instance 'menu-window :input-blocking t :items items2 :position (list 0 0) :layout (list 1 (length items2))
                                             :max-item-length 15 :width (width scr) :border t :enable-function-keys t)))
      ;; start the output at line 4, below the menu bar.
      (move scr 4 0)
      (refresh scr)
      ;; add the screen to the main window stack to ensure that it is updated when the menu is exited.
      (setf (stackedp scr) t)
      (loop named menu-case
         do (let ((result (select menu)))
              (unless result (return-from menu-case))
              (format scr "You chose ~A~%" result)
              (refresh scr)))
      (mapc #'close (list menu sub-menu1 sub-menu2)))))

(defun t19f (&optional (border t))
  "A more fancy version of t19a, a yes-no dialog using the class dialog-window."
  (with-screen (scr :input-echoing nil :input-blocking t :cursor-visible nil :enable-colors t)
    (let* ((items (list "Yes" "No" "OK" 'cancel))
           (menu (make-instance 'dialog-window
                                :input-blocking t
                                :items items
                                ;; when center is t for a dialog window, we do not need to pass the position explicitely.
                                ;;:position (list 5 15)
                                :center t
                                :layout (list 1 4)
                                :max-item-length 12
                                :current-item-mark "> "
                                :color-pair (list :yellow :red)

                                ;; width and height will be automatically calculated if they are not given here
                                ;;:width 68
                                ;;:dimensions '(10 80)

                                :border border
                                :enable-function-keys t

                                :name :t19f
                                ;; if the title is given as a string, it overrides the default title = name
                                ;; :title "this is a selection dialog"
                                :title t

                                :message-height 2
                                :message-text "Press <- or -> to choose. Enter to confirm choice.~%Press q to exit.")))

      (setf (background scr) (make-instance 'complex-char :simple-char :board :color-pair (list :black :white)))

      (refresh scr)
      (loop named menu-case
         do (let ((result (select menu)))
              (unless result (return-from menu-case))
              (format scr "You chose ~A~%" result)
              (refresh scr)))
      (close menu))))

(defun t19g (&optional (border t))
  "A checklist dialog."
  (with-screen (scr :input-echoing nil :input-blocking t :cursor-visible nil :enable-colors t :use-terminal-colors t)
    (let* ((items (list "Yes" "No" "OK" 'cancel "Maybe" 'hello))
           (menu (make-instance 'dialog-window
                                :input-blocking t
                                :items items
                                ;; when a menu or dialog type is a checklist, items can be checked and unchecked with x/space.
                                :menu-type :checklist
                                ;; a dialog window can be automatically centered in the terminal window.
                                ;;:position (list 5 15)
                                :center t
                                ;; an error is signaled if h*w of the grid does not match the length of items
                                ;;:layout (list 2 3)
                                :color-pair (list :yellow :red)
                                ;; we do not need an item mark in a checklist
                                :current-item-mark ""

                                ;; the default dimensions of the dialog are calculated from the
                                ;; grid dimensions (layout) and the max-item-length
                                :max-item-length 60
                                ;;:dimensions '(20 80)
                                ;;:width 60

                                :border border
                                :enable-function-keys t
                                :title "this is a checkbox dialog"

                                :message-height 4
                                :message-text "Press <- or -> to choose. Enter to confirm choice. Press q to exit. Press <- or -> to choose. Enter to confirm choice. Press q to exit.")))
      ;; #x2592 = :board
      (setf (background scr) (make-instance 'complex-char :simple-char #x2592 :color-pair (list :white :black)))
      (refresh scr)
      (loop named menu-case
         do (let ((result (select menu)))
              ;; TODO: returning an empty list exits the loop.
              (unless result (return-from menu-case))
              (format scr "You chose ~A~%" (mapcar #'value result))
              (refresh scr)))
      (close menu))))

;; Passing the color attribute directly to a character.
(defun t20 ()
  "Display a randomly created carpet of the seven default ANSI colors, except for black."
  (with-screen (scr :input-echoing nil :input-blocking nil :enable-colors t :cursor-visible nil)
    (let ((colors '(:red :green :yellow :blue :magenta :cyan :white)))
      (event-case (scr event)
        (#\q (return-from event-case))
        ((nil)
         (sleep 0.01)
         (echo scr #\space
               :y (random (height scr))
               :x (random (width scr))
               :color-pair (list nil (nth (random 7) colors))))))))

(defun t20a ()
  "Display a randomly created carpet of the 16 default xterm colors, except for black.

This only works with TERM=xterm-256color in xterm and gnome-terminal."
  (with-screen (scr :input-echoing nil :input-blocking nil :enable-colors t :cursor-visible nil)
    (let ((colors '(       :maroon :green :olive  :navy :purple  :teal :silver
                    :gray  :red    :lime  :yellow :blue :magenta :cyan :white)))
      (bind scr #\q 'exit-event-loop)
      (bind scr nil
            (lambda (win e)
              (declare (ignore e))
              (echo win #\space
                    :y (random (height win))
                    :x (random (width win))
                    :bgcolor (nth (random 15) colors))))
      (setf (frame-rate scr) 1000)
      (run-event-loop scr))))

;; it is just a coincidence that "echo-char" works here.
;; echo-char is a chtype-function, and chtype allows only 1 byte for color pairs, which means 256 color pairs.
;; it works only because the foreground color is always black in the example.
;; echo-wide-char has to be used.
(defun t20b ()
  "Display the 256 supported colors. This only works with TERM=xterm-256color in xterm and gnome-terminal."
  (with-screen (scr :input-echoing nil :input-blocking t :enable-colors t :cursor-visible nil)
    ;; 0-15: 8 ANSI colors and 8 bold ANSI colors
    ;; only those first 16 colors are named.
    ;; note that the naming of the first 8 ANSI colors is not the same as the first 8 xterm colors.
    (loop for i from 0 to 7 do
         (loop for j from 0 to 1 do
              (loop for k from 0 to 2 do
                   (echo scr #\space
                         :y j
                         :x (+ (* i 3) k)
                         :color-pair (list :black
                                           (list :number (+ i (* j 8))))) )))
    ;; 16-231: 6x6x6 color cube
    (loop for n from 0 to 1 do
         (loop
            for m from 0 to 2
            for a1 = (+ 16 (* m 36))
            for a2 = (+ 16 (* m 36) 5)
            for a3 = (* m 19)
            do (loop for i from a1 to a2 do
                    (loop for j from 0 to 5 do
                         (loop for k from 0 to 2 do
                              (echo scr #\space
                                    :y (+ 3 j (* n 7))
                                    :x (+ (* (- i a1) 3) k a3)
                                    :color-pair (list :black
                                                      (list :number (+ (* n 108) i (* j 6)))) ))))))
    ;; 232-255: 24 shades of gray, without black and white
    (loop for i from 232 to 255 do
         (loop for k from 0 to 2 do
              (echo scr #\space
                    :y 17
                    :x (+ (* (- i 232) 3) k)
                    :color-pair (list :black
                                      (list :number i)))))
    (get-char scr)))

(defun t20c ()
  "Display the 256 supported colors. This only works with TERM=xterm-256color in xterm and gnome-terminal."
  (with-screen (scr :input-echoing nil :input-blocking t :enable-colors t :cursor-visible nil)
    ;; 0-15: 8 ANSI colors and 8 bold ANSI colors
    (loop for i from 0 to 7 do
         (loop for j from 0 to 1 do
              (loop for k from 0 to 2 do
                   (add scr #\space
                        :y j
                        :x (+ (* i 3) k)
                        :bgcolor (list :number (+ i (* j 8))))) ))
    ;; 16-231: 6x6x6 color cube
    (loop for n from 0 to 1 do
         (loop
            for m from 0 to 2
            for a1 = (+ 16 (* m 36))
            for a2 = (+ 16 (* m 36) 5)
            for a3 = (* m 19)
            do (loop for i from a1 to a2 do
                    (loop for j from 0 to 5 do
                         (loop for k from 0 to 2 do
                              (add scr #\space
                                   :y (+ 3 j (* n 7))
                                   :x (+ (* (- i a1) 3) k a3)
                                   :bgcolor (list :number (+ (* n 108) i (* j 6)))) )))))
    ;; 232-255: 24 shades of gray, without black and white
    (loop for i from 232 to 255 do
         (loop for k from 0 to 2 do
              (add scr #\space
                   :y 17
                   :x (+ (* (- i 232) 3) k)
                   :bgcolor (list :number i))))
    (refresh scr)
    (get-char scr)))

(defun t21 ()
  "Tests for insert-char, insert-string, extract-char."
  (with-screen (scr :cursor-visible nil)
    (move scr 0 0)
    (add-char scr #\a)

    ;; overwrite b over a
    (move scr 0 0)
    (add scr (make-instance 'complex-char :simple-char #\b
                            :attributes (list :bold :underline) :color-pair '(:blue :yellow)))

    ;; add e after b
    (echo scr (make-instance 'complex-char :simple-char #\e :attributes (list :underline)
                             :color-pair '(:blue :yellow)))

    ;; add pi after e
    (insert-wide-char scr 960 :color-pair '(:red :white))

    ;; insert pi before b
    (move scr 0 0)
    (insert scr :pi :color-pair '(:yellow :red))

    ;; insert "d " before pi
    (move scr 0 0)
    (insert scr "d ")

    ;; change the attributes of the d (without moving the cursor)
    (move scr 0 0)
    (change-attributes scr 1 '(:underline) :color-pair '(:green :black))

    ;; extract the complex d from the window
    (let ((e (extract-char scr)))

      ;; print the extracted char and its properties
      ;; format uses print-object specialized on complex-chars
      (move scr 1 0)
      (format scr "~S ~S ~S ~S" e (simple-char e) (attributes e) (color-pair e))

      ;; print the extracted character by its components
      (move scr 2 0)
      (add-char scr (simple-char e) :attributes (attributes e) :color-pair (color-pair e))

      ;; print the extracted complex character directly
      ;; n -1 = repeat to the end of the line
      (add-char scr e :y 3 :x 0 :n -1))

    (refresh scr)
    (get-char scr)))

(defun t21a ()
  "Tests for string wrapping, printing and extracting complex strings."
  (with-screen (scr :cursor-visible nil)
    ;; print strings with attributes and colors
    ;; attributes dont work (yet) when a long string gets wrapped around the last column.
    (add-string scr "string with attributes 1" :y 0 :x 0 :attributes '(:underline) :color-pair '(:black :green))

    ;; test adding both chars and strings with a single wrapper routine.
    (add scr #\a :y 2 :x 0 :attributes '(:underline) :color-pair '(:yellow :red) :n -1)

    ;; n limits the number of chars printed
    (add scr "string with attributes 2" :y 4 :x 0 :n 11 :attributes '(:underline :bold) :color-pair '(:yellow :green))

    ;; test print-object specialisation on complex-string
    (let ((str (make-instance 'complex-string :string "complex-string" :color-pair '(:white :blue) :attributes '(:underline))))
      (move scr 6 0)
      (format scr "~S" str)

      ;; print near the last column to force wrapping
      ;; assumes terminal size 80x25
      (add-string scr str :y 8 :x 70)
      ;; n -1 avoids wrapping and prints the string only till the end of the line.
      (add scr str :y 10 :x 70 :n -1))

    ;; extract 5 chars from the first line as a complex-string, then reprint it at line 12
    (let ((str (extract-complex-string scr :y 0 :x 0 :n 5)))
      (move scr 12 0)
      (add scr str :n -1))

    ;; extract 5 chars from the first line as a simple string, then reprint it at line 14
    (let ((str (extract-string scr :y 0 :x 0 :n 5)))
      (move scr 14 0)
      (add scr str))

    (refresh scr)
    (get-char scr)))

;; temporarily end the main screen, write something to the repl, and then return to the screen.
;; the predicate closed-p can be used to check whether the screen has been temporarily closed.
(defun t22 ()
  (let ((scr (make-instance 'screen :input-blocking t :input-echoing nil)))
    (unwind-protect
         (progn
           (clear scr)
           ;; should yield NIL, since we didnt endwin yet
           (format scr "1. screen before ending: ~A~%" (closed-p scr))
           (get-char scr)

           (close scr)

           ;; this shouldnt be visible, but it is visible after the later refresh
           ;; obviously ncurses doesnt destroy the screen window after it is closed
           (format scr "2. screen after ending, we dont see it till after the refresh: ~A~%" (closed-p scr))

           ;; should yield T, because we closed the screen
           (format t "3. in the repl, after ending the screen: ~A~%" (closed-p scr))
           (force-output *standard-output*)
           (sleep 5)

           (refresh scr)
           ;; should yield NIL, since we refreshed
           (format scr "4. screen after refreshing: ~A~%" (closed-p scr))

           (get-char scr))
      (close scr))))

(defun t23 ()
  "Use save-excursion to return the cursor to its initial position."
  (with-screen (scr :input-echoing nil :input-blocking t :cursor-visible t :enable-colors t)
    (move scr 0 0)
    (princ "1. hello" scr)
    (save-excursion scr
      (move scr 5 5)
      (princ "2. there" scr))
    (princ "3. dear john" scr)
    (get-char scr)))

(defun t24 ()
  "Test usage of insert-line and delete-line."
  (with-screen (scr :input-echoing nil :input-blocking t :cursor-visible t :enable-colors t)
    (loop for i from 0 to (- (height scr) 1)
       do
         (move scr i 0)
         (format scr "~A" i))
    (refresh scr)
    (event-case (scr event)
      ((:up :down) (move-direction scr (event-key event)) (refresh scr))
      (#\d (delete-line scr) (refresh scr))
      (#\i (insert-line scr) (refresh scr))
      (#\q (return-from event-case)))
    (get-char scr)))

(defun t25 ()
  "Test initialisation and refreshing of pads and sub-pads."
  (with-screen (scr :input-blocking t :cursor-visible nil :enable-colors t)
    (let* ((p (make-instance 'pad :height 100 :width 100))
           (sp (make-instance 'sub-pad :parent p :height 5 :width 10 :position (list 10 10))))
      ;; populate the pad with numbers.
      (loop for j from 0 to 99 do
        (loop for i from 0 to 99 do
          (move p j i)
          (format p "~D" (mod (* i j) 10))))
      ;; populate the sub-pad with letters.
      (loop for j from 0 to 4 do
        (loop for i from 0 to 9 do
          (move sp j i)
          (add-char sp #\X) ))

      ;; we have to modify the sub-window attributes first because apparently once
      ;; chars have attributes, they can not be changed by subsequent background changes.
      (setf (background sp) (make-instance 'complex-char :color-pair '(:white :red))
            (background p)  (make-instance 'complex-char :color-pair '(:green :white)))

      (let ((pad-min-y     0)
            (pad-min-x     0)
            (screen-min-y  0)
            (screen-min-x  0)
            (screen-max-y  5)
            (screen-max-x 10))
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
          (:right (incf pad-min-x) (incf screen-min-x) (incf screen-max-x) (touch scr) (refresh scr)
                  (refresh p pad-min-y pad-min-x screen-min-y screen-min-x screen-max-y screen-max-x))
          (#\q   (return-from event-case))
          (otherwise nil)))
      (close p)
      (close sp))))

(defun t26 ()
  "Test accessors of window and cursor positions."
  (with-screen (scr :input-echoing nil :input-blocking t :cursor-visible t :enable-colors t)
    (let* ((win (make-instance 'window :height 5 :width 20 :position (list 0 0)))
           (stack (make-instance 'stack :items (list win scr))))

      (setf (background scr) (make-instance 'complex-char :color-pair '(:white :red))
            (background win) (make-instance 'complex-char :color-pair '(:black :yellow)))

      (refresh stack)
      (get-char scr)

      (setf (widget-position win) (list 2 4))
      (refresh stack)
      (get-char scr)

      (setf (position-y win) 4)
      (refresh stack)
      (get-char scr)

      (setf (position-x win) 8)
      (refresh stack)
      (get-char scr)

      (setf (cursor-position win) (list 0 0))
      (princ "a" win)
      (refresh stack)
      (get-char scr)

      (setf (cursor-position-y win) 2)
      (princ "b" win)
      (refresh stack)
      (get-char scr)

      (setf (cursor-position-x win) 4)
      (princ "c" win)
      (refresh stack)
      (get-char scr))))

(defun t26a ()
  (with-screen (scr :input-echoing nil :input-blocking t :cursor-visible t :enable-colors t)
    (let* ((windows (mapcar (lambda (pos c)
                              (make-instance 'window :height 5 :width 20 :position pos :background (make-instance 'complex-char :bgcolor c)))
                            '((0 0) (2 4) (4 8) (0 40) (2 44) (4 48))
                            '(:red :yellow :cyan :red :yellow :cyan)))
           ;; the stack items can be passed as a list, order: 0 1 2
           (st1 (make-instance 'stack :items (subseq windows 0 3)))
           (st2 (make-instance 'stack)))

      ;; or stacked (pushed) individually, order: 5 4 3
      (loop for w in (subseq windows 3 6)
         do (stack-push w st2))

      (refresh st1)
      (refresh st2)
      (get-char (car windows)))))

(defun t27 ()
  "Use run-event-loop and bind instead of event-case to handle keyboard events."
  (with-screen (scr :input-echoing nil :input-blocking t)
    ;; count and display before and after event hooks
    (let ((a 0) (b 0))
      (hook scr 'before-event-hook
            (lambda (win)
              (save-excursion win
                (incf a)
                (move win 0 60) (format win "   ")
                (move win 0 60) (format win "~A" a)
                (refresh win))))
      (hook scr 'after-event-hook
            (lambda (win)
              (save-excursion win
                (incf b)
                (move win 1 60) (format win "   ")
                (move win 1 60) (format win "~A" b)
                (refresh win)))))

    ;; q ends the loop.
    (bind scr #\q 'exit-event-loop)

    ;; The event handler function has to take two arguments, the window and the event.

    ;; a and s add a string to the window.
    (bind scr #\a (lambda (win event) (declare (ignore event)) (format win "Hello there.~%")))
    (bind scr #\b (lambda (win event) (declare (ignore event)) (format win "Dear John.~%")))

    (bind scr '("^a" "^b" "^d") (lambda (w e) (format w "Control char: ~A~%" (event-key e))))

    ;; d clears the window.
    (bind scr #\d (lambda (win event) (declare (ignore event)) (clear win)))

    ;; u unbinds all keys other than q
    (bind scr #\u (lambda (win event) (declare (ignore win event)) (unbind scr '(#\a #\b #\d "^a" "^b" "^d"))))

    (clear scr)
    (add-string scr "Type a, b or d with or without Ctrl. Type q to quit.")
    (terpri scr)
    (refresh scr)

    (run-event-loop scr)))

(defun t28-hello (win event)
  (format win "Hello there ~A.~%" (event-key event)))

(defun t28-clear (win event)
  (declare (ignore event))
  (clear win))

(defparameter *t28-map*
  (make-instance 'keymap :bindings-plist
    (list
     #\q  'exit-event-loop
     #\a  #'t28-hello
     #\d  't28-clear))
  "Define a keymap separately and then set it as a window's event handler before running the event loop.")

(defparameter *t28-ctrl-x-map*
  (make-instance 'keymap :bindings-plist
    (list
     t    (lambda (win event) (format win "^X map: Default C-x map event handler ~A~%" (event-key event)))
     #\k  (lambda (win event) (format win "^X map: ~A~%" (event-key event)))
     ;; ^T is another prefix key, so the keys in that map can be reached via "^X ^T".
     "^T" *t28-map*))
  "A separately defined keymap bound to the prefix key ^X.")

(defun t28 ()
  "Use run-event-loop and a pre-defined event handler alist. Use a default handler."
  (with-screen (scr :input-echoing nil :input-blocking t)
    ;; add the separately defined keymap to the window object.
    (setf (keymap scr) *t28-map*)

    ;; add another event handler to the window instead of the external keymap
    ;; Object-local bindings override the external keymap. The local bindings
    ;; are checked first for a handler, then the external keymap.
    (bind scr #\s (lambda (win event) (format win "Dear John ~A~%" (event-key event))))

    ;; t is the default handler for all events without defined handlers.
    ;; The default event handler should not be used to handle the nil event when input-blocking is nil
    (bind *t28-map* t (lambda (win event) (format win "Default map event handler ~A~%" (event-key event))))

    ;; Defining a keymap as a handler for ^X makes it the prefix key for that keymap.
    (bind scr "^X" *t28-ctrl-x-map*)

    (clear scr)
    (format scr "Type a, s or d. Type ^X k. Type q to quit.~%")
    (refresh scr)

    (run-event-loop scr)))

(defun t28a ()
  "Test the use of the run-event-loop with non-blocking events."
  (with-screen (scr :input-echoing nil :input-blocking nil)

    (setf (keymap scr) *t28-map*)

    (bind scr #\s (lambda (win event) (format win "Dear John ~A~%" (event-key event))))

    ;; The handler function for the nil event will be called between keyboard events.
    (bind scr nil (lambda (win event) (declare (ignore event)) (format win ".")))

    ;; Defining a keymap as a hander for ^X makes it the prefix key for that keymap.
    (bind scr "^X" *t28-ctrl-x-map*)

    (clear scr)
    (format scr "Type a, s or d. Type ^X k. Type q to quit.~%")
    (refresh scr)

    ;; When input-blocking is nil, set the rate at which the nil event will be handled in fps (frames per second).
    ;; For the same effect as frame-rate of 1, you can set :input-blocking to 1000 ms.
    (setf (frame-rate scr) 1)

    (run-event-loop scr)))

(defun t28b-show-bindings (win event)
  (declare (ignore event))
  (format win "~S" (bindings (find-keymap (keymap win)))))

;; defines and centrally registers a keymap
(define-keymap t28b-map
  (#\q 'exit-event-loop)
  (#\a 't28-hello)
  (#\d 't28-clear)
  ("^X" *t28-ctrl-x-map*)
  ("^B" 't28b-show-bindings))

(defun t28b ()
  "Use run-event-loop and a pre-defined event handler alist. Use a default handler."
  (with-screen (scr :input-echoing nil :input-blocking t)
    ;; add the separately defined keymap to the window object.
    (setf (keymap scr) 't28b-map)

    ;; add another event handler to the window not to the external keymap
    ;; Object-local bindings override the external keymap. The local bindings
    ;; are checked first for a handler, then the external keymap.
    (bind scr #\s (lambda (win event) (format win "Dear John ~A~%" (event-key event))))

    ;; t is the default handler for all events without defined handlers.
    ;; The default event handler should not be used to handle the nil event when input-blocking is nil
    (bind (find-keymap 't28b-map) t (lambda (win event) (format win "Default event handler ~A~%" (event-key event))))

    (clear scr)
    (add-string scr "Type a, s or d. Type q to quit.")
    (refresh scr)

    ;; see waiting (input-blocking t) vs polling (input-blocking nil)
    ;; http://www.meandmark.com/sdlopenglpart6.html
    (run-event-loop scr)))

(defun draw-t29-shapes (window shapes &optional squarify)
  "Draw a list of shapes to window."
  (clear window)
  (unless (listp shapes) (setf shapes (list shapes)))
  (dolist (s shapes)
    (draw-shape window s squarify)))

;; Assumes an 80x24 terminal
(defun t29 ()
  "Draw an ASCII-art tree to illustrate the use of shapes."
  (with-screen (scr :input-blocking t :enable-colors t :input-echoing nil :cursor-visible nil :input-buffering nil)
    (let* ((leaf-char   (make-instance 'complex-char :simple-char #\O :color-pair '(:green :black)))
           (trunk-char  (make-instance 'complex-char :simple-char #\H :color-pair '(:white :black)))
           (ground-char (make-instance 'complex-char :simple-char #\i :color-pair '(:green :black)))
           (sun-char    (make-instance 'complex-char :simple-char #\o :color-pair '(:yellow :black)))

           (tree-trunk  (rectangle 19 27 5 4       :filled t :char trunk-char))
           (upper-crown (triangle 4 29 12 22 12 35 :filled t :char leaf-char))
           (lower-crown (triangle 8 29 18 20 18 37 :filled t :char leaf-char))
           (tree-crown  (merge-shapes upper-crown lower-crown))
           (ground      (line 23 0 23 80 :char ground-char))
           (sun         (circle 5 6 2 :char sun-char :filled t)))

      (draw-t29-shapes scr (list tree-trunk tree-crown ground sun) t)
      ;; wait for a keypress
      (get-char scr)

      ;; redraw with squarify nil
      (draw-t29-shapes scr (list tree-trunk tree-crown ground sun) nil)
      (get-char scr) )))

(defun t29a ()
  "Draw a line using the Bresenham algorithm."
  (with-screen (scr :input-echoing nil :input-blocking t :cursor-visible t :enable-colors nil)
    (let ((start (center-position scr))
          (x2 0)
          (y2 0))
      ;; Perform the actions before and after every non-nil event is handled in event-case.
      (hook scr 'before-event-hook (lambda (win) (clear win)))
      (hook scr 'after-event-hook (lambda (win) (draw-line win start (list y2 x2)) (refresh win)))

      ;; Draw a line from the center of the screen to the current cursor position.
      (draw-line scr start (list y2 x2))
      (refresh scr)

      ;; Move the cursor which is the end of the line.
      (event-case (scr event)
        (#\q (return-from event-case))
        (:up     (decf y2))
        (:down   (incf y2))
        (:left   (decf x2))
        (:right  (incf x2))))))

(defun t30 ()
  "Test color pair completion for style parameters."
  (with-screen (scr :input-echoing nil :input-blocking t :cursor-visible nil :enable-colors t)
    (with-windows ((w1 :height 5 :width 5 :position '(0 0))
                   (w2 :height 5 :width 5 :position '(0 5))
                   (w3 :height 5 :width 5 :position '(0 10)))
      (setf (background w1) (make-instance 'complex-char :color-pair '(:black :cyan))
            (background w2) (make-instance 'complex-char :color-pair '(:black :magenta))
            (background w3) (make-instance 'complex-char :color-pair '(:black :white)))
      ;; the character style contains only one color
      ;; the fg/bg of the target window is used to complete the color pair.
      (let ((s1 (list :attributes '(:bold) :fgcolor :red))
            (s2 (list :attributes '(:bold) :bgcolor :yellow)))
        (add w1 #\a :y 2 :x 2 :style s1)
        (add w2 #\b :y 2 :x 2 :style s1)
        (add w3 #\c :y 2 :x 2 :style s2))
      (refresh w1)
      (refresh w2)
      (refresh w3)
      (get-char w1))))

;; the ncurses window background property and the color-pair apparently set the same internal variable
;; setting one to nil sets the other to nil.
;; croatoan does not do that. here, the window color if it is set overrides the background property.
;; if a color parameter is directly passed, it overrides both the window color-pair and background.
(defun t31 ()
  "Test color pair completion for fgcolor and bgcolor parameters."
  (with-screen (scr :input-echoing nil :input-blocking t :cursor-visible nil :enable-colors t)
    (add-string scr "default colors" :y 0 :x 0) (refresh scr) (get-char scr)

    (setf (color-pair scr) '(:yellow :blue))
    (add-string scr "color-pair yellow on blue" :y 1 :x 0) (refresh scr) (get-char scr)

    ;; text is not printed red on white because the color-pair overrides the background color.
    (setf (background scr nil) (make-instance 'complex-char :simple-char #\- :color-pair '(:red :white)))
    (add-string scr "                          background red on white" :y 2 :x 0) (refresh scr) (get-char scr)

    (setf (color-pair scr) '(:blue :yellow))
    (add-string scr "color-pair blue on yellow" :y 3 :x 0) (refresh scr) (get-char scr)

    ;; text is not printed red on white because the color-pair overrides the background color.
    (setf (background scr nil) (make-instance 'complex-char :simple-char #\space :color-pair '(:white :red)))
    (add-string scr "                          background white on red" :y 4 :x 0) (refresh scr) (get-char scr)

    ;; now when we set the color-pair to nil, the text is printed in background colors.
    (setf (color-pair scr) nil)
    (add-string scr "color-pair nil" :y 5 :x 0) (refresh scr) (get-char scr)

    ;; when we then also set the background to nil, the text is displayed in default white on black.
    (setf (background scr nil) nil)
    (add-string scr "                          background nil" :y 6 :x 0) (refresh scr) (get-char scr)

    (add-string scr "fgcolor red bgcolor cyan" :y 7 :x 0 :fgcolor :red :bgcolor :cyan) (refresh scr) (get-char scr)
    (add-string scr "fgcolor red (bgcolor default)" :y 8 :x 0 :fgcolor :red)  (refresh scr) (get-char scr)
    (add-string scr "(fgcolor default) bgcolor cyan" :y 9 :x 0 :bgcolor :cyan) (refresh scr) (get-char scr)

    (setf (color-pair scr) '(:yellow :blue))
    ;; the color parameters override the color-pair window property
    (add-string scr "color pair yellow on blue, then fgcolor red" :y 10 :x 0 :fgcolor :red) (refresh scr) (get-char scr)
    (add-string scr "color pair yellow on blue, then bgcolor cyan" :y 11 :x 0 :bgcolor :cyan) (refresh scr) (get-char scr)

    ;; the background is ignored because color-pair is set
    (setf (background scr nil) (make-instance 'complex-char :simple-char #\. :color-pair '(:white :red)))
    (add-string scr "fgcolor red" :y 12 :x 0 :fgcolor :red)  (refresh scr) (get-char scr)
    (add-string scr "bgcolor cyan" :y 13 :x 0 :bgcolor :cyan)  (refresh scr) (get-char scr)

    ;; when the pair is set to nil, the background is used again.
    (setf (color-pair scr) nil)
    (add-string scr "fgcolor magenta" :y 14 :x 0 :fgcolor :magenta)  (refresh scr) (get-char scr)
    (add-string scr "bgcolor cyan" :y 15 :x 0 :bgcolor :cyan)  (refresh scr) (get-char scr) ))

(defun t31a ()
  "Test color pair completion for fgcolor and bgcolor parameters."
  (with-screen (scr :input-echoing nil :input-blocking t :cursor-visible nil :enable-colors t)
    (format scr "default colors~%") (refresh scr) (get-char scr)

    (setf (color-pair scr) '(:yellow :blue))
    (format scr "color-pair yellow on blue | background ---~%") (refresh scr) (get-char scr)

    ;; color-pair overrides the background char because format uses add-wide-char under the hood
    ;; format (add-wide-char) first checks the color pair of a window, then it checks the background.

    (setf (background scr nil) (make-instance 'complex-char :simple-char #\- :color-pair '(:red :white)))
    (format scr "color-pair yellow on blue | background red on white~%") (refresh scr) (get-char scr)

    ;; only after we remove the color-pair we get characters using the background colors.
    (setf (color-pair scr) nil)
    (format scr "color-pair nil            | background red on white~%") (refresh scr) (get-char scr)

    (clear scr)
    (format scr "clear uses the background char~%") (refresh scr) (get-char scr)

    (setf (color-pair scr) '(:blue :yellow))
    (format scr "color pair blue on yellow | background red on white~%") (refresh scr) (get-char scr)

    (setf (color-pair scr) nil)
    (format scr "color pair nil            | background red on white~%") (refresh scr) (get-char scr)

    (setf (background scr nil) nil)
    (format scr "color pair nil            | background nil~%") (refresh scr) (get-char scr)

    ;; clear with background nil
    (clear scr) (refresh scr) (get-char scr)))

(defun t31b ()
  "Test color pair completion for fgcolor and bgcolor window properties."
  (with-screen (scr :input-echoing nil :input-blocking t :cursor-visible nil :enable-colors t)
    (format scr "default colors~%") (refresh scr) (get-char scr)

    (setf (fgcolor scr) :yellow)
    (format scr "fgcolor yellow~%") (refresh scr) (get-char scr)

    (setf (bgcolor scr) :red)
    (format scr "fgcolor yellow bgcolor red~%") (refresh scr) (get-char scr)

    (setf (color-pair scr) nil)
    (format scr "color pair nil~%") (refresh scr) (get-char scr)

    (setf (bgcolor scr) :red)
    (add-string scr "fgcolor cyan bgcolor red" :fgcolor :cyan) (refresh scr) (get-char scr)

    (setf (background scr nil) (make-instance 'complex-char :simple-char #\- :color-pair '(:blue :white)))

    ;; the fg from background and bg from color-pair will be combined to blue on red.
    (format scr "~%color-pair nil on red | background blue on white~%") (refresh scr) (get-char scr)

    ;; when the window bgcolor is removed, the background simple-char is displayed instead of spaces
    (setf (bgcolor scr) nil)
    (format scr "color-pair nil | background blue on white~%") (refresh scr) (get-char scr)))

(defun t31c ()
  (with-screen (scr :input-echoing nil :input-blocking t :cursor-visible nil :enable-colors t)
    ;; red on default black
    (add scr #\a :fgcolor :red)
    (terpri scr)
    ;; yellow on default black
    (setf (fgcolor scr) :yellow)
    (format scr "~A~%" (color-pair scr))
    ;; white on default black
    (add scr #\b :fgcolor :white)
    ;; yellow (fgcolor of the window) on default black
    (add scr #\c)
    (refresh scr)
    (get-char scr)))

(defun t32 ()
  "Test/example of `croatoan:key-to-string' from util.lisp.

Press C-j, C-m, C-i, C-h to see the difference."
  (with-screen (scr :input-echoing         nil
                    :process-control-chars nil ; set to nil to be able to access C-s, C-c, C-z, C-q.
                    :input-blocking        t
                    :enable-function-keys  t
                    :cursor-visible        nil)
    (bind scr "^Q" 'exit-event-loop)
    (bind scr t (lambda (win e)
                  (with-accessors ((event event-key)) e
                    (clear win)
                    (add win "Press C-q to exit." :y 0 :x 0)
                    (move win 2 0)
                    (cond ((characterp event)
                           (let ((key-decoded  (key-to-string  event))
                                 (char-decoded (char-to-string event)))
                             (format win "event ~a key decoded ~a char decoded ~a" event key-decoded char-decoded)))
                          ((function-key-p (key-name-to-code event event))
                           (let ((key-decoded  (key-to-string (key-name-to-code event)))
                                 ;; decoding a char when taking a function key returns no useful result
                                 (char-decoded (char-to-string (key-name-to-code event))))
                             (move win 2 0)
                             (format win "event ~a key decoded ~a char decoded ~a" event key-decoded char-decoded)))
                          ((numberp event)
                           (format win "Unknown keycode ~a" event))
                          (t (format win "Unknown event ~A" event)))
                    (refresh win))))
    (clear scr)
    (add scr "Press C-q to exit." :y 0 :x 0)
    (refresh scr)
    (run-event-loop scr)))

(defun t33 ()
  "Showing the complex-string utility functions"
  (with-screen (scr :input-echoing  nil
                    :input-blocking t
                    :cursor-visible nil
                    :enable-colors  t)
    (flet ((add-xy (text x y)
             (add scr text :x x :y y)))
      (let ((bg       (make-background :black))
            (string-1 (make-instance 'complex-string
                                     :string "hello"
                                     :attributes '(:bold)
                                     :fgcolor :yellow))
            (string-2 (make-instance 'complex-string
                                     :string     "world!"
                                     :attributes '(:underline)
                                     :fgcolor    :red)))
        (setf (background scr) bg)
        (setf (fgcolor scr)    :white)
        (add-xy "Two strings with different attributes" 0 0)
        (add-xy string-1 0 2)
        (add-xy string-2 (1+ (text-width string-1)) 2)
        (add-xy (concatenate 'string
                             "The same two strings concatenated with a space between. "
                             "The second string "
                             "ihnerits the attributes of the first")
                0 4)
        (add-xy (concat-complex-string (concat-complex-string string-1 " ")
                                       string-2
                                       :color-attributes-contagion t)
                0 6)
        (add-xy "Two strings concatenated, the second keeps ist attributes" 0 8)
        (add-xy (concat-complex-string string-1 string-2) 0 10)
        (add-xy "Ellipsize text" 0 12)
        (add-xy (text-ellipsize (concat-complex-string string-1
                                                       string-2
                                                       :color-attributes-contagion nil)
                                9)
                0 14)
        (add-xy "Right padding with #\\+" 0 16)
        (add-xy (text-right-pad (concat-complex-string string-1
                                                       string-2
                                                       :color-attributes-contagion nil)
                                20
                                :padding-char #\+)

                0 18)
        (get-char scr)))))

(defun t34 ()
  "Fill and clear a rectangular area in a window."
  (with-screen (scr)
    (clear scr)
    ;; fill and clear rectangles given by 4 coordinates of the position and dimensions.
    (fill-rectangle scr #\. 5 10 10 20)
    (clear-rectangle scr 7 12 4 8)
    ;; fill and clear rectangles given by a position and dimensions given by 2-element lists
    (fill-rectangle scr #\* '(5 40) '(10 20))
    (clear-rectangle scr '(7 42) '(4 8))
    (refresh scr)
    (get-char scr)))
