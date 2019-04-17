(in-package :de.anvi.croatoan.tests)

;; Example from "Land of Lisp", Copyright by Conrad Barski.
;; Source: http://landoflisp.com/evolution.lisp
;; Unknown licence.

(defparameter *width*  100)
(defparameter *height* 30)
(defparameter *jungle* '(45 10 10 10))
(defparameter *plant-energy* 80)
(defparameter *plants* (make-hash-table :test #'equal))
(defparameter *reproduction-energy* 200)

(defstruct animal x y energy dir genes)


(defun random-plant (left top width height)
  (let ((pos (cons (+ left (random width))
                   (+ top  (random height)))))
    (setf (gethash pos *plants*) t)))

(defun add-plants ()
  ;; First one in the jungle.
  (apply #'random-plant *jungle*)
  ;; Then one in the rest of the world.
  (random-plant 0 0 *width* *height*))

;; Define one starting animal.
(defparameter *animals*
  (list (make-animal :x (ash *width* -1)
                     :y (ash *height* -1)
                     :energy 1000
                     :dir 0
                     :genes (loop repeat 8 collect (1+ (random 10))))))

(defun move- (animal)
  (let ((dir (animal-dir animal))
        (x   (animal-x animal))
        (y   (animal-y animal)))

    ;; If x>width, wrap around the world border with the mod function.
    (setf (animal-x animal)
          (mod (+ x (cond ((and (>= dir 2) (< dir 5)) 1)
                          ((or  (=  dir 1) (= dir 5)) 0)
                          (t -1))
                  *width*)
               *width*))

    ;; Down is +1, up is -1.
    (setf (animal-y animal)
          (mod (+ y (cond ((and (>= dir 0) (< dir 3)) -1)
                          ((and (>= dir 4) (< dir 7))  1)
                          (t 0))
                  *height*)
               *height*))

    (decf (animal-energy animal))))

(defun turn (animal)
  (let ((x (random (apply #'+ (animal-genes animal)))))
    (labels ((angle (genes x)
               (let ((xnu (- x (car genes))))
                 (if (< xnu 0)
                     0
                     (1+ (angle (cdr genes) xnu))))))

      (setf (animal-dir animal)
            (mod (+ (animal-dir animal)
                    (angle (animal-genes animal) x))
                 8)))))

(defun eat (animal)
  (let ((pos (cons (animal-x animal) (animal-y animal))))
    (when (gethash pos *plants*)
      (incf (animal-energy animal) *plant-energy*)
      (remhash pos *plants*))))

(defun reproduce (animal)
  (let ((e (animal-energy animal)))
    (when (>= e *reproduction-energy*)
      (setf (animal-energy animal) (ash e -1))
      (let ((animal-nu (copy-structure animal))
            (genes     (copy-list (animal-genes animal)))
            (mutation  (random 8)))
        ;; mutate a random gene by +1 or -1.
        (setf (nth mutation genes)
              (max 1 (+ (nth mutation genes) (random 3) -1)))
        ;; deep copy of the gene list.
        (setf (animal-genes animal-nu) genes)
        ;; push the new animal to the list.
        (push animal-nu *animals*)))))

(defun update-world ()
  ;; Remove dead animals.
  (setf *animals*
        (remove-if (lambda (animal) (<= (animal-energy animal) 0))
                   *animals*))
  ;; Do what animals do.
  (mapc (lambda (animal)
          (turn animal)
          (move- animal)
          (eat animal)
          (reproduce animal))
        *animals*)
  ;; Grow plants.
  (add-plants))

;;; simple non-ncurses version from LOL, prints to REPL.
(defun draw-world ()
  (loop for y
        below *height*
        do (progn (fresh-line)
                  ;; beginning of the line.
                  (princ "|")
                  (loop for x
                        below *width*
                        do (princ (cond 
                                    ;; if there is one or more animals, print a M.
                                    ((some (lambda (animal) (and (= (animal-x animal) x)
                                                            (= (animal-y animal) y)))
                                           *animals*)
                                     #\M)
                                    ;; if there is a plant, print *
                                    ((gethash (cons x y) *plants*) #\*)
                                    ;; if there is neithe a plant nor an animal, print a space.
                                    (t #\space))))
                  ;; end of the line.
                  (princ "|"))))

;; enter a recursive infinite loop as the programs main loop.
(defun evolution ()
  (draw-world)
  ;; add an empty line between worlds.
  (fresh-line)
  (let ((str (read-line)))
    (cond ((equal str "quit") ())
          (t (let ((x (parse-integer str :junk-allowed t)))
               (if x
                   (loop for i
                      below x
                      do (update-world)
                      if (zerop (mod i 1000))
                      do (princ #\.))
                   (update-world))
               (evolution))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; instead of using princ to draw to stdout, use add-string to draw to the curses screen.
;; the screen has to be initialized first in the main function evolve.
(defun draw-world-croatoan (scr)
  (loop 
     for y 
     from 0
     below *height*
     do (loop 
           for x 
           from 0
           below *width*
           do (add-string scr
                          (format nil "~A"
                                  (cond 
                                    ;; if there is one or more animals, print a M.
                                    ((some (lambda (animal) (and (= (animal-x animal) x)
                                                            (= (animal-y animal) y)))
                                           *animals*)
                                     #\M)
                                    ;; if there is a plant, print *
                                    ((gethash (cons x y) *plants*) #\*)
                                    ;; if there is neithe a plant nor an animal, print a space.
                                    (t #\space)))
                          :y y
                          :x x)))
  ;; refresh the physical screen to dsplay the drawn changes.
  (refresh scr))

;; enter a recursive infinite loop as the programs main loop.
(defun evolve ()
  (with-screen (scr :input-blocking nil :input-echoing nil :cursor-visibility nil)
    (clear scr)
    (setf (.background scr) (make-instance 'complex-char :color-pair '(:green :white)))

    (setq *width* (.width scr))
    (setq *height* (.height scr))

    (loop 
       initially 
         (draw-world-croatoan scr)

       for ch = (get-char scr)

       while (or (= ch -1) (not (equal (code-char ch) #\q)))
       do
         (update-world) 
         (sleep 0.001)
         (draw-world-croatoan scr))))

;; uses tagbody to separate between event handling and body.
(defun evolve2 ()
  (with-screen (scr :input-blocking nil :input-echoing nil :cursor-visibility nil)
    (clear scr)
    (setf (.background scr) (make-instance 'complex-char :color-pair '(:green :white)))

    (setq *width* (.width scr))
    (setq *height* (.height scr))

    (draw-world-croatoan scr)
    (refresh scr)

    (loop
     (tagbody

      ;; read a char.
      (let ((ch (get-char scr)))

        ;; if it is -1, just execute the body.
        ;; this is a special event when there is no event.
        (cond ((= ch -1) (go evolution-body))

              ;; And from here we have real key/character events.
              ((equal (code-char ch) #\q) (return))))

      evolution-body
      
      (update-world) 
      (sleep 0.001)
      (draw-world-croatoan scr)
      (refresh scr)))))

;; uses key-pressed-p to separate between events and the body, instead of loop and tagbody used in 1 and 2.
(defun evolve3 ()
  (with-screen (scr :input-blocking nil :input-echoing nil :cursor-visibility nil)
    (clear scr)
    ;(setf (.background scr) (make-instance 'complex-char :color-pair '(:green :white)))

    (setq *width* (.width scr))
    (setq *height* (.height scr))

    (draw-world-croatoan scr)

    (loop
     (if (key-pressed-p scr)
         (let ((ch (get-char scr)))
           (cond ((equal (code-char ch) #\q) (return))))
       (progn     
         (update-world) 
         (sleep 0.001)
         (draw-world-croatoan scr) )))))

(defun evolve4 ()
  "Uses get-event to separate between events and the body, instead of key-pressed-p."
  (with-screen (scr :input-blocking nil :input-echoing nil :cursor-visibility nil)
    (clear scr)
    ;(setf (.background scr) (make-instance 'complex-char :color-pair '(:green :white)))

    (setq *width* (.width scr))
    (setq *height* (.height scr))

    (draw-world-croatoan scr)

    (loop
       (let ((event (get-event scr)))
         (if event
             (case event
               (#\q (return)))
             (progn     
               (update-world) 
               (sleep 0.001)
               (draw-world-croatoan scr) ))))))

(defun evolve5 ()
  "Use the event-case macro for event handling."
  (with-screen (scr :input-blocking nil :input-echoing nil :cursor-visibility nil)
    (clear scr)

    (setq *width* (.width scr))
    (setq *height* (.height scr))

    (draw-world-croatoan scr)

    (event-case (scr event)
      (#\q (return-from event-case))
      ((nil) 
       (update-world) 
       (sleep 0.001)
       (draw-world-croatoan scr)))))

(defun update-game-state (win event)
  "Main game state update function for evolve6, called during the nil event handling."
  (declare (ignore event))
  (update-world)
  (draw-world-croatoan win))

(defun evolve6 ()
  "Use the run-event-loop for event handling."
  (with-screen (scr :input-blocking nil :input-echoing nil :cursor-visibility nil)
    (clear scr)

    (setq *width* (.width scr))
    (setq *height* (.height scr))

    (bind scr #\q 'exit-event-loop)
    (bind scr nil 'update-game-state)

    ;; For the same effect, set :input-blocking to 1 milisecond.
    (setf (.frame-rate scr) 1000)

    ;; Draw the world once before entering the main loop.
    (draw-world-croatoan scr)
    
    (run-event-loop scr)))
