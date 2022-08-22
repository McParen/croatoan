(in-package :de.anvi.croatoan)

;; (box win hline vline) = (draw-border win vline vline hline hline nil nil nil nil)
(defun box (window &optional (hline-char 0) (vline-char 0))
  "Draw a border around the window.

If any parameter is nil or zero, the default ACS char will be used."
  (let ((winptr (winptr window)))
    (ncurses:box winptr hline-char vline-char)))

(defun draw-hline (win y x length &optional char &rest keys &key &allow-other-keys)
  (let ((ch (if (null char)
                (acs :horizontal-line)
                char)))
    (dotimes (i length)
      (apply #'put-char win y (+ x i) ch keys))))

(defun draw-vline (win y x length &optional char &rest keys &key &allow-other-keys)
  (let ((ch (if (null char)
                (acs :vertical-line)
                char)))
    (dotimes (i length)
      (apply #'put-char win (+ y i) x ch keys))))

(defun draw-rectangle (win y x h w &rest keys &key &allow-other-keys)
  (apply #'put-char win    y          x       (acs :upper-left-corner)  keys)
  (apply #'put-char win    y       (+ x w -1) (acs :upper-right-corner) keys)
  (apply #'put-char win (+ y h -1)    x       (acs :lower-left-corner)  keys)
  (apply #'put-char win (+ y h -1) (+ x w -1) (acs :lower-right-corner) keys)
  (apply #'draw-vline win (1+ y)    x       (- h 2) nil keys)
  (apply #'draw-vline win (1+ y) (+ x w -1) (- h 2) nil keys)
  (apply #'draw-hline win    y       (1+ x) (- w 2) nil keys)
  (apply #'draw-hline win (+ y h -1) (1+ x) (- w 2) nil keys))

(defun draw-border (window &key left right top bottom                        ;; lines
                                top-left top-right bottom-left bottom-right) ;; corners
  "Draw a border around the window using single-byte line-drawing characters.

If no border chars are given, the default ncurses ACS chars will be used."
  (let ((winptr (winptr window)))
    (apply #'ncurses:wborder
           winptr
           ;; if the argument is not nil, convert it to chtype first, the pass it to wborder.
           ;; if the argument is nil, pass 0 to wborder, then the default ACS char will be used.
           (mapcar #'(lambda (i) (if i (make-chtype i nil nil) 0))
                   (list left right top bottom                            ;; lines
                         top-left top-right bottom-left bottom-right))))) ;; corners
