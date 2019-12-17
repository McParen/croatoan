(in-package :de.anvi.croatoan)

;; (box win hline vline) = (draw-border win vline vline hline hline nil nil nil nil)
(defun box (window &optional (hline-char 0) (vline-char 0))
  "Draw a border around the window.

If any parameter is nil or zero, the default ACS char will be used."
  (let ((winptr (winptr window)))
    (%box winptr hline-char vline-char)))

(defun draw-border (window &key left right top bottom                        ;; lines
                                top-left top-right bottom-left bottom-right) ;; corners
  "Draw a border around the window using single-byte line-drawing characters.

If no border chars are given, the default ncurses ACS chars will be used."
  (let ((winptr (winptr window)))
    (apply #'%wborder
           winptr
           ;; if the argument is not nil, convert it to chtype first, the pass it to wborder.
           ;; if the argument is nil, pass 0 to wborder, then the default ACS char will be used.
           (mapcar #'(lambda (i) (if i (make-chtype i nil nil) 0))
                   (list left right top bottom                            ;; lines
                         top-left top-right bottom-left bottom-right))))) ;; corners
