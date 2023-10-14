(in-package :de.anvi.croatoan)

;; (clear scr :redraw t)
;; (clear scr :target :window :redraw t)

;; (clear scr :target :end-of-line)
;; (clear scr :target :bottom)

(defmethod clear ((window window) &key redraw (target :window))
  "Clear the window by overwriting it with blanks.

If the keyword redraw is t, first copy blanks to every position in the
window, then set the clear-redraw-flag to have the window redrawn from
scratch on the next refresh.

If target is :end-of-line, clear the window from the cursor to the end
of the current line.

If target is :bottom, clear the window from the cursor to the end of
the current line and all lines below."
  (let ((winptr (winptr window)))
    (ecase target
      (:window (if redraw (ncurses:wclear winptr) (ncurses:werase winptr)))
      (:end-of-line (ncurses:wclrtoeol winptr))
      (:bottom (ncurses:wclrtobot winptr)))))

(defun fill-rectangle (win char &rest args)
  "Display char in every cell of a rectangular area in a window.

The rectangle is specified by a starting position and its dimensions.

4 arguments: y0 x0 height width
2 arguments: (y0 x0) (h w)"
  (cond ((= (length args) 4)
         ;; 4 separate args
         (destructuring-bind (y0 x0 h w) args
           (dogrid ((i y0 h)
                    (j x0 w))
             (put win i j char))))
        ((= (length args) 2)
         ;; two 2-element lists
         (destructuring-bind ((y0 x0) (h w)) args
           (dogrid ((i y0 h)
                    (j x0 w))
             (put win i j char))))
        (t
         (error "fill-rectangle: Wrong number of arguments."))))

(defun clear-rectangle (win &rest args)
  "Clear the rectangular area of a window by overwriting it with space.

If the background of a window is set, then the background char is used instead."
  (apply #'fill-rectangle win #\space args))
