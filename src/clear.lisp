(in-package :de.anvi.croatoan)

;; (clear scr :redraw t)
;; (clear scr :target :window :redraw t)

;; (clear scr :target :end-of-line)
;; (clear scr :target :bottom)

(defgeneric clear (object &key))

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
    (case target
      (:window (if redraw (ncurses:wclear winptr) (ncurses:werase winptr)))
      (:end-of-line (ncurses:wclrtoeol winptr))
      (:bottom (ncurses:wclrtobot winptr)))))
