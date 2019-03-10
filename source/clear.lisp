(in-package :de.anvi.croatoan)

;; (clear scr :redraw t)
;; (clear scr :target :whole-screen :redraw t)

;; (clear scr :target :end-of-line)
;; (clear scr :target :bottom)

(defgeneric clear (object &key))

(defmethod clear ((window window) &key redraw (target :whole-window))
  "Clear the window by overwriting it with blanks.

If the keyword redraw is t, first copy blanks to every position in the
window, then set the clear-redraw-flag to have the window redrawn from
scratch on the next refresh.

If target is :end-of-line, clear the window from the cursor to the end
of the current line.

If target is :bottom, clear the window from the cursor to the end of
the current line and all lines below."
  (let ((winptr (.winptr window)))
    (case target
      (:whole-window (if redraw (%wclear winptr) (%werase winptr)))
      (:end-of-line (%wclrtoeol winptr))
      (:bottom (%wclrtobot winptr)))))
