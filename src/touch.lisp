(in-package :de.anvi.croatoan)

(defgeneric touch (win)
  (:documentation
   "Touching a window marks all its cells as changed.

This makes the next call to refresh rewrite whe whole window instead
of only the cells that have actually been changed.

The combination of touch and refresh is required to raise overlapping
windows."))

(defmethod touch ((win window))
  (ncurses:touchwin (winptr win)))

(defmethod touch ((win panel))
  "Touching a panel also touches its shadow and border window, if existing."
  (with-slots (border-win shadow-win winptr) win
    (when shadow-win (touch shadow-win))
    (when border-win (touch border-win))
    (ncurses:touchwin winptr)))
