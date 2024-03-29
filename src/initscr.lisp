(in-package :de.anvi.croatoan)

(defun init-screen ()
  "Initializes the curses mode. Returns the main window."
  (ncurses:initscr))

(defun end-screen ()
  "Clean shutdown of the curses display."
  (ncurses:endwin))

(defgeneric closed-p (s)
  (:documentation "Check whether the screen has been closed without a subsequent call to refresh to reactivate it."))

(defmethod closed-p ((s screen))
  (declare (ignore s))
  (ncurses:isendwin))

(defun new-terminal (type out-fd in-fd)
  "Use instead of init-screen when you want more than one terminal."
  (ncurses:newterm type out-fd in-fd))

(defun set-current-terminal (new-screen)
  "Sets new-screen as the current terminal. Returns the old screen."
  (ncurses:set-term new-screen))
