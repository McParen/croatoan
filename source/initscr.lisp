(in-package :de.anvi.croatoan)

(defun init-screen ()
  "Initializes the curses mode. Returns the main window."
  (%initscr))

(defun end-screen ()
  "Clean shutdown of the curses display."
  (%endwin))

(defgeneric closed-p (s)
  (:documentation "Check whether the screen has been closed without a subsequent call to refresh to reactivate it."))

(defmethod closed-p ((s screen))
  (declare (ignore s))
  (%isendwin))

(defun new-terminal (type out-fd in-fd)
  "Use instead of init-screen when you want more than one terminal."
  (%newterm type out-fd in-fd))

(defun set-current-terminal (new-screen)
  "Sets new-screen as the current terminal. Returns the old screen."
  (%set-term new-screen))


;;; TODOs

;; [ ] are files in lisp in newterm correctly represented by fd-s?
;; [ ] add type info either to the docs or in asserts.
;; [ ] document all possible return values and check for them.
