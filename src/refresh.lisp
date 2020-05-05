(in-package :de.anvi.croatoan)

(defgeneric refresh (obj &rest args)
  (:documentation
   "Redisplay the object after changes have been made to it.

Copy the object to the virtual screen, then updates the visible
physical screen by the contents of the virtual screen."))

(defmethod refresh ((win window) &rest args)
  "Redisplay the window after changes have been made to it.

Copies a window to the virtual screen, then updates the visible
physical screen by the contents of the virtual screen.

Only updates the changed parts of the window. In order to redraw the
whole window, it has to be explicitely touched or marked for redraw.

A window does not require any additional arguments to be refreshed.
Any provided additional arguments are ignored."
  (ncurses:wrefresh (winptr win)))

(defmethod refresh ((pad pad) &rest args)
  "A pad requires additional arguments to be refreshed.

The additional arguments are required to specify which part of the pad
should be displayed and on which position on the screen.

All 6 arguments have to be given, otherwise an error is signalled. 
We don't have default arguments."
  (destructuring-bind (pad-min-y pad-min-x screen-min-y screen-min-x screen-max-y screen-max-x) args
    (unless (and pad-min-y pad-min-x screen-min-y screen-min-x screen-max-y screen-max-x)
      (error "One of the arguments for pad refreshing is missing."))
    (ncurses:prefresh (winptr pad) pad-min-y pad-min-x screen-min-y screen-min-x screen-max-y screen-max-x)))

(defun mark-for-refresh (win &rest args)
  "Mark a window for a later refresh.

Copy a window to the virtual screen, but do not display it on the
visible physical screen.

After all windows are marked, call refresh-marked to display all
marked refreshes.

The goal of this batch refresh is improved efficiency and preventing
flicker that might occur if several refreshes are called in sequence.

A window does not require any additional arguments to be refreshed.

A pad requires additional arguments to be refreshed. The additional
arguments are required to specify which part of the pad should be
displayed and on which position on the screen."
  (let ((winptr (winptr win)))
    (typecase win
      ;; typease whether window or pad
      ;; if window, any parameters provided are ignored.
      ;; if pad, signal error if not all parameters are provided.
      ;; typecase uses type-of or typep internally.
      ;; if typep, a pad will be recognized as a window, since it is a subclass.
      ;; the pad type has to be checked first.
      (pad
       (destructuring-bind (pad-min-y pad-min-x screen-min-y screen-min-x screen-max-y screen-max-x) args
         (unless (and pad-min-y pad-min-x screen-min-y screen-min-x screen-max-y screen-max-x)
           (error "One of the arguments for pad refreshing is missing."))
         (ncurses:pnoutrefresh winptr pad-min-y pad-min-x screen-min-y screen-min-x screen-max-y screen-max-x)))
      (window
       (ncurses:wnoutrefresh winptr)))))

(defun refresh-marked ()
  "Refresh one or more windows marked for refresh.

If only one window is refreshed, refresh can be called directly.

The goal of this batch refresh is improved efficiency and preventing
flicker that might occur if several windows are refreshed in sequence."
  (ncurses:doupdate))

(defun mark-for-redraw (window &key first-line no-of-lines)
  "Mark a whole window or a number of lines to be completely redrawn on the next refresh.

It does not redraw, only marks for redrawing by refresh. 

It assumes that the display on the terminal has been corrupted.

It is unclear how redrawwin differs from touchwin."
  (let ((winptr (winptr window)))
    (if (and first-line no-of-lines)
        (ncurses:wredrawln winptr first-line no-of-lines)
        ;; if no lines have been given, redraw the whole window.
        (ncurses:redrawwin winptr))))
