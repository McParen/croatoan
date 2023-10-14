(in-package :de.anvi.croatoan)

(defmethod refresh ((win window) &rest args)
  "Redisplay the window after changes have been made to it.

Copies a window to the virtual screen, then updates the visible
physical screen by the contents of the virtual screen.

Only updates the changed parts of the window. In order to redraw the
whole window, it has to be explicitely touched or marked for redraw.

A window does not require any additional arguments to be refreshed.
Any provided additional arguments are ignored."
  (declare (ignore args))
  (ncurses:wrefresh (winptr win)))

(defmethod refresh ((pad pad) &rest args)
  "A pad requires 6 additional arguments to be refreshed.

The additional arguments are required to specify which part of the pad
should be displayed and on which position on the screen.

The top-left corner of the rectangular area of the pad to be displayed.

1. pad-min-y
2. pad-min-x

The top-left and bottom-right corner of the screen where the pad should
be displayed.

3. screen-min-y
4. screen-min-x

5. screen-max-y
6. screen-max-y

All 6 arguments have to be given, otherwise an error is signalled.
There are no default arguments."
  (if (= (length args) 6)
      (apply #'ncurses:prefresh (winptr pad) args)
      (error "refresh pad: one of the pad position arguments is missing.")))

(defgeneric mark-for-refresh (win &rest args)
  (:documentation
  "Mark a window for a later refresh.

Copy a window to the virtual screen, but do not display it on the
visible physical screen.

After all windows are marked, call refresh-marked to display all
marked refreshes.

refresh = mark + refresh-marked

The goal of this batch refresh is improved efficiency and preventing
flicker that might occur if several refreshes are called in sequence."))

(defmethod mark-for-refresh ((win window) &rest args)
  "A window does not require any additional arguments to be refreshed."
  (declare (ignore args))
  (ncurses:wnoutrefresh (winptr win)))

(defmethod mark-for-refresh ((win pad) &rest args)
  "A pad requires 6 additional arguments to be refreshed.

The additional arguments are required to specify which part of the pad
should be displayed and on which position on the screen.

All 6 arguments have to be given, otherwise an error is signalled.
We don't have default arguments."
  (if (= (length args) 6)
      (apply #'ncurses:pnoutrefresh (winptr win) args)
      (error "mark-for-refresh pad: one of the pad arguments is missing.")))

(defmethod mark-for-refresh ((win panel) &rest args)
  "Refreshing a panel also refreshes its shadow and border, if existing."
  (declare (ignore args))
  (with-slots (border-win shadow-win winptr) win
    (when shadow-win (mark-for-refresh shadow-win))
    (when border-win (mark-for-refresh border-win))
    (ncurses:wnoutrefresh winptr)))

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
