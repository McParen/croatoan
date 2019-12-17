(in-package :de.anvi.croatoan)

;; refresh windows _and_ pads
(defun refresh (win &optional pad-min-y pad-min-x screen-min-y screen-min-x screen-max-y screen-max-x)
  "Redisplay the window after changes have been made to it.

Copies a window to the virtual screen, then updates the visible
physical screen by the contents of the virtual screen.

Only updates the changed parts of the window. In order to redraw the
whole window, it has to be explicitely touched or marked for redraw."
  (let ((winptr (winptr win)))
    ;; typease whether window or pad
    ;; if window, signal error if any parameters are provided.
    ;; if pad, signal error if not all parameters are provided.
    ;; typecase uses type-of or typep internally.
    ;; if typep, a pad will be recognized as a window, since it is a subclass.
    ;; the pad type has to be checked first.
    (typecase win
      (pad (progn
             ;; all 6 arguments have to be given, we dont have default arguments.
             (unless (and pad-min-y pad-min-x screen-min-y screen-min-x screen-max-y screen-max-x)
               (error "One of the arguments for pad refreshing is missing."))
             (%prefresh winptr pad-min-y pad-min-x screen-min-y screen-min-x screen-max-y screen-max-x)))
      (window (%wrefresh winptr)))))

;; call refresh-marked after this.
(defun mark-for-refresh (win &optional pad-min-y pad-min-x screen-min-y screen-min-x screen-max-y screen-max-x)
  "Mark a window for a later batch-refresh.

Copy a window to the virtual screen, but do not display it on the
visible physical screen. Call batch-refresh to display all marked
refreshes."
  (let ((winptr (winptr win)))
    ;; typecase uses typep internally, so pad has to be checked first because it is a subclass of window.
    (typecase win
      (pad (progn
             (unless (and pad-min-y pad-min-x screen-min-y screen-min-x screen-max-y screen-max-x)
               (error "One of the arguments for pad refreshing is missing."))
             (%prefresh winptr pad-min-y pad-min-x screen-min-y screen-min-x screen-max-y screen-max-x)))
      (window (%wnoutrefresh winptr)))))

;; call this after several windows have been marked for refresh.
(defun refresh-marked ()
  "Refresh windows marked for refresh."
  (%doupdate))

;; does not redraw, only marks for redrawing by refresh.
;; It assumes that the display on the terminal has been corrupted.
;; It is unclear how redrawwin differs from touchwin.
(defun mark-for-redraw (window &key first-line no-of-lines)
  "Mark a whole window or a number of lines to be completely redrawn on the next refresh."
  (let ((winptr (winptr window)))
    (if (and first-line no-of-lines)
        (%wredrawln winptr first-line no-of-lines)
        (%redrawwin winptr))))
