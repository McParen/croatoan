(in-package :de.anvi.croatoan)

;; refresh = no-out-refresh + do-update
(defun refresh (window)
  "Redisplay the window after changes have been made to it.

Copies a window to the virtual screen, then updates the visible
physical screen by the contents of the virtual screen.

Only updates the changed parts of the window. In order to redraw the
whole window, it has to be explicitely touched or marked for redraw."
  (let ((winptr (.winptr window)))
        (%wrefresh winptr)))

;; call this after several windows have been marked for refresh.
(defun refresh-marked ()
  "Refresh windows marked for refresh."
  (%doupdate))

;; call batch-refresh after this.
(defun mark-for-refresh (window)
  "Mark a window for a later batch-refresh.

Copy a window to the virtual screen, but do not display it on the
visible physical screen. Call batch-refresh to display all marked
refreshes."
  (let ((winptr (.winptr window)))
    (%wnoutrefresh winptr)))

;; does not redraw, only marks for redrawing by refresh.
;; It assumes that the display on the terminal has been corrupted.
;; It is unclear how redrawwin differs from touchwin.
(defun mark-for-redraw (window &key first-line no-of-lines)
  "Mark a whole window or a number of lines to be completely redrawn on the next refresh."
  (let ((winptr (.winptr window)))
    (if (and first-line no-of-lines)
        (%wredrawln winptr first-line no-of-lines)
        (%redrawwin winptr))))
