(in-package :de.anvi.croatoan)

;; Copies a window to the virtual screen, then updates the visible
;; physical screen. refresh = no-out-refresh + do-update
(defun refresh (window &key (update t))
  (let ((winptr (.winptr window)))
    (if update
        (%wrefresh winptr)
        (%wnoutrefresh winptr))))

;; call this after (refresh :update nil)
(defun update ()
  "Updates the visible physical screen by the contents of the virtual screen.

This function should be used to display the result of a batch of refresh calls."
  (%doupdate))

(defun mark-for-redraw (window &key first-line count)
  "Mark a window or specified lines to be completely redrawn on the next refresh."
  (if (and first-line count)
      (%wredrawln window first-line count)
      (%redrawwin window)))
