(in-package :de.anvi.croatoan)

;; to use clearok with curscr, use ncurses:clearok directly.
(defun (setf redraw-on-clear-p) (flag window)
  "If flag is t, when refresh is called after clear, it will redraw the screen from scratch."
  (ncurses:clearok (winptr window) flag))

(defun redraw-on-clear-p (window)
  "If t, the next refresh will redraw the screen from scratch."
  (ncurses:is_cleared (winptr window)))


(defun (setf insert-delete-char-p) (flag window)
  "If flag is t, use the hardware insert/delete char feature, if the terminal supports it.

It is enabled by default."
  (ncurses:idcok (winptr window) flag))

(defun insert-delete-char-p (window)
  "If t, use the hardware insert/delete char feature, if the terminal supports it."
  (ncurses:is_idcok (winptr window)))


(defun (setf insert-delete-line-p) (flag window)
  "If flag is t, use the hardware insert/delete line feature, if the terminal supports it.

It is disabled by default."
  (ncurses:idlok (winptr window) flag))

(defun insert-delete-line-p (window)
  "If t, use the hardware insert/delete line feature, if the terminal supports it."
  (ncurses:is_idlok (winptr window)))


(defun (setf immediately-refresh-p) (flag window)
  "If flag is t, any change to a window will automatically call refresh.

It is disabled by default, since it can degrade performance."
  (ncurses:immedok (winptr window) flag))

(defun immediately-refresh-p (window)
  "If t, any change to a window will automatically call refresh."
  (ncurses:is_immedok (winptr window)))


(defun (setf leave-cursor-on-refresh-p) (flag window)
  "If flag is t, don't move the cursor back to the position before refresh.

It is disabled by default."
  (ncurses:leaveok (winptr window) flag))

(defun leave-cursor-on-refresh-p (window)
  "If t, don't move the cursor back to the position before refresh."
  (ncurses:is_leaveok (winptr window)))
