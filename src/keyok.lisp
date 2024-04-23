(in-package :de.anvi.croatoan)

;; keyok
;; enable or disable a keycode
;; https://invisible-island.net/ncurses/man/keyok.3x.html

(defun enable-function-key (code &optional (enable t))
  "Enable (default) or disable a function key given by its code.

This function allows to disable specific keys as an alternative
to using ncurses:keypad to disable all function keys."
  (ncurses:keyok code enable))
