(in-package :de.anvi.ncurses)

;;; wresize
;;; resize a curses window
;;; http://invisible-island.net/ncurses/man/wresize.3x.html

;;; C prototypes

;; int wresize(WINDOW *win, int lines, int columns);

;;; Low-level CFFI wrappers

(cffi:defcfun ("wresize" wresize) :int (win window) (lines :int) (columns :int))
