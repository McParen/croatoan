(in-package :croatoan)

;;; wresize
;;; resize a curses window
;;; http://invisible-island.net/ncurses/man/wresize.3x.html

;;; C prototypes

;; int wresize(WINDOW *win, int lines, int columns);

;;; Low-level C functions

(defcfun ("wresize" %wresize) :int (win window) (lines :int) (columns :int))

;;; High-level Lisp wrappers

(defun resize-window (window height width)
  (%wresize window height width))

;;; TODOs

