(in-package :de.anvi.ncurses)

;;; move
;;; move curses window cursor
;;; http://invisible-island.net/ncurses/man/curs_move.3x.html
;;; http://www.manpagez.com/man/3/curs_move/

;;; C prototypes

;; int move(int y, int x);
;; int wmove(WINDOW *win, int y, int x);

;;; Low-level CFFI wrappers

(cffi:defcfun ("move"  move)  :int              (y :int) (x :int))
(cffi:defcfun ("wmove" wmove) :int (win window) (y :int) (x :int))
