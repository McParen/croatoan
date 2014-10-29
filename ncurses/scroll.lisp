(in-package :de.anvi.ncurses)

;;; scroll
;;; scroll a curses window
;;; http://invisible-island.net/ncurses/man/curs_scroll.3x.html

;;; C prototypes

;; int scroll(WINDOW *win);
;; int scrl(int n);
;; int wscrl(WINDOW *win, int n);

;;; Low-level CFFI wrappers

(defcfun ("scroll" %scroll) :int (win window))
(defcfun ("scrl"   %scrl)   :int              (n :int))
(defcfun ("wscrl"  %wscrl)  :int (win window) (n :int))
