(in-package :croatoan)

;;; scroll
;;; scroll a curses window
;;; http://invisible-island.net/ncurses/man/curs_scroll.3x.html

;;; C prototypes

;; int scroll(WINDOW *win);
;; int scrl(int n);
;; int wscrl(WINDOW *win, int n);

;;; Low-level C functions

(defcfun ("scroll" %scroll) :int (win window))
(defcfun ("scrl"   %scrl)   :int              (n :int))
(defcfun ("wscrl"  %wscrl)  :int (win window) (n :int))

;;; High-level Lisp wrappers

(defun scroll (window &optional (n 1))
  "Scroll the window for n lines.

If n is positive, scroll the window down. If n is negative, scroll the
window up. The cursor position is not changed."
  (%wscrl n))

;;; TODOs

;; [ ] what about return values?
