(in-package :de.anvi.ncurses)

;;; insch
;;; insert a character before cursor in a curses window
;;; http://invisible-island.net/ncurses/man/curs_insch.3x.html

;;; C prototypes

;; int insch(chtype ch);
;; int winsch(WINDOW *win, chtype ch);
;; int mvinsch(int y, int x, chtype ch);
;; int mvwinsch(WINDOW *win, int y, int x, chtype ch);

;;; Low-level CFFI wrappers

(defcfun ("insch"    %insch)    :int                                (ch chtype))
(defcfun ("winsch"   %winsch)   :int (win window)                   (ch chtype))
(defcfun ("mvinsch"  %mvinsch)  :int              (y :int) (x :int) (ch chtype))
(defcfun ("mvwinsch" %mvwinsch) :int (win window) (y :int) (x :int) (ch chtype))
