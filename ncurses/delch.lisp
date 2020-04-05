(in-package :de.anvi.ncurses)

;;; delch
;;; delete character under the cursor in a curses window
;;; http://invisible-island.net/ncurses/man/curs_delch.3x.html

;;; C prototypes

;; int delch(void);
;; int wdelch(WINDOW *win);
;; int mvdelch(int y, int x);
;; int mvwdelch(WINDOW *win, int y, int x);

;;; Low-level CFFI wrappers

(cffi:defcfun ("delch"     delch)    :int)
(cffi:defcfun ("wdelch"    wdelch)   :int (win window))
(cffi:defcfun ("mvdelch"   mvdelch)  :int              (y :int) (x :int))
(cffi:defcfun ("mvwdelch"  mvwdelch) :int (win window) (y :int) (x :int))
