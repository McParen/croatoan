(in-package :de.anvi.ncurses)

;;; inch
;;; get a character and attributes from a curses window
;;; http://invisible-island.net/ncurses/man/curs_inch.3x.html

;;; C prototypes

;; chtype inch(void);
;; chtype winch(WINDOW *win);
;; chtype mvinch(int y, int x);
;; chtype mvwinch(WINDOW *win, int y, int x);

;;; Low-level CFFI wrappers

(defcfun ("inch"    %inch)   chtype)
(defcfun ("winch"   %winch)  chtype (win window))
(defcfun ("mvinch"  %mvinch) chtype              (y :int) (x :int))
(defcfun ("mvwinch" %winch)  chtype (win window) (y :int) (x :int))
