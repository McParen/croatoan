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

(cffi:defcfun ("inch"    inch)    chtype)
(cffi:defcfun ("winch"   winch)   chtype (win window))
(cffi:defcfun ("mvinch"  mvinch)  chtype              (y :int) (x :int))
(cffi:defcfun ("mvwinch" mvwinch) chtype (win window) (y :int) (x :int))
