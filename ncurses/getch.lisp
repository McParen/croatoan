(in-package :de.anvi.ncurses)

;;; getch
;;; get (or push back) characters from curses terminal keyboard
;;; http://invisible-island.net/ncurses/man/curs_getch.3x.html
;;; http://www.manpagez.com/man/3/curs_getch/

;;; C prototypes

;; int getch(void);
;; int wgetch(WINDOW *win);
;; int mvgetch(int y, int x);
;; int mvwgetch(WINDOW *win, int y, int x);
;; int ungetch(int ch);
;; int has_key(int ch);

;;; Low-level CFFI wrappers

(cffi:defcfun ("getch"    getch)    :int)
(cffi:defcfun ("wgetch"   wgetch)   :int (win window))
(cffi:defcfun ("mvgetch"  mvgetch)  :int              (y :int) (x :int))
(cffi:defcfun ("mvwgetch" mvwgetch) :int (win window) (y :int) (x :int))

(cffi:defcfun ("ungetch"  ungetch)  :int (ch :int))
(cffi:defcfun ("has_key"  has-key)  :int (ch :int))
