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

(defcfun ("getch"    %getch)    :int)
(defcfun ("wgetch"   %wgetch)   :int (win window))
(defcfun ("mvgetch"  %mvgetch)  :int              (y :int) (x :int))
(defcfun ("mvwgetch" %mvwgetch) :int (win window) (y :int) (x :int))

(defcfun ("ungetch"  %ungetch)  :int (ch :int))
(defcfun ("has_key"  %has-key)  :int (ch :int))
