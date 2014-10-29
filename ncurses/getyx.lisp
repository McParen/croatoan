;; (in-package :de.anvi.ncurses)

;;; getyx
;;; get curses cursor and window coordinates
;;; http://invisible-island.net/ncurses/man/curs_getyx.3x.html

;;; C prototypes

;; void getyx(WINDOW *win, int y, int x);
;; void getparyx(WINDOW *win, int y, int x);
;; void getbegyx(WINDOW *win, int y, int x);
;; void getmaxyx(WINDOW *win, int y, int x);

;;; Low-level CFFI wrappers

;; see legacy.lisp

;;; NOTES

#|

These four C macros are defined in terms of other, simpler macros:

#define getyx(win,y,x)          (y = getcury(win), x = getcurx(win))
#define getbegyx(win,y,x)       (y = getbegy(win), x = getbegx(win))
#define getmaxyx(win,y,x)       (y = getmaxy(win), x = getmaxx(win))
#define getparyx(win,y,x)       (y = getpary(win), x = getparx(win))

And those simpler macros are just accessing the window struct.

#define getcurx(win)            ((win) ? (win)->_curx : ERR)
#define getcury(win)            ((win) ? (win)->_cury : ERR)
#define getbegx(win)            ((win) ? (win)->_begx : ERR)
#define getbegy(win)            ((win) ? (win)->_begy : ERR)
#define getmaxx(win)            ((win) ? ((win)->_maxx + 1) : ERR)
#define getmaxy(win)            ((win) ? ((win)->_maxy + 1) : ERR)
#define getparx(win)            ((win) ? (win)->_parx : ERR)
#define getpary(win)            ((win) ? (win)->_pary : ERR)

Those are defined as low-level functions in legacy.lisp.

|#
