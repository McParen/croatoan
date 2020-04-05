(in-package :de.anvi.ncurses)

;;; legacy
;;; get curses cursor and window coordinates, attributes
;;; http://invisible-island.net/ncurses/man/curs_legacy.3x.html

;;; C prototypes

;; int getattrs(WINDOW *win);
;; int getbegx(WINDOW *win);
;; int getbegy(WINDOW *win);
;; int getcurx(WINDOW *win);
;; int getcury(WINDOW *win);
;; int getmaxx(WINDOW *win);
;; int getmaxy(WINDOW *win);
;; int getparx(WINDOW *win);
;; int getpary(WINDOW *win);

;;; Low-level CFFI wrappers

(cffi:defcfun ("getbegx" getbegx) :int (win window))
(cffi:defcfun ("getbegy" getbegy) :int (win window))
(cffi:defcfun ("getcurx" getcurx) :int (win window))
(cffi:defcfun ("getcury" getcury) :int (win window))
(cffi:defcfun ("getmaxx" getmaxx) :int (win window))
(cffi:defcfun ("getmaxy" getmaxy) :int (win window))
(cffi:defcfun ("getparx" getparx) :int (win window))
(cffi:defcfun ("getpary" getpary) :int (win window))
