(in-package :croatoan)

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

;;; Low-level C functions

(defcfun ("getbegx" %getbegx) :int (win window))
(defcfun ("getbegy" %getbegy) :int (win window))
(defcfun ("getcurx" %getcurx) :int (win window))
(defcfun ("getcury" %getcury) :int (win window))
(defcfun ("getmaxx" %getmaxx) :int (win window))
(defcfun ("getmaxy" %getmaxy) :int (win window))
(defcfun ("getparx" %getparx) :int (win window))
(defcfun ("getpary" %getpary) :int (win window))

;;; High-level Lisp wrappers

;; See getyx.lisp.

;;; NOTES

;;; TODOs

;; getattrs compare with attr_get

