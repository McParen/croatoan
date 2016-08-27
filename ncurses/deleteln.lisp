(in-package :de.anvi.ncurses)

;;; deleteln
;;; delete and insert lines in a curses window
;;; http://invisible-island.net/ncurses/man/curs_deleteln.3x.html

;;; C prototypes

;; int deleteln(void);
;; int wdeleteln(WINDOW *win);
;; int insdelln(int n);
;; int winsdelln(WINDOW *win, int n);
;; int insertln(void);
;; int winsertln(WINDOW *win);

;;; Low-level CFFI wrappers

(defcfun ("deleteln"  %deleteln)  :int)
(defcfun ("wdeleteln" %wdeleteln) :int (win window))
(defcfun ("insdelln"  %insdelln)  :int              (n :int))
(defcfun ("winsdelln" %winsdelln) :int (win window) (n :int))
(defcfun ("insertln"  %insertln)  :int)
(defcfun ("winsertln" %winsertln) :int (win window))
