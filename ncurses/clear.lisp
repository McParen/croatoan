(in-package :de.anvi.ncurses)

;;; clear
;;; clear all or part of a curses window
;;; http://invisible-island.net/ncurses/man/curs_clear.3x.html
;;; http://linux.die.net/man/3/erase

;;; C prototypes

;; int erase(void);
;; int werase(WINDOW *win);
;; int clear(void);
;; int wclear(WINDOW *win);
;; int clrtobot(void);
;; int wclrtobot(WINDOW *win);
;; int clrtoeol(void);
;; int wclrtoeol(WINDOW *win);

;;; Low-level CFFI wrappers

(cffi:defcfun ("erase"     %erase)     :int)
(cffi:defcfun ("werase"    %werase)    :int (win window))
(cffi:defcfun ("clear"     %clear)     :int)
(cffi:defcfun ("wclear"    %wclear)    :int (win window))

(cffi:defcfun ("clrtobot"  %clrtobot)  :int)
(cffi:defcfun ("wclrtobot" %wclrtobot) :int (win window))
(cffi:defcfun ("clrtoeol"  %clrtoeol)  :int)
(cffi:defcfun ("wclrtoeol" %wclrtoeol) :int (win window))
