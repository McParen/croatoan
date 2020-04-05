(in-package :de.anvi.ncurses)

;;; refresh
;;; refresh curses windows and lines
;;; http://www.manpagez.com/man/3/curs_refresh/

;;; C prototypes

;; int refresh(void);
;; int wrefresh(WINDOW *win);
;; int wnoutrefresh(WINDOW *win);
;; int doupdate(void);
;; int redrawwin(WINDOW *win);
;; int wredrawln(WINDOW *win, int beg_line, int num_lines);

;;; Low-level CFFI wrappers

(cffi:defcfun ("refresh"      refresh)      :int)
(cffi:defcfun ("wrefresh"     wrefresh)     :int (win window))
(cffi:defcfun ("wnoutrefresh" wnoutrefresh) :int (win window))
(cffi:defcfun ("doupdate"     doupdate)     :int)
(cffi:defcfun ("redrawwin"    redrawwin)    :int (win window))
(cffi:defcfun ("wredrawln"    wredrawln)    :int (win window) (beg-line :int) (num-lines :int))
