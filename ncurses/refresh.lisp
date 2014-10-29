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

(defcfun ("refresh"      %refresh)      :int)
(defcfun ("wrefresh"     %wrefresh)     :int (win window))
(defcfun ("wnoutrefresh" %wnoutrefresh) :int (win window))
(defcfun ("doupdate"     %doupdate)     :int)
(defcfun ("redrawwin"    %redrawwin)    :int (win window))
(defcfun ("wredrawln"    %wredrawln)    :int (win window) (beg-line :int) (num-lines :int))
