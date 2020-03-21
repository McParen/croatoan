(in-package :de.anvi.ncurses)

;;; touch
;;; curses refresh control routines
;;; http://invisible-island.net/ncurses/man/curs_touch.3x.html
;;; http://www-01.ibm.com/support/knowledgecenter/ssw_aix_61/com.ibm.aix.basetrf2/touchwin.htm

;;; C prototypes

;; int touchwin(WINDOW *win);
;; int touchline(WINDOW *win, int start, int count);
;; int untouchwin(WINDOW *win);
;; int wtouchln(WINDOW *win, int y, int n, int changed);
;; bool is_linetouched(WINDOW *win, int line);
;; bool is_wintouched(WINDOW *win);

;;; Low-level CFFI wrappers

(cffi:defcfun ("touchwin"       %touchwin)       :int     (win window))
(cffi:defcfun ("touchline"      %touchline)      :int     (win window) (start :int) (count :int))
(cffi:defcfun ("untouchwin"     %untouchwin)     :int     (win window))
(cffi:defcfun ("wtouchln"       %wtouchln)       :int     (win window) (y :int) (n :int) (changed :int))

(cffi:defcfun ("is_linetouched" %is-linetouched) :boolean (win window) (line :int))
(cffi:defcfun ("is_wintouched"  %is-wintouched)  :boolean (win window))
