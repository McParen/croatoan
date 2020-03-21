(in-package :de.anvi.ncurses)

;;; addstr 
;;; add a string of characters to a curses window and advance cursor
;;; http://invisible-island.net/ncurses/man/curs_addstr.3x.html
;;; http://www.manpagez.com/man/3/curs_addstr/

;;; C prototypes

;; int addstr(const char *str);
;; int addnstr(const char *str, int n);
;; int waddstr(WINDOW *win, const char *str);
;; int waddnstr(WINDOW *win, const char *str, int n);
;; int mvaddstr(int y, int x, const char *str);
;; int mvaddnstr(int y, int x, const char *str, int n);
;; int mvwaddstr(WINDOW *win, int y, int x, const char *str);
;; int mvwaddnstr(WINDOW *win, int y, int x, const char *str, int n);

;;; Low-level CFFI wrappers

(cffi:defcfun ("addstr"     %addstr)     :int                                (str :string))
(cffi:defcfun ("addnstr"    %addnstr)    :int                                (str :string) (n :int))

(cffi:defcfun ("waddstr"    %waddstr)    :int (win window)                   (str :string))
(cffi:defcfun ("waddnstr"   %waddnstr)   :int (win window)                   (str :string) (n :int))

(cffi:defcfun ("mvaddstr"   %mvaddstr)   :int              (y :int) (x :int) (str :string))
(cffi:defcfun ("mvaddnstr"  %mvaddnstr)  :int              (y :int) (x :int) (str :string) (n :int))

(cffi:defcfun ("mvwaddstr"  %mvwaddstr)  :int (win window) (y :int) (x :int) (str :string))
(cffi:defcfun ("mvwaddnstr" %mvwaddnstr) :int (win window) (y :int) (x :int) (str :string) (n :int))
