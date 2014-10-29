(in-package :de.anvi.ncurses)

;;; addch
;;; add a character (with attributes) to a curses window, then advance the cursor
;;; http://invisible-island.net/ncurses/man/curs_addch.3x.html

;;; C prototypes

;; int addch(const chtype ch);
;; int waddch(WINDOW *win, const chtype ch);
;; int mvaddch(int y, int x, const chtype ch);
;; int mvwaddch(WINDOW *win, int y, int x, const chtype ch);
;; int echochar(const chtype ch);
;; int wechochar(WINDOW *win, const chtype ch);

;;; Low-level CFFI wrappers

(defcfun ("addch"     %addch)     :int (ch chtype))
(defcfun ("waddch"    %waddch)    :int (win window) (ch chtype))

(defcfun ("mvaddch"   %mvaddch)   :int (y :int) (x :int) (ch chtype))
(defcfun ("mvwaddch"  %mvwaddch)  :int (win window) (y :int) (x :int) (ch chtype))

(defcfun ("echochar"  %echochar)  :int (ch chtype))
(defcfun ("wechochar" %wechochar) :int (win window) (ch chtype))
