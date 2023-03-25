(in-package :de.anvi.ncurses)

;;; variables
;;; curses global variables
;;; http://invisible-island.net/ncurses/man/curs_variables.3x.html

;;; Low-level C global variables

;; int COLOR_PAIRS;
;; int COLORS;
;; int COLS;
;; int ESCDELAY;
;; int LINES;
;; int TABSIZE;
;; WINDOW * curscr;
;; WINDOW * newscr;
;; WINDOW * stdscr;

;;; Lisp read-only global constants.

(cffi:defcvar ("COLOR_PAIRS" COLOR-PAIRS :read-only t) :int)
(cffi:defcvar ("COLORS"      COLORS      :read-only t) :int)
(cffi:defcvar ("COLS"        COLS        :read-only t) :int)
(cffi:defcvar ("ESCDELAY"    ESCDELAY    :read-only t) :int)
(cffi:defcvar ("LINES"       LINES       :read-only t) :int)
(cffi:defcvar ("TABSIZE"     TABSIZE     :read-only t) :int)

(cffi:defcvar ("curscr" curscr :read-only t) window)
(cffi:defcvar ("newscr" newscr :read-only t) window)
(cffi:defcvar ("stdscr" stdscr :read-only t) window)

#|

http://h71000.www7.hp.com/doc/83final/5763/5763pro_016.html

curscr is the contents of the physical display screen, so it naturally
includes the ripped-off lines.

6.3.1 Predefined Windows (stdscr and curscr)

Initially, two windows the size of the terminal screen are predefined
by Curses. These windows are called stdscr and curscr . The stdscr
window is defined for your use. Many Curses macros default to this
window.

The second predefined window, curscr, is designed for internal Curses
work; it is an image of what is currently displayed on the terminal
screen. The only HP C for OpenVMS Curses function that will accept
this window as an argument is clearok. Do not write to or read from
curscr. Use stdscr and user-defined windows for all your Curses
applications.

|#
