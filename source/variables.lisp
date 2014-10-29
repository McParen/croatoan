(in-package :croatoan)

;;; variables
;;; curses global variables
;;; http://invisible-island.net/ncurses/man/curs_variables.3x.html
;;; http://h71000.www7.hp.com/doc/83final/5763/5763pro_016.html

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

(defcvar ("COLOR_PAIRS" +color-pairs+    :read-only t) :int 
  "Number of color pairs which the terminal can support.")

(defcvar ("COLORS"      +colors+         :read-only t) :int 
  "Number of colors which the terminal can support.")

(defcvar ("COLS"        +screen-columns+ :read-only t) :int 
  "Width of the screen, the number of columns.")

(defcvar ("ESCDELAY"    +esc-delay+      :read-only t) :int 
  "Number of miliseconds to wait after reading an escape character,
to distinguish between an individual escape character entered on the
keyboard from escape sequences sent by cursor- and function-keys.")

(defcvar ("LINES"       +screen-lines+   :read-only t) :int
  "Height of the screen, the number of lines.")

(defcvar ("TABSIZE"     +tab-size+       :read-only t) :int
  "Number of columns to convert a tab character to spaces when
displaying the tab in a window.")

#|

curscr is the contents of the physical display screen, so it naturally
includes the ripped-off lines.

6.3.1 Predefined Windows (stdscr and curscr)

Initially, two windows the size of the terminal screen are predefined
by Curses. These windows are called stdscr and curscr . The stdscr
window is defined for your use. Many Curses macros default to this
window.

The second predefined window, curscr , is designed for internal Curses
work; it is an image of what is currently displayed on the terminal
screen. The only HP C for OpenVMS Curses function that will accept
this window as an argument is clearok . Do not write to or read from
curscr . Use stdscr and user-defined windows for all your Curses
applications.

|#
