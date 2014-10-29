(in-package :de.anvi.ncurses)

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

(defcvar ("COLOR_PAIRS" %COLOR-PAIRS :read-only t) :int)
(defcvar ("COLORS"      %COLORS      :read-only t) :int)
(defcvar ("COLS"        %COLS        :read-only t) :int)
(defcvar ("ESCDELAY"    %ESCDELAY    :read-only t) :int)
(defcvar ("LINES"       %LINES       :read-only t) :int)
(defcvar ("TABSIZE"     %TABSIZE     :read-only t) :int)
