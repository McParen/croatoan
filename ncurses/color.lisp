(in-package :de.anvi.ncurses)

;;; color
;;; curses color manipulation routines
;;; http://invisible-island.net/ncurses/man/curs_color.3x.html
;;; http://linux.die.net/man/3/init_color

;;; C prototypes

;; int start_color(void);
;; int init_pair(short pair, short f, short b);
;; int init_color(short color, short r, short g, short b);
;; bool has_colors(void);
;; bool can_change_color(void);
;; int color_content(short color, short *r, short *g, short *b);
;; int pair_content(short pair, short *f, short *b);

;;; C macros

;; COLOR_PAIR(int n)

;;; Low-level CFFI wrappers

(defcfun ("start_color"      %start-color)      :int)
(defcfun ("init_pair"        %init-pair)        :int (pair :short) (f :short) (b :short))
(defcfun ("init_color"       %init-color)       :int (color :short) (r :short) (g :short) (b :short))
(defcfun ("has_colors"       %has-colors)       :boolean)
(defcfun ("can_change_color" %can-change-color) :boolean)
(defcfun ("color_content"    %color-content)    :int (color :short) (r (:pointer :short)) (g (:pointer :short)) (b (:pointer :short)))
(defcfun ("pair_content"     %pair-content)     :int (pair :short) (f (:pointer :short)) (b (:pointer :short)))
(defcfun ("COLOR_PAIR"       %color-pair)       :int (n :int))

(defconstant %COLOR_BLACK   0)
(defconstant %COLOR_RED     1)
(defconstant %COLOR_GREEN   2)
(defconstant %COLOR_YELLOW  3)
(defconstant %COLOR_BLUE    4)
(defconstant %COLOR_MAGENTA 5)
(defconstant %COLOR_CYAN    6)
(defconstant %COLOR_WHITE   7)
