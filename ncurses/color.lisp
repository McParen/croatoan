(in-package :de.anvi.ncurses)

;;; color
;;; curses color manipulation routines
;;; http://invisible-island.net/ncurses/man/curs_color.3x.html
;;; http://linux.die.net/man/3/init_color

;;; C prototypes

;; int start_color(void);

;; bool has_colors(void);
;; bool can_change_color(void);

;; int init_pair(short pair, short f, short b);
;; int init_color(short color, short r, short g, short b);
;; int pair_content(short pair, short *f, short *b);
;; int color_content(short color, short *r, short *g, short *b);

;; int init_extended_pair(int pair, int f, int b);
;; int init_extended_color(int color, int r, int g, int b);
;; int extended_pair_content(int pair, int *f, int *b);
;; int extended_color_content(int color, int *r, int *g, int *b);

;;; C macros

;; COLOR_PAIR(int n)
;; PAIR_NUMBER(attrs);

;;; Low-level CFFI wrappers

(defcfun ("start_color"      %start-color)      :int)

(defcfun ("has_colors"       %has-colors)       :boolean)
(defcfun ("can_change_color" %can-change-color) :boolean)

(defcfun ("init_pair"     %init-pair)     :int (pair  :short) (f :short) (b :short))
(defcfun ("init_color"    %init-color)    :int (color :short) (r :short) (g :short) (b :short))
(defcfun ("pair_content"  %pair-content)  :int (pair  :short) (f (:pointer :short)) (b (:pointer :short)))
(defcfun ("color_content" %color-content) :int (color :short) (r (:pointer :short)) (g (:pointer :short)) (b (:pointer :short)))

(defcfun ("init_extended_pair"     %init-extended-pair)     :int (pair  :int) (f :int) (b :int))
(defcfun ("init_extended_color"    %init-extended-color)    :int (color :int) (r :int) (g :int) (b :int))
(defcfun ("extended_pair_content"  %pair-extended-content)  :int (pair  :int) (f (:pointer :int)) (b (:pointer :int)))
(defcfun ("extended_color_content" %color-extended-content) :int (color :int) (r (:pointer :int)) (g (:pointer :int)) (b (:pointer :int)))

(defcfun ("COLOR_PAIR"  %color-pair)  :int (n :int))
(defcfun ("PAIR_NUMBER" %pair-number) :int (attrs :int))     

(defconstant %COLOR_BLACK   0)
(defconstant %COLOR_RED     1)
(defconstant %COLOR_GREEN   2)
(defconstant %COLOR_YELLOW  3)
(defconstant %COLOR_BLUE    4)
(defconstant %COLOR_MAGENTA 5)
(defconstant %COLOR_CYAN    6)
(defconstant %COLOR_WHITE   7)
