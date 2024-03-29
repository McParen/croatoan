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

;; void reset_color_pairs(void);

;;; C macros

;; COLOR_PAIR(int n)
;; PAIR_NUMBER(attrs);

;;; Low-level CFFI wrappers

(cffi:defcfun ("start_color"      start-color)      :int)
(cffi:defcfun ("has_colors"       has-colors)       :boolean)
(cffi:defcfun ("can_change_color" can-change-color) :boolean)

(cffi:defcfun ("init_pair"     init-pair)     :int (pair  :short) (f :short) (b :short))
(cffi:defcfun ("init_color"    init-color)    :int (color :short) (r :short) (g :short) (b :short))
(cffi:defcfun ("pair_content"  pair-content)  :int (pair  :short) (f (:pointer :short)) (b (:pointer :short)))
(cffi:defcfun ("color_content" color-content) :int (color :short) (r (:pointer :short)) (g (:pointer :short)) (b (:pointer :short)))

(cffi:defcfun ("init_extended_pair"     init-extended-pair)     :int (pair  :int) (f :int) (b :int))
(cffi:defcfun ("init_extended_color"    init-extended-color)    :int (color :int) (r :int) (g :int) (b :int))
(cffi:defcfun ("extended_pair_content"  extended-pair-content)  :int (pair  :int) (f (:pointer :int)) (b (:pointer :int)))
(cffi:defcfun ("extended_color_content" extended-color-content) :int (color :int) (r (:pointer :int)) (g (:pointer :int)) (b (:pointer :int)))

(cffi:defcfun ("reset_color_pairs"      reset-color-pairs) :void)

(cffi:defcfun ("COLOR_PAIR"  color-pair)  :int (n :int))
(cffi:defcfun ("PAIR_NUMBER" pair-number) :int (attrs :int))

(defconstant +COLOR-BLACK+   0)
(defconstant +COLOR-RED+     1)
(defconstant +COLOR-GREEN+   2)
(defconstant +COLOR-YELLOW+  3)
(defconstant +COLOR-BLUE+    4)
(defconstant +COLOR-MAGENTA+ 5)
(defconstant +COLOR-CYAN+    6)
(defconstant +COLOR-WHITE+   7)
