(in-package :de.anvi.ncurses)

;; default_colors
;; use terminal's default colors
;; http://invisible-island.net/ncurses/man/default_colors.3x.html

;;; C prototypes

;; int use_default_colors(void);
;; int assume_default_colors(int fg, int bg);

;;; Low-level CFFI wrappers

(cffi:defcfun ("use_default_colors"    %use-default-colors)    :int)
(cffi:defcfun ("assume_default_colors" %assume-default-colors) :int (fg :int) (bg :int))
