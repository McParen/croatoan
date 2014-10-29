(in-package :croatoan)

;; default_colors
;; use terminal's default colors
;; http://invisible-island.net/ncurses/man/default_colors.3x.html

;;; C prototypes

;; int use_default_colors(void);
;; int assume_default_colors(int fg, int bg);

;;; Low-level C functions

(defcfun ("use_default_colors" %use-default-colors) :int)
(defcfun ("assume_default_colors" %assume-default-colors) :int (fg :int) (bg :int))

;;; High-level Lisp wrappers

(defun use-default-colors (flag)
  "Assign the terminal default colors to the color number -1."
  (when flag
    (%use-default-colors)))

(defun assume-default-colors (fg bg)
  "Modify the default color pair 0 to use the color numbers fg and bg.

Ncurses otherwise will use white on black."
  (%assume-default-colors fg bg))

;;; NOTICE
