(in-package :de.anvi.ncurses)

;;; new_pair
;;; new curses color-pair functions
;;; https://invisible-island.net/ncurses/man/new_pair.3x.html

;;; C prototypes

;; int alloc_pair(int fg, int bg);
;; int find_pair(int fg, int bg);
;; int free_pair(int pair);

;;; Low-level CFFI wrappers

(cffi:defcfun ("alloc_pair" alloc-pair) :int (fg :int) (bg :int))
(cffi:defcfun ("find_pair"  find-pair)  :int (fg :int) (bg :int))
(cffi:defcfun ("free_pair"  free-pair)  :int (pair :int))
