(in-package :de.anvi.ncurses)

;;; legacy coding
;;; http://invisible-island.net/ncurses/man/legacy_coding.3x.html

;;; C prototypes

;; int use_legacy_coding(int level);

;;; Low-level CFFI wrappers

(cffi:defcfun ("use_legacy_coding" %use-legacy-coding) :int (level :int))
