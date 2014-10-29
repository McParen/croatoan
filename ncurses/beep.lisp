(in-package :de.anvi.ncurses)

;;; beep
;;; curses bell and screen flash routines
;;; http://invisible-island.net/ncurses/man/curs_beep.3x.html

;;; C prototypes

;; int beep(void);
;; int flash(void);

;;; Low-level CFFI wrappers

(defcfun ("beep"  %beep)  :int)
(defcfun ("flash" %flash) :int)
