(in-package :de.anvi.ncurses)

;; keyok
;; enable or disable a keycode
;; https://invisible-island.net/ncurses/man/keyok.3x.html

;;; C prototypes

;; int keyok(int keycode, bool enable);

;;; Low-level CFFI wrappers

(cffi:defcfun ("keyok" keyok) :int (keycode :int) (enable :boolean))
