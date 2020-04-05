(in-package :de.anvi.ncurses)

;; keybound
;; return definition of keycode
;; http://invisible-island.net/ncurses/man/keybound.3x.html

;;; C prototypes

;; char *keybound(int keycode, int count);

;;; Low-level CFFI wrappers

(cffi:defcfun ("keybound" keybound) :string (keycode :int) (count :int))
