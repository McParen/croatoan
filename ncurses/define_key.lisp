(in-package :de.anvi.ncurses)

;; define_key
;; define a keycode
;; http://invisible-island.net/ncurses/man/define_key.3x.html

;;; C prototypes

;; int define_key(const char *definition, int keycode);

;;; Low-level CFFI wrappers

(cffi:defcfun ("define_key" %define-key) :int (definition :string) (keycode :int))
