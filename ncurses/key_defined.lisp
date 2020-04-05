(in-package :de.anvi.ncurses)

;; key_defined
;; check if a keycode is defined
;; http://invisible-island.net/ncurses/man/key_defined.3x.html

;;; C prototypes

;; int key_defined(const char *definition);

;;; Low-level CFFI wrappers

(cffi:defcfun ("key_defined" key-defined) :int (definition :string))
