(in-package :croatoan)

;; define_key
;; define a keycode
;; http://invisible-island.net/ncurses/man/define_key.3x.html

;;; C prototypes

;; int define_key(const char *definition, int keycode);

;;; Low-level C functions

(defcfun ("define_key" %define-key) :int (definition :string) (keycode :int))

;;; High-level Lisp wrappers

(defun define-key (definition code)
  "Define a new keycode with its definition string.

If the string is empty, or the code zero or negative, the existing
definition is removed."
  (%define-key definition code))

;;; TODOs
