(in-package :croatoan)

;; key_defined
;; check if a keycode is defined
;; http://invisible-island.net/ncurses/man/key_defined.3x.html

;;; C prototypes

;; int key_defined(const char *definition);

;;; Low-level C functions

(defcfun ("key_defined" %key-defined) :int (definition :string))

;;; High-level Lisp wrappers

(defun key-defined-p (key-name)
  "If keycode is defined, return the keycode."
  (let ((retval (%key-defined key-name)))
    (cond ((= retval 0) nil)
          ((= retval -1) nil)
          (t retval))))

;;; TODOs

;; [ ] dont return any numeric codes, do something with keywords.
;; [ ] somehow handle the -1 error case.
