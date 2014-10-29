(in-package :croatoan)

;; keybound
;; return definition of keycode
;; http://invisible-island.net/ncurses/man/keybound.3x.html

;;; C prototypes

;; char *keybound(int keycode, int count);

;;; Low-level C functions

(defcfun ("keybound" %keybound) :string (keycode :int) (count :int))

;;; High-level Lisp wrappers

(defun key-description (code n)
  "Return the n-th description of key stored in the terminfo database."
  (%keybound code n))

;;; TODOs

;; [ ] dont return any numeric codes, do something with keywords.
;; [ ] somehow handle the -1 error case.
