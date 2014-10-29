(in-package :croatoan)

;;; beep
;;; curses bell and screen flash routines
;;; http://invisible-island.net/ncurses/man/curs_beep.3x.html

;;; C prototypes

;; int beep(void);
;; int flash(void);

;;; Low-level C functions

(defcfun ("beep"  %beep)     :int)
(defcfun ("flash" %flash)    :int)

;;; High-level Lisp wrappers

(defun alert (&optional (type :beep))
  (case type
    (:beep (%beep))
    (:flash (%flash))
    (otherwise (error "An altert can be a :beep or a :flash"))))

;;; TODOs

;; [ ] Return values, errors.

