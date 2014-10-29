(in-package :de.anvi.croatoan)

;;; kernel
;;; low-level curses routines
;;; http://invisible-island.net/ncurses/man/curs_kernel.3x.html

;;; C prototypes

;; int def_prog_mode(void);
;; int def_shell_mode(void);
;; int reset_prog_mode(void);
;; int reset_shell_mode(void);
;; int resetty(void);
;; int savetty(void);
;; void getsyx(int y, int x);
;; void setsyx(int y, int x);
;; int ripoffline(int line, int (*init)(WINDOW *, int));
;; int curs_set(int visibility);
;; int napms(int ms);

;;; Low-level C functions

;; (defcfun ("def_prog_mode"    %def-prog-mode)    :int)
;; (defcfun ("def_shell_mode"   %def-shell-mode)   :int)
;; (defcfun ("reset_prog_mode"  %reset-prog-mode)  :int)
;; (defcfun ("reset_shell_mode" %reset-shell-mode) :int)
;; (defcfun ("resetty"          %resetty)          :int)
;; (defcfun ("savetty"          %savetty)          :int)
;; (defcfun ("getsyx"           %getsyx)           :void (y :int) (x :int))
;; (defcfun ("setsyx"           %setsyx)           :void (y :int) (x :int))
;; (defcfun ("ripoffline"       %ripoffline)       :int (line :int) (init :pointer))
;; (defcfun ("curs_set"         %curs-set)         :int (visibility :int))
;; (defcfun ("napms"            %napms)            :int (ms :int))

;;; High-level Lisp wrappers

;; 0 invisible, 1 visible, 2 very visible.
(defun set-cursor-visibility (status)
  (case status
    ((nil :invisible)    (%curs-set 0))
    ((t :visible)      (%curs-set 1))
    (:very-visible (%curs-set 2))
    (otherwise (error "`set-cursor-visibility' only accepts nil or t or :very-visible as arguments."))))

;;; TODOs

;; [ ] no idea how to declare a function pointer correctly in "ripoffline".

