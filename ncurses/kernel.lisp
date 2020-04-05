(in-package :de.anvi.ncurses)

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

;;; Low-level CFFI wrappers

(cffi:defcfun ("def_prog_mode"    def-prog-mode)    :int)
(cffi:defcfun ("def_shell_mode"   def-shell-mode)   :int)
(cffi:defcfun ("reset_prog_mode"  reset-prog-mode)  :int)
(cffi:defcfun ("reset_shell_mode" reset-shell-mode) :int)
(cffi:defcfun ("resetty"          resetty)          :int)
(cffi:defcfun ("savetty"          savetty)          :int)
(cffi:defcfun ("getsyx"           getsyx)           :void (y :int) (x :int))
(cffi:defcfun ("setsyx"           setsyx)           :void (y :int) (x :int))
(cffi:defcfun ("ripoffline"       ripoffline)       :int (line :int) (init :pointer))
(cffi:defcfun ("curs_set"         curs-set)         :int (visibility :int))
(cffi:defcfun ("napms"            napms)            :int (ms :int))
