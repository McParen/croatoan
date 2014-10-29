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

(defcfun ("def_prog_mode"    %def-prog-mode)    :int)
(defcfun ("def_shell_mode"   %def-shell-mode)   :int)
(defcfun ("reset_prog_mode"  %reset-prog-mode)  :int)
(defcfun ("reset_shell_mode" %reset-shell-mode) :int)
(defcfun ("resetty"          %resetty)          :int)
(defcfun ("savetty"          %savetty)          :int)
(defcfun ("getsyx"           %getsyx)           :void (y :int) (x :int))
(defcfun ("setsyx"           %setsyx)           :void (y :int) (x :int))
(defcfun ("ripoffline"       %ripoffline)       :int (line :int) (init :pointer))
(defcfun ("curs_set"         %curs-set)         :int (visibility :int))
(defcfun ("napms"            %napms)            :int (ms :int))
