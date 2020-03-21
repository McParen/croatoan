(in-package :de.anvi.ncurses)

;;; bkgd
;;; curses window background manipulation routines 
;;; http://invisible-island.net/ncurses/man/curs_bkgd.3x.html

;;; C prototypes

;; void bkgdset(chtype ch);
;; void wbkgdset(WINDOW *win, chtype ch);
;; int bkgd(chtype ch);
;; int wbkgd(WINDOW *win, chtype ch);
;; chtype getbkgd(WINDOW *win); 

;;; Low-level CFFI wrappers

(cffi:defcfun ("bkgdset"  %bkgdset)  :void (ch chtype))
(cffi:defcfun ("wbkgdset" %wbkgdset) :void (win window) (ch chtype))
(cffi:defcfun ("bkgd"     %bkgd)     :int  (ch chtype))
(cffi:defcfun ("wbkgd"    %wbkgd)    :int  (win window) (ch chtype))
(cffi:defcfun ("getbkgd"  %getbkgd)  chtype (win window))
