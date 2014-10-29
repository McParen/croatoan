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

(defcfun ("bkgdset"  %bkgdset)  :void (ch chtype))
(defcfun ("wbkgdset" %wbkgdset) :void (win window) (ch chtype))
(defcfun ("bkgd"     %bkgd)     :int  (ch chtype))
(defcfun ("wbkgd"    %wbkgd)    :int  (win window) (ch chtype))
(defcfun ("getbkgd"  %getbkgd)  chtype (win window))
