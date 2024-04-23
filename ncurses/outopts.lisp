(in-package :de.anvi.ncurses)

;;; outopts
;;; curses output options
;;; http://invisible-island.net/ncurses/man/curs_outopts.3x.html

;;; C prototypes

;; int clearok(WINDOW *win, bool bf);
;; int idlok(WINDOW *win, bool bf);
;; void idcok(WINDOW *win, bool bf);
;; void immedok(WINDOW *win, bool bf);
;; int leaveok(WINDOW *win, bool bf);
;; int scrollok(WINDOW *win, bool bf);

;; int setscrreg(int top, int bot);
;; int wsetscrreg(WINDOW *win, int top, int bot);

;;; Low-level CFFI wrappers

(cffi:defcfun ("clearok"    clearok)    :int  (win window) (bf :boolean))
(cffi:defcfun ("idlok"      idlok)      :int  (win window) (bf :boolean))
(cffi:defcfun ("idcok"      idcok)      :void (win window) (bf :boolean))
(cffi:defcfun ("immedok"    immedok)    :void (win window) (bf :boolean))
(cffi:defcfun ("leaveok"    leaveok)    :int  (win window) (bf :boolean))
(cffi:defcfun ("scrollok"   scrollok)   :int  (win window) (bf :boolean))

(cffi:defcfun ("setscrreg"  setscrreg)  :int               (top :int) (bot :int))
(cffi:defcfun ("wsetscrreg" wsetscrreg) :int  (win window) (top :int) (bot :int))
