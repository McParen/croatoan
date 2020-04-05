(in-package :de.anvi.ncurses)

;;; pad
;;; create and display curses pads
;;; http://invisible-island.net/ncurses/man/curs_pad.3x.html
;;; http://pic.dhe.ibm.com/infocenter/aix/v7r1/topic/com.ibm.aix.basetechref/doc/basetrf2/newpad.htm
;;; http://www.gnu.org/software/guile-ncurses/manual/html_node/Create-and-display-pads.html

;;; C prototypes

;; WINDOW *newpad(int nlines, int ncols);
;; WINDOW *subpad(WINDOW *orig, int nlines, int ncols, int begin_y, int begin_x);

;; int prefresh(WINDOW *pad, int pminrow, int pmincol, int sminrow, int smincol, int smaxrow, int smaxcol);
;; int pnoutrefresh(WINDOW *pad, int pminrow, int pmincol, int sminrow, int smincol, int smaxrow, int smaxcol);

;; int pechochar(WINDOW *pad, chtype ch);
;; int pecho_wchar(WINDOW *pad, const cchar_t *wch);

;;; Low-level CFFI wrappers

(cffi:defcfun ("newpad" newpad) window               (nlines :int) (ncols :int))
(cffi:defcfun ("subpad" subpad) window (orig window) (nlines :int) (ncols :int) (begin_y :int) (begin_x :int))

(cffi:defcfun ("prefresh" prefresh) :int 
  (pad     window)
  (pminrow :int)
  (pmincol :int)
  (sminrow :int) 
  (smincol :int) 
  (smaxrow :int)
  (smaxcol :int))

(cffi:defcfun ("pnoutrefresh" pnoutrefresh) :int
  (pad     window)
  (pminrow :int)
  (pmincol :int)
  (sminrow :int)
  (smincol :int)
  (smaxrow :int)
  (smaxcol :int))

(cffi:defcfun ("pechochar"   pechochar)   :int (pad window) (ch chtype))
(cffi:defcfun ("pecho_wchar" pecho-wchar) :int (pad window) (wch (:pointer (:struct cchar_t))))
