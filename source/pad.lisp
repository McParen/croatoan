(in-package :croatoan)

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

;;; Low-level C functions

(defcfun ("newpad" %newpad) window               (nlines :int) (ncols :int))
(defcfun ("subpad" %subpad) window (orig window) (nlines :int) (ncols :int) (begin_y :int) (begin_x :int))

(defcfun ("prefresh" %prefresh) :int 
  (pad     window)
  (pminrow :int)
  (pmincol :int)
  (sminrow :int) 
  (smincol :int) 
  (smaxrow :int)
  (smaxcol :int))

(defcfun ("pnoutrefresh" %pnoutrefresh) :int
  (pad     window)
  (pminrow :int)
  (pmincol :int)
  (sminrow :int)
  (smincol :int)
  (smaxrow :int)
  (smaxcol :int))

(defcfun ("pechochar" %pechochar) :int (pad window) (ch chtype))

;;; High-level Lisp wrappers

(defun new-pad (height width)
  (%newpad height width))

(defun new-subpad (parent-pad height width begin-y begin-x)
  (%subpad parent-pad height width begin-y begin-x))

(defun refresh-pad (pad-min-y pad-min-x screen-min-y screen-min-x screen-max-y screen-max-x)
  (%prefresh pad-min-y pad-min-x screen-min-y screen-min-x screen-max-y screen-max-x))

;; %doupdate has to be called afer this.
(defun no-output-refresh-pad (pad-min-y pad-min-x screen-min-y screen-min-x screen-max-y screen-max-x)
  (%pnoutrefresh pad-min-y pad-min-x screen-min-y screen-min-x screen-max-y screen-max-x))

;; combined addch+refresh if only a single char is output.
;; there is allegedly a performance gain compared to manual addch+refresh.
(defun pad-echo-char (pad char)
  (%pechochar pad char))

;;; TODOs
