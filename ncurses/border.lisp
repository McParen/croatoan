(in-package :de.anvi.ncurses)

;;; border
;;; create curses borders, horizontal and vertical lines
;;; http://invisible-island.net/ncurses/man/curs_border.3x.html
;;; http://linux.die.net/man/3/box

;;; C prototypes

;; int border(chtype ls, chtype rs, chtype ts, chtype bs, chtype tl, chtype tr, chtype bl, chtype br);
;; int wborder(WINDOW *win, chtype ls, chtype rs, chtype ts, chtype bs, chtype tl, chtype tr, chtype bl, chtype br);
;; int box(WINDOW *win, chtype verch, chtype horch);

;; int hline(chtype ch, int n);
;; int whline(WINDOW *win, chtype ch, int n);
;; int vline(chtype ch, int n);
;; int wvline(WINDOW *win, chtype ch, int n);

;; int mvhline(int y, int x, chtype ch, int n);
;; int mvwhline(WINDOW *, int y, int x, chtype ch, int n);
;; int mvvline(int y, int x, chtype ch, int n);
;; int mvwvline(WINDOW *, int y, int x, chtype ch, int n);

;;; Low-level CFFI wrappers

(cffi:defcfun ("border" border) :int
  (ls chtype)
  (rs chtype)
  (ts chtype)
  (bs chtype)
  (tl chtype)
  (tr chtype)
  (bl chtype)
  (br chtype))

(cffi:defcfun ("wborder" wborder) :int
  (win window)
  (ls chtype)
  (rs chtype)
  (ts chtype)
  (bs chtype)
  (tl chtype)
  (tr chtype)
  (bl chtype)
  (br chtype))

(cffi:defcfun ("box"       box)      :int (win window) (verch chtype) (horch chtype))

(cffi:defcfun ("hline"     hline)    :int              (ch chtype) (n :int))
(cffi:defcfun ("whline"    whline)   :int (win window) (ch chtype) (n :int))
(cffi:defcfun ("vline"     vline)    :int              (ch chtype) (n :int))
(cffi:defcfun ("wvline"    wvline)   :int (win window) (ch chtype) (n :int))

(cffi:defcfun ("mvhline"   mvhline)  :int              (y :int) (x :int) (ch chtype) (n :int))
(cffi:defcfun ("mvwhline"  mvwhline) :int (win window) (y :int) (x :int) (ch chtype) (n :int))
(cffi:defcfun ("mvvline"   mvvline)  :int              (y :int) (x :int) (ch chtype) (n :int))
(cffi:defcfun ("mvwvline"  mvwvline) :int (win window) (y :int) (x :int) (ch chtype) (n :int))
