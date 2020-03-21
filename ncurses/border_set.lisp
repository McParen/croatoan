(in-package :de.anvi.ncurses)

;;; border_set
;;; create curses borders or lines using complex characters and renditions
;;; http://invisible-island.net/ncurses/man/curs_border_set.3x.html

;;; C prototypes

;; int border_set(const cchar_t *ls, const cchar_t *rs, const cchar_t *ts, const cchar_t *bs, const cchar_t *tl, const cchar_t *tr, const cchar_t *bl, const cchar_t *br);
;; int wborder_set(WINDOW *win, const cchar_t *ls, const cchar_t *rs, const cchar_t *ts, const cchar_t *bs, const cchar_t *tl, const cchar_t *tr, const cchar_t *bl, const cchar_t *br);

;; int box_set(WINDOW *win, const cchar_t *verch, const cchar_t *horch);

;; int hline_set(const cchar_t *wch, int n);
;; int whline_set(WINDOW *win, const cchar_t *wch, int n);
;; int mvhline_set(int y, int x, const cchar_t *wch, int n);
;; int mvwhline_set(WINDOW *win, int y, int x, const cchar_t *wch, int n);

;; int vline_set(const cchar_t *wch, int n);
;; int wvline_set(WINDOW *win, const cchar_t *wch, int n);
;; int mvvline_set(int y, int x, const cchar_t *wch, int n);
;; int mvwvline_set(WINDOW *win, int y, int x, const cchar_t *wch, int n);

;;; Low-level CFFI wrappers

(cffi:defcfun ("border_set" %border-set) :int 
  (ls (:pointer (:struct cchar_t))) 
  (rs (:pointer (:struct cchar_t))) 
  (ts (:pointer (:struct cchar_t))) 
  (bs (:pointer (:struct cchar_t)))
  (tl (:pointer (:struct cchar_t))) 
  (tr (:pointer (:struct cchar_t))) 
  (bl (:pointer (:struct cchar_t))) 
  (br (:pointer (:struct cchar_t))))

(cffi:defcfun ("wborder_set" %wborder-set) :int 
  (win window)
  (ls (:pointer (:struct cchar_t)))
  (rs (:pointer (:struct cchar_t)))
  (ts (:pointer (:struct cchar_t)))
  (bs (:pointer (:struct cchar_t)))
  (tl (:pointer (:struct cchar_t)))
  (tr (:pointer (:struct cchar_t)))
  (bl (:pointer (:struct cchar_t)))
  (br (:pointer (:struct cchar_t))))

(cffi:defcfun ("box_set" %box-set) :int
  (win window)
  (verch (:pointer (:struct cchar_t)))
  (horch (:pointer (:struct cchar_t))))

(cffi:defcfun ("hline_set"    %hline-set)    :int                                (wch (:pointer (:struct cchar_t))) (n :int))
(cffi:defcfun ("whline_set"   %whline-set)   :int (win window)                   (wch (:pointer (:struct cchar_t))) (n :int))
(cffi:defcfun ("mvhline_set"  %mvhline-set)  :int              (y :int) (x :int) (wch (:pointer (:struct cchar_t))) (n :int))
(cffi:defcfun ("mvwhline_set" %mvwhline-set) :int (win window) (y :int) (x :int) (wch (:pointer (:struct cchar_t))) (n :int))

(cffi:defcfun ("vline_set"    %vline-set)    :int                                (wch (:pointer (:struct cchar_t))) (n :int))
(cffi:defcfun ("wvline_set"   %wvline-set)   :int (win window)                   (wch (:pointer (:struct cchar_t))) (n :int))
(cffi:defcfun ("mvvline_set"  %mvvline-set)  :int              (y :int) (x :int) (wch (:pointer (:struct cchar_t))) (n :int))
(cffi:defcfun ("mvwvline_set" %mvwvline-set) :int (win window) (y :int) (x :int) (wch (:pointer (:struct cchar_t))) (n :int))
