(in-package :de.anvi.ncurses)

;;; ins_wch
;;; insert a complex character and rendition into a window
;;; http://invisible-island.net/ncurses/man/curs_ins_wch.3x.html

;;; C prototypes

;; int ins_wch(const cchar_t *wch);
;; int wins_wch(WINDOW *win, const cchar_t *wch);
;; int mvins_wch(int y, int x, const cchar_t *wch);
;; int mvwins_wch(WINDOW *win, int y, int x, const cchar_t *wch);

;;; Low-level CFFI wrappers

(cffi:defcfun ("ins_wch"    ins-wch)    :int                                (wch (:pointer (:struct cchar_t))))
(cffi:defcfun ("wins_wch"   wins-wch)   :int (win window)                   (wch (:pointer (:struct cchar_t))))
(cffi:defcfun ("mvins_wch"  mvins-wch)  :int              (y :int) (x :int) (wch (:pointer (:struct cchar_t))))
(cffi:defcfun ("mvwins_wch" mvwins-wch) :int (win window) (y :int) (x :int) (wch (:pointer (:struct cchar_t))))
