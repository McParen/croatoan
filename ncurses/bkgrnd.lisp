(in-package :de.anvi.ncurses)

;;; bkgrnd
;;; window complex background manipulation routines
;;; http://invisible-island.net/ncurses/man/curs_bkgrnd.3x.html

;;; C prototypes

;; int bkgrnd( const cchar_t *wch);
;; int wbkgrnd( WINDOW *win, const cchar_t *wch);
;; void bkgrndset(const cchar_t *wch );
;; void wbkgrndset(WINDOW *win, const cchar_t *wch);
;; int getbkgrnd(cchar_t *wch);
;; int wgetbkgrnd(WINDOW *win, cchar_t *wch);

;;; Low-level CFFI wrappers

(cffi:defcfun ("bkgrnd"     %bkgrnd)     :int               (wch (:pointer (:struct cchar_t))))
(cffi:defcfun ("wbkgrnd"    %wbkgrnd)    :int  (win window) (wch (:pointer (:struct cchar_t))))
(cffi:defcfun ("bkgrndset"  %bkgrndset)  :void              (wch (:pointer (:struct cchar_t))))
(cffi:defcfun ("wbkgrndset" %wbkgrndset) :void (win window) (wch (:pointer (:struct cchar_t))))
(cffi:defcfun ("getbkgrnd"  %getbkgrnd)  :int               (wch (:pointer (:struct cchar_t))))
(cffi:defcfun ("wgetbkgrnd" %wgetbkgrnd) :int  (win window) (wch (:pointer (:struct cchar_t))))
