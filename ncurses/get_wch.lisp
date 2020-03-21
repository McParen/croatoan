(in-package :de.anvi.ncurses)

;;; get_wch
;;; get (or push back) a wide (multi-byte) character from curses terminal keyboard
;;; http://invisible-island.net/ncurses/man/curs_get_wch.3x.html

;;; C prototypes

;; int get_wch(wint_t *wch);
;; int wget_wch(WINDOW *win, wint_t *wch);
;; int mvget_wch(int y, int x, wint_t *wch);
;; int mvwget_wch(WINDOW *win, int y, int x, wint_t *wch);
;; int unget_wch(const wchar_t wch);

;;; Low-level CFFI wrappers

(cffi:defcfun ("get_wch"    %get-wch)    :int                                (wch (:pointer wint_t)))
(cffi:defcfun ("wget_wch"   %wget-wch)   :int (win window)                   (wch (:pointer wint_t)))
(cffi:defcfun ("mvget_wch"  %mvget-wch)  :int              (y :int) (x :int) (wch (:pointer wint_t)))
(cffi:defcfun ("mvwget_wch" %mvwget-wch) :int (win window) (y :int) (x :int) (wch (:pointer wint_t)))
(cffi:defcfun ("unget_wch"  %unget-wch)  :int (wch wchar_t))
