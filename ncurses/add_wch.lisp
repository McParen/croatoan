(in-package :de.anvi.ncurses)

;;; add_wch
;;; add a complex character and rendition to a curses window, then advance the cursor
;;; http://invisible-island.net/ncurses/man/curs_add_wch.3x.html

;;; C prototypes

;;; int add_wch(const cchar_t *wch);
;;; int wadd_wch(WINDOW *win, const cchar_t *wch);
;;; int mvadd_wch(int y, int x, const cchar_t *wch);
;;; int mvwadd_wch( WINDOW *win, int y, int x, const cchar_t *wch);
;;; int echo_wchar(const cchar_t *wch);
;;; int wecho_wchar(WINDOW *win, const cchar_t *wch);

;;; Low-level CFFI wrappers

(defcfun ("add_wch"    %add-wch)    :int                                 (wch (:pointer (:struct cchar_t))))
(defcfun ("wadd_wch"   %wadd-wch)   :int  (win window)                   (wch (:pointer (:struct cchar_t))))

(defcfun ("mvadd_wch"  %mvadd-wch)  :int               (y :int) (x :int) (wch (:pointer (:struct cchar_t))))
(defcfun ("mvwadd_wch" %mvwadd-wch) :int  (win window) (y :int) (x :int) (wch (:pointer (:struct cchar_t))))

(defcfun ("echo_wch"   %echo-wch)   :int                                 (wch (:pointer (:struct cchar_t))))
(defcfun ("wecho_wch"  %wecho-wch)  :int  (win window)                   (wch (:pointer (:struct cchar_t))))
