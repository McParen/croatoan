(in-package :de.anvi.ncurses)

;;; in_wch
;;; extract a wide (multi-byte) character and rendition from a window
;;; http://invisible-island.net/ncurses/man/curs_in_wch.3x.html

;;; C prototypes

;; int in_wch(cchar_t *wcval);
;; int mvin_wch(int y, int x, cchar_t *wcval);
;; int win_wch(WINDOW *win, cchar_t *wcval);
;; int mvwin_wch(WINDOW *win, int y, int x, cchar_t *wcval);

;;; Low-level CFFI wrappers

(defcfun ("in_wch"    %in-wch)    :int                                (wcval (:pointer (:struct cchar_t))))
(defcfun ("mvin_wch"  %mvin-wch)  :int              (y :int) (x :int) (wcval (:pointer (:struct cchar_t))))
(defcfun ("win_wch"   %win-wch)   :int (win window)                   (wcval (:pointer (:struct cchar_t))))
(defcfun ("mvwin_wch" %mvwin-wch) :int (win window) (y :int) (x :int) (wcval (:pointer (:struct cchar_t))))
