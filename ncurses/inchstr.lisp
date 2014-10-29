(in-package :de.anvi.ncurses)

;;; inchstr
;;; get a string of characters (and attributes) from a curses window
;;; http://invisible-island.net/ncurses/man/curs_inchstr.3x.html

;;; C prototypes

;; int inchstr(chtype *chstr);
;; int inchnstr(chtype *chstr, int n);
;; int winchstr(WINDOW *win, chtype *chstr);
;; int winchnstr(WINDOW *win, chtype *chstr, int n);
;; int mvinchstr(int y, int x, chtype *chstr);
;; int mvinchnstr(int y, int x, chtype *chstr, int n);
;; int mvwinchstr(WINDOW *win, int y, int x, chtype *chstr);
;; int mvwinchnstr(WINDOW *win, int y, int x, chtype *chstr, int n);

;;; Low-level CFFI wrappers

(defcfun ("inchstr"     %inchstr)     :int                                (chstr (:pointer chtype)))
(defcfun ("inchnstr"    %inchnstr)    :int                                (chstr (:pointer chtype)) (n :int))
(defcfun ("winchstr"    %winchstr)    :int (win window)                   (chstr (:pointer chtype)))
(defcfun ("winchnstr"   %winchnstr)   :int (win window)                   (chstr (:pointer chtype)) (n :int))
(defcfun ("mvinchstr"   %mvinchstr)   :int              (y :int) (x :int) (chstr (:pointer chtype)))
(defcfun ("mvinchnstr"  %mvinchnstr)  :int              (y :int) (x :int) (chstr (:pointer chtype)) (n :int))
(defcfun ("mvwinchstr"  %mvwinchstr)  :int (win window) (y :int) (x :int) (chstr (:pointer chtype)))
(defcfun ("mvwinchnstr" %mvwinchnstr) :int (win window) (y :int) (x :int) (chstr (:pointer chtype)) (n :int))
