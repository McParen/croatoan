(in-package :de.anvi.ncurses)

;;; inwstr
;;; extract a string of wchar_t characters from a curses window
;;; http://invisible-island.net/ncurses/man/curs_inwstr.3x.html

;;; C prototypes

;; int inwstr(wchar_t *str);
;; int innwstr(wchar_t *str, int n);
;; int winwstr(WINDOW *win, wchar_t *str);
;; int winnwstr(WINDOW *win, wchar_t *str, int n);
;; int mvinwstr(int y, int x, wchar_t *str);
;; int mvinnwstr(int y, int x, wchar_t *str, int n);
;; int mvwinwstr(WINDOW *win, int y, int x, wchar_t *str);
;; int mvwinnwstr(WINDOW *win, int y, int x, wchar_t *str, int n);

;;; Low-level CFFI wrappers

(cffi:defcfun ("inwstr"     %inwstr)     :int                                (str (:pointer wchar_t)))
(cffi:defcfun ("innwstr"    %innwstr)    :int                                (str (:pointer wchar_t)) (n :int))
(cffi:defcfun ("winwstr"    %winwstr)    :int (win window)                   (str (:pointer wchar_t)))
(cffi:defcfun ("winnwstr"   %winnwstr)   :int (win window)                   (str (:pointer wchar_t)) (n :int))
(cffi:defcfun ("mvinwstr"   %mvinwstr)   :int              (y :int) (x :int) (str (:pointer wchar_t)))
(cffi:defcfun ("mvinnwstr"  %mvinnwstr)  :int              (y :int) (x :int) (str (:pointer wchar_t)) (n :int))
(cffi:defcfun ("mvwinwstr"  %mvwinwstr)  :int (win window) (y :int) (x :int) (str (:pointer wchar_t)))
(cffi:defcfun ("mvwinnwstr" %mvwinnwstr) :int (win window) (y :int) (x :int) (str (:pointer wchar_t)) (n :int))
