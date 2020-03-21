(in-package :de.anvi.ncurses)

;;; instr
;;; get a string of characters from a curses window
;;; http://invisible-island.net/ncurses/man/curs_instr.3x.html

;;; C prototypes

;; int instr(char *str);
;; int innstr(char *str, int n);
;; int winstr(WINDOW *win, char *str);
;; int winnstr(WINDOW *win, char *str, int n);
;; int mvinstr(int y, int x, char *str);
;; int mvinnstr(int y, int x, char *str, int n);
;; int mvwinstr(WINDOW *win, int y, int x, char *str);
;; int mvwinnstr(WINDOW *win, int y, int x, char *str, int n);

;;; Low-level CFFI wrappers

(cffi:defcfun ("instr"     %instr)     :int                                (str :string))
(cffi:defcfun ("innstr"    %innstr)    :int                                (str :string) (n :int))
(cffi:defcfun ("winstr"    %winstr)    :int (win window)                   (str :string))
(cffi:defcfun ("winnstr"   %winnstr)   :int (win window)                   (str :string) (n :int))
(cffi:defcfun ("mvinstr"   %mvinstr)   :int              (y :int) (x :int) (str :string))
(cffi:defcfun ("mvinnstr"  %mvinnstr)  :int              (y :int) (x :int) (str :string) (n :int))
(cffi:defcfun ("mvwinstr"  %mvwinstr)  :int (win window) (y :int) (x :int) (str :string))
(cffi:defcfun ("mvwinnstr" %mvwinnstr) :int (win window) (y :int) (x :int) (str :string) (n :int))
