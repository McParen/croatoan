(in-package :de.anvi.ncurses)

;;; insstr
;;; insert string before cursor in a curses window
;;; http://invisible-island.net/ncurses/man/curs_insstr.3x.html

;;; C prototypes

;; int insstr(const char *str);
;; int insnstr(const char *str, int n);
;; int winsstr(WINDOW *win, const char *str);
;; int winsnstr(WINDOW *win, const char *str, int n);
;; int mvinsstr(int y, int x, const char *str);
;; int mvinsnstr(int y, int x, const char *str, int n);
;; int mvwinsstr(WINDOW *win, int y, int x, const char *str);
;; int mvwinsnstr(WINDOW *win, int y, int x, const char *str, int n);

;;; Low-level CFFI wrappers

(defcfun ("insstr"     %insstr)     :int                                (str :string))
(defcfun ("insnstr"    %insnstr)    :int                                (str :string) (n :int))
(defcfun ("winsstr"    %winsstr)    :int (win window)                   (str :string))
(defcfun ("winsnstr"   %winsnstr)   :int (win window)                   (str :string) (n :int))
(defcfun ("mvinsstr"   %mvinsstr)   :int              (y :int) (x :int) (str :string))
(defcfun ("mvinsnstr"  %mvinsnstr)  :int              (y :int) (x :int) (str :string) (n :int))
(defcfun ("mvwinsstr"  %mvwinsstr)  :int (win window) (y :int) (x :int) (str :string))
(defcfun ("mvwinsnstr" %mvwinsnstr) :int (win window) (y :int) (x :int) (str :string) (n :int))
