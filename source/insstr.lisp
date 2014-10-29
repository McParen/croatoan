(in-package :croatoan)

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

;;; Low-level C functions

(defcfun ("insstr"     %insstr)     :int                                (str :string))
(defcfun ("insnstr"    %insnstr)    :int                                (str :string) (n :int))
(defcfun ("winsstr"    %winsstr)    :int (win window)                   (str :string))
(defcfun ("winsnstr"   %winsnstr)   :int (win window)                   (str :string) (n :int))
(defcfun ("mvinsstr"   %mvinsstr)   :int              (y :int) (x :int) (str :string))
(defcfun ("mvinsnstr"  %mvinsnstr)  :int              (y :int) (x :int) (str :string) (n :int))
(defcfun ("mvwinsstr"  %mvwinsstr)  :int (win window) (y :int) (x :int) (str :string))
(defcfun ("mvwinsnstr" %mvwinsnstr) :int (win window) (y :int) (x :int) (str :string) (n :int))

;;; High-level Lisp wrappers

(defun insert-string (window string &key y x n)
  "Insert string at the current position in window.

Chars right of the cursor are moved to the right. The rightmost chars
on the line may be lost. The cursor position is not changed.

If n is given, insert n chars. If (<= n 0) insert the whole string.

If the destination coordinates y and x are given, move the cursor
there first."
  (cond ((and y x n)
         (%mvwinsnstr window y x string n))
        ((and y x)
         (%mvwinsstr window y x string))
        (n
         (%winsnstr window string n))
        (t
         (%winsstr window string))))

;;; NOTES

;;; TODOs
