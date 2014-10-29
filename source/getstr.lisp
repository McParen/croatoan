(in-package :croatoan)

;;; getstr
;;; accept character strings from curses terminal keyboard
;;; http://invisible-island.net/ncurses/man/curs_getstr.3x.html

;;; C prototypes

;; int getstr(char *str);
;; int getnstr(char *str, int n);
;; int wgetstr(WINDOW *win, char *str);
;; int wgetnstr(WINDOW *win, char *str, int n);
;; int mvgetstr(int y, int x, char *str);
;; int mvgetnstr(int y, int x, char *str, int n);
;; int mvwgetstr(WINDOW *win, int y, int x, char *str);
;; int mvwgetnstr(WINDOW *, int y, int x, char *str, int n);

;;; Low-level C functions

(defcfun ("getstr"     %getstr)     :int                                (str :string))
(defcfun ("getnstr"    %getnstr)    :int                                (str :string) (n :int))
(defcfun ("wgetstr"    %wgetstr)    :int (win window)                   (str :string))
(defcfun ("wgetnstr"   %wgetnstr)   :int (win window)                   (str :string) (n :int))
(defcfun ("mvgetstr"   %mvgetstr)   :int              (y :int) (x :int) (str :string))
(defcfun ("mvgetnstr"  %mvgetnstr)  :int              (y :int) (x :int) (str :string) (n :int))
(defcfun ("mvwgetstr"  %mvwgetstr)  :int (win window) (y :int) (x :int) (str :string))
(defcfun ("mvwgetnstr" %mvwgetnstr) :int (win window) (y :int) (x :int) (str :string) (n :int))

;;; High-level Lisp wrappers

(defun get-string (window string &key y x n)
  "Read a string from the keyboard and return it.

Reading is performed until a newline or carriage return is received.
The terminating character is not included in the returned string.

If n is given, read at most n chars, to prevent a possible input
buffer overflow.

If the destination coordinates y and x are given, move the cursor
there first."
  (cond ((and y x n)
         (%mvwgetnstr window y x string n))
        ((and y x)
         (%mvwgetstr window y x string))
        (n
         (%wgetnstr window string n))
        (t
         (%wgetstr window string))))

;;; NOTES

;;; TODOs
