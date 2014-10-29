(in-package :croatoan)

;;; insch
;;; insert a character before cursor in a curses window
;;; http://invisible-island.net/ncurses/man/curs_insch.3x.html

;;; C prototypes

;; int insch(chtype ch);
;; int winsch(WINDOW *win, chtype ch);
;; int mvinsch(int y, int x, chtype ch);
;; int mvwinsch(WINDOW *win, int y, int x, chtype ch);

;;; Low-level C functions

(defcfun ("insch"    %insch)   :int                                (ch chtype))
(defcfun ("winsch"   %winsch)  :int (win window)                   (ch chtype))
(defcfun ("mvinsch"  %mvinsch) :int              (y :int) (x :int) (ch chtype))
(defcfun ("mvwinsch" %winsch)  :int (win window) (y :int) (x :int) (ch chtype))

;;; High-level Lisp wrappers

(defun insert-char (window char &key y x)
  "Insert char into window before the character currently under the cursor.

Chars right of the cursor are moved one position to the right.
The rightmost character on the line may be lost. The position of the
cursor is not changed.

If the destination coordinates y and x are given, move the cursor
there first."
  (cond ((and y x)
         (%mvwinsch window y x))
        (t
         (%winsch window))))

;;; TODOs

;; [ ] Make it return an xchar instead of chtype.
