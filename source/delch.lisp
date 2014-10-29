(in-package :croatoan)

;;; delch
;;; delete character under the cursor in a curses window
;;; http://invisible-island.net/ncurses/man/curs_delch.3x.html

;;; C prototypes

;; int delch(void);
;; int wdelch(WINDOW *win);
;; int mvdelch(int y, int x);
;; int mvwdelch(WINDOW *win, int y, int x);

;;; Low-level C functions

(defcfun ("delch"     %delch)     :int)
(defcfun ("wdelch"    %wdelch)    :int (win window))
(defcfun ("mvdelch"   %mvdelch)   :int (y :int) (x :int))
(defcfun ("mvwdelch"  %mvwdelch)  :int (win window) (y :int) (x :int))

;;; High-level Lisp wrappers

(defun delete-char (window &key y x)
  "Delete the character under the cursor. 

All characters to the right of the cursor on the same line are moved
to the left one position and the last character on the line is filled
with a blank. The cursor position does not change after moving
to (y,x), if specified."
  (cond ((and y x)
         (%mvwdelch win y x))
        (t
         (%wdelch win))))

;;; TODOs

