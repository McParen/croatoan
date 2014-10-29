(in-package :croatoan)

;;; inch
;;; get a character and attributes from a curses window
;;; http://invisible-island.net/ncurses/man/curs_inch.3x.html

;;; C prototypes

;; chtype inch(void);
;; chtype winch(WINDOW *win);
;; chtype mvinch(int y, int x);
;; chtype mvwinch(WINDOW *win, int y, int x);

;;; Low-level C functions

(defcfun ("inch"    %inch)   chtype)
(defcfun ("winch"   %winch)  chtype (win window))
(defcfun ("mvinch"  %mvinch) chtype              (y :int) (x :int))
(defcfun ("mvwinch" %winch)  chtype (win window) (y :int) (x :int))

;;; High-level Lisp wrappers

(defun extract-char (window &key y x)
  "Extract and return the rendered char from the current position in window.

If the destination coordinates y and x are given, move the cursor
there first."
  (cond ((and y x)
         (%mvwinch window y x))
        (t
         (%winch window))))

;;; TODOs

;; [ ] Make it return an xchar instead of chtype.
