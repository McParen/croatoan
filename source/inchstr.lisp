(in-package :croatoan)

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

;;; Low-level C functions

(defcfun ("inchstr"     %inchstr)     :int                                (chstr (:pointer chtype)))
(defcfun ("inchnstr"    %inchnstr)    :int                                (chstr (:pointer chtype)) (n :int))
(defcfun ("winchstr"    %winchstr)    :int (win window)                   (chstr (:pointer chtype)))
(defcfun ("winchnstr"   %winchnstr)   :int (win window)                   (chstr (:pointer chtype)) (n :int))
(defcfun ("mvinchstr"   %mvinchstr)   :int              (y :int) (x :int) (chstr (:pointer chtype)))
(defcfun ("mvinchnstr"  %mvinchnstr)  :int              (y :int) (x :int) (chstr (:pointer chtype)) (n :int))
(defcfun ("mvwinchstr"  %mvwinchstr)  :int (win window) (y :int) (x :int) (chstr (:pointer chtype)))
(defcfun ("mvwinchnstr" %mvwinchnstr) :int (win window) (y :int) (x :int) (chstr (:pointer chtype)) (n :int))

;;; High-level Lisp wrappers

(defun extract-rendered-string (window &key y x n)
  "Extract and return a list of rendered chars from window.

Start at the current cursor position and end at the right margin of
window. If n is given, read at most n chars.

If the destination coordinates y and x are given, move the cursor
to the destination first."
  (with-foreign-object (array chtype 200)
    ;; zero the result string.
    (setf (mem-ref array chtype 200) 0)
    ;; populate the foreign string with chars.
    ;; the c routines return ERR (-1) or the number of chars extracted.
    (let ((retval (cond ((and y x n)
                         (%mvwinnstr window y x array n))
                        ((and y x)
                         (%mvwinstr window y x array))
                        (n
                         (%winnstr window array n))
                        (t
                         (%winstr window array)))))
      (if (= retval -1)
          nil
          ;; collect the chtypes from the array into a list to return.
          (loop for i from 0 below 200
                collect (mem-ref array chtype i))))))

;;; NOTES

;;; TODOs

;; [ ] Reimplement this completely in Lisp, using extract-char. Extract chtypes till NULL or n.
;; [ ] Reimplement to return xchars instead of chtypes.
