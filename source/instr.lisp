(in-package :croatoan)

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

;;; Low-level C functions

(defcfun ("instr"     %instr)     :int                                (str :string))
(defcfun ("innstr"    %innstr)    :int                                (str :string) (n :int))
(defcfun ("winstr"    %winstr)    :int (win window)                   (str :string))
(defcfun ("winnstr"   %winnstr)   :int (win window)                   (str :string) (n :int))
(defcfun ("mvinstr"   %mvinstr)   :int              (y :int) (x :int) (str :string))
(defcfun ("mvinnstr"  %mvinnstr)  :int              (y :int) (x :int) (str :string) (n :int))
(defcfun ("mvwinstr"  %mvwinstr)  :int (win window) (y :int) (x :int) (str :string))
(defcfun ("mvwinnstr" %mvwinnstr) :int (win window) (y :int) (x :int) (str :string) (n :int))

;;; High-level Lisp wrappers

(defun extract-string (window &key y x n)
  "Extract and return a string from window.

Any attributes are stripped from the characters before the string is
returned.

Start at the current cursor position and end at the right margin of
window. If n is given, read at most n chars. 

If the destination coordinates y and x are given, move the cursor to
the destination first."
  (with-foreign-pointer (string 200 len)
    ;; zero the result string.
    (setf (mem-ref string :char (1- len)) 0)
    ;; populate the foreign string with chars.
    ;; the c routines return ERR (-1) or the number of chars extracted.
    (let ((retval (cond ((and y x n)
                         (%mvwinnstr window y x string n))
                        ((and y x)
                         (%mvwinstr window y x string))
                        (n
                         (%winnstr window string n))
                        (t
                         (%winstr window string)))))
      (if (= retval -1)
          nil
          ;; convert the char pointer to a lisp string.
          (foreign-string-to-lisp string)))))

;;; NOTES

;;; TODOs

;; [ ] Reimplement completely in Lisp, using extract-char.

