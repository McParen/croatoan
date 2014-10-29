(in-package :de.anvi.ncurses)

;;; clear
;;; clear all or part of a curses window
;;; http://invisible-island.net/ncurses/man/curs_clear.3x.html
;;; http://linux.die.net/man/3/erase

;;; C prototypes

;; int erase(void);
;; int werase(WINDOW *win);
;; int clear(void);
;; int wclear(WINDOW *win);
;; int clrtobot(void);
;; int wclrtobot(WINDOW *win);
;; int clrtoeol(void);
;; int wclrtoeol(WINDOW *win);

;;; Low-level CFFI wrappers

(defcfun ("erase"     %erase)     :int)
(defcfun ("werase"    %werase)    :int (win window))
(defcfun ("clear"     %clear)     :int)
(defcfun ("wclear"    %wclear)    :int (win window))

(defcfun ("clrtobot"  %clrtobot)  :int)
(defcfun ("wclrtobot" %wclrtobot) :int (win window))
(defcfun ("clrtoeol"  %clrtoeol)  :int)
(defcfun ("wclrtoeol" %wclrtoeol) :int (win window))

;;; High-level Lisp wrappers

;; (clear scr :redraw t)
;; (clear scr :target :whole-screen :redraw t)

;; (clear scr :target :end-of-line)
;; (clear scr :target :bottom)

(defun clear (window &key redraw (target :whole-window))
  "Clear the window by overwriting it with blanks.

If the keyword redraw is t, first copy blanks to every position in the
window, then set the clear-redraw-flag to have the window redrawn from
scratch on the next refresh.

If target is :end-of-line, clear the window from the cursor to the end
of the current line.

If target is :bottom, clear the window from the cursor to the end of
the current line and all lines below."
  (let ((winptr (.c-pointer window)))
    (case target
      (:whole-window (if redraw (%wclear winptr) (%werase winptr)))
      (:end-of-line (%wclrtoeol winptr))
      (:bottom (%wclrtobot winptr)))))

;;; TODOs

;; [ ] clear docstring: check for naming of clearok.

