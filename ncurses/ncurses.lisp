(in-package :de.anvi.ncurses)

;;; ncurses
;;; CRT screen handling and optimization package
;;; http://invisible-island.net/ncurses/man/ncurses.3x.html
;;; http://www.manpagez.com/man/3/ncurses/

(define-foreign-library libncurses
#+sb-unicode
    (:unix (:or "libncursesw.so.5.9" "libncursesw.so.5" "libncursesw.so"))
#-sb-unicode
    (:unix (:or "libncurses.so.5.9" "libncurses.so.5" "libncurses.so"))
    (t     (:default "libncurses")))

(use-foreign-library libncurses)

;;; ------------------------------------------------------------------

;; Every function that can take a win, should take a win.
;; We dont want functions that implicitly operate on a global stdscr.

;; Function should return lisp-style bools, not C type int 0 or 1.
;; cffi automatically does this conversion.

;;; ------------------------------------------------------------------

;; General ncurses library implementation info:

;; data types:

;; http://pubs.opengroup.org/onlinepubs/7908799/xcurses/curses.h.html
;; http://pubs.opengroup.org/onlinepubs/7908799/xcurses/implement.html
;; http://pubs.opengroup.org/onlinepubs/7908799/cursesix.html

;; http://refspecs.linuxbase.org/LSB_3.0.0/LSB-Core-generic/LSB-Core-generic/libncurses.html
;; http://refspecs.linuxbase.org/LSB_3.0.0/LSB-Core-generic/LSB-Core-generic/libncurses-ddefs.html

;; http://h71000.www7.hp.com/doc/83final/5763/5763pro_016.html

;; http://docs.python.org/release/2.4.3/lib/curses-window-objects.html

;; http://web.ist.utl.pt/tiago.dionizio/lua/lcurses.html

;;; ------------------------------------------------------------------

;;; Types

;; Basic types as seen in the ncurses function prototypes.
;; These will only be used inside cffi wrappers.

;; typedef unsigned char bool;
(defctype bool   :int)

;; typedef unsigned long int chtype;
(defctype chtype :int)

;; typedef chtype attr_t;
(defctype attr   :int)

;; winptr = *WINDOW
(defctype window :pointer)

;; scrptr = *SCREEN
(defctype screen :pointer)

;; fileptr = *FILE
(defctype file :pointer)

;;; ------------------------------------------------------------------

;;; Constants

;; #defines taken from ncurses.h, should not be user visible.
;; wherever cffi defines a boolean, we can use t and nil instead of TRUE and FALSE.
;;(defconstant TRUE 1)
;;(defconstant FALSE 0)
;;(defconstant ERROR -1)
;;(defconstant OK 0)
