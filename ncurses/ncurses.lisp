(in-package :de.anvi.ncurses)

;;; ncurses
;;; CRT screen handling and optimization package
;;; http://invisible-island.net/ncurses/man/ncurses.3x.html

(define-foreign-library libncurses
#+sb-unicode
    (:unix (:or "libncursesw.so.6.0" "libncursesw.so.6" "libncursesw.so.5.9" "libncursesw.so.5" "libncursesw.so"))
#-sb-unicode
    (:unix (:or "libncurses.so.6.0" "libncurses.so.6" "libncurses.so.5.9" "libncurses.so.5" "libncurses.so"))
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

;; wide character type, defined in:
;; /usr/lib/gcc/x86_64-linux-gnu/5/include/stddef.h
(defctype wchar_t :int32)

;; typedef chtype attr_t;
(defctype attr   :int)

;; winptr = *WINDOW
(defctype window :pointer)

;; scrptr = *SCREEN
(defctype screen :pointer)

;; fileptr = *FILE
(defctype file :pointer)

;; typedef unsigned int wint_t;
;; used in get_wch.lisp
(defctype wint_t :int32)


;;; C structures

#|

Excerpt from /usr/include/ncurses.h

/*
 * cchar_t stores an array of CCHARW_MAX wide characters.  The first is
 * normally a spacing character.  The others are non-spacing.  If those
 * (spacing and nonspacing) do not fill the array, a null L'\0' follows. 
 * Otherwise, a null is assumed to follow when extracting via getcchar().
 */
#define CCHARW_MAX	5
typedef struct
{
    attr_t	attr;
    wchar_t	chars[CCHARW_MAX];
#if 0
#undef NCURSES_EXT_COLORS
#define NCURSES_EXT_COLORS 20110404
    int		ext_color;	/* color pair, must be more than 16-bits */
#endif
}
cchar_t;

|#

;; For non-extended colors, the color pair is OR-ed into the attr value.
;; only for extended colors, we get a new struct slot.

;; Intended to be used with setcchar.
(defcstruct cchar_t
  "hello"
  (cchar-attr   attr)
  (cchar-chars  wchar_t :count 5)
  (cchar-colors :int))

;; Intended to be used with convert-to-foreign plist translation.
;; For some reasons, plists with pointers dont work, so we have to pass by value.
(defcstruct cchar
  "hello"
  (cchar-attr   attr)
  (cchar-chars  wchar_t)
  (cchar-colors :int))

;; the default on ubuntu 12.04, 16.04 is --enable-ext-colors,
;; so is probably on debian, so we need to include "int ext_color" by default in
;; the cchar struct.

;; convert-to-foreign and convert-from-foreign do not return structs,
;; but pointers to structs.

;;; mouse.lisp

;; typedef unsigned long mmask_t;
;; 
;; typedef struct {
;;     short id;         /* ID to distinguish multiple devices */
;;     int x, y, z;      /* event coordinates */
;;     mmask_t bstate;   /* button state bits */
;; } MEVENT;
;; 

(defctype mmask_t :ulong)

(defcstruct mevent
  "hello"
  (id :short)
  (x :int)
  (y :int)
  (z :int)
  (bstate mmask_t))

;;; ------------------------------------------------------------------

;;; Constants

;; #defines taken from ncurses.h, should not be user visible.
;; wherever cffi defines a boolean, we can use t and nil instead of TRUE and FALSE.
;;(defconstant TRUE 1)
;;(defconstant FALSE 0)
;;(defconstant ERROR -1)
;;(defconstant OK 0)
