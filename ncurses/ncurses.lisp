(in-package :de.anvi.ncurses)

;;; ncurses
;;; CRT screen handling and optimization package
;;; http://invisible-island.net/ncurses/man/ncurses.3x.html

;; The wide multi-byte library is preferred and will be loaded when the underlying lisp system supports unicode.
(define-foreign-library libncursesw
    (:darwin (:or "libncursesw.6.dylib" "libncursesw.5.dylib" "libncursesw.dylib" "libcurses.dylib"))
    (:unix   (:or "libncursesw.so.6.1" "libncursesw.so.6.0" "libncursesw.so.6" "libncursesw.so.5.9" "libncursesw.so.5" "libncursesw.so"))
    (t       (:default "libncursesw")))

#+(or sb-unicode unicode)
(use-foreign-library libncursesw)

;; Attempt to use the legacy single-byte library only when the lisp implementation doesnt support unicode.
(define-foreign-library libncurses
    (:darwin (:or "libncurses.6.dylib" "libncurses.5.dylib" "libncurses.dylib" "libcurses.dylib"))
    (:unix   (:or "libncurses.so.6.1" "libncurses.so.6.0" "libncurses.so.6" "libncurses.so.5.9" "libncurses.so.5" "libncurses.so"))
    (t       (:default "libncurses")))

#-(or sb-unicode unicode)
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

;; not used, cffi automatically translates a boolean type
;; typedef unsigned char bool;
(defctype bool :unsigned-char)

#|
--enable-lp64
Allows an application to define _LP64 to declare chtype and mmask_t as
simply unsigned rather than the configured types
using the --with-chtype and --with-mmask_t options.

#if 1 && defined(_LP64)
typedef unsigned chtype;
typedef unsigned mmask_t;
#else
typedef uint32_t chtype;
typedef uint32_t mmask_t;
#endif
|#

;; default ubuntu build options, they build for API5.
;; --disable-lp64 --with-chtype='long' --with-mmask-t='long'

;; the default type on ubuntu isnt "unsigned int" but "long", which means signed long.

;; We will use the ABI6 default uint32_t instead of long.

;; TODO: use ABI6 values for chtype, cchar_t and mmask: --with-chtype=uint32_t
;;(defctype chtype :unsigned-int)
(defctype chtype :uint32)

;; wide character type, defined in:
;; /usr/lib/gcc/x86_64-linux-gnu/5/include/stddef.h
;; it has to be able to contain the 21-bit 10FFFF, which is the max allowed unicode point.
(defctype wchar_t :int32)

;; typedef unsigned int wint_t;
;; used in get_wch.lisp
;; wint_t needs to be a signed int to represent a negative WEOF value.
(defctype wint_t :int32)

;; typedef chtype attr_t;
;; TODO: rename this to attr_t
(defctype attr :uint32
  "The 32 bit integral type attr_t holds an OR-ed set of attributes.")
(defctype attr_t :uint32
  "The 32 bit integral type attr_t holds an OR-ed set of attributes.")

;; winptr = *WINDOW
;; TODO: define window struct
;; (defctype ptr-window (:pointer (:struct window)))
(defctype window :pointer)

;; scrptr = *SCREEN
(defctype screen :pointer)

;; fileptr = *FILE
(defctype file :pointer)

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

;; For non-extended colors, the color pair is OR-ed into the attr value, like it has been done
;; with color pairs in chtypes. This is ABI5.

;;(defcstruct cchar_t
;;  "A C struct containing contains a wide char and an integer containing the color pair and attributes."
;;  (cchar-attr   attr)
;;  (cchar-chars  wchar_t :count 5))

;; For extended colors, we get a new struct slot. This is ABI6, which is the default since ncurses 6.0, 20150808.

;; Intended to be used with setcchar.
;; TODO: write meaningsful docstrings here.
(defcstruct cchar_t
  "C struct containing a wide char, a color pair and attributes."
  (cchar-attr   attr_t)
  (cchar-chars  wchar_t :count 5)
  (cchar-colors :int))

(defctype ptr-cchar_t (:pointer (:struct cchar_t)))

;; Intended to be used with convert-to-foreign plist translation.
;; For some reasons, plists with pointers dont work, so we have to pass by value.
;; TODO: we dont need this any more, everything works now with the cchar_t struct.
(defcstruct cchar
  "C struct containing a wide char, a color pair and attributes."
  (cchar-attr   attr_t)
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

;; TODO: ABI6: --with-mmask_t=uint32_t
;;(defctype mmask_t :unsigned-int)
(defctype mmask_t :uint32)

(defcstruct mevent
  "C struct containing mouse coordinates and button state."
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
