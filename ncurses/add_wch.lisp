(in-package :de.anvi.ncurses)

;;; add_wch
;;; add a complex character and rendition to a curses window, then advance the cursor
;;; http://invisible-island.net/ncurses/man/curs_add_wch.3x.html

;;; C data types

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

;;; C prototypes

;;; int add_wch(const cchar_t *wch);
;;; int wadd_wch(WINDOW *win, const cchar_t *wch);
;;; int mvadd_wch(int y, int x, const cchar_t *wch);
;;; int mvwadd_wch( WINDOW *win, int y, int x, const cchar_t *wch);
;;; int echo_wchar(const cchar_t *wch);
;;; int wecho_wchar(WINDOW *win, const cchar_t *wch);

;;; Low-level CFFI wrappers

(defctype wchar_t :int32)

;; Intended to be used with convert-to-foreign plist translation.
;; For some reasons, plists with pointers dont work, so we have to pass by value.
(defcstruct cchar (cchar-attr attr) (cchar-chars wchar_t))

;; Intended to be used with setcchar.
(defcstruct cchar_t (cchar-attr attr) (cchar-chars wchar_t :count 5))

(defcfun ("add_wch"    %add-wch)    :int                                 (wch (:pointer (:struct cchar_t))))
(defcfun ("wadd_wch"   %wadd-wch)   :int  (win window)                   (wch (:pointer (:struct cchar_t))))

(defcfun ("mvadd_wch"  %mvadd-wch)  :int               (y :int) (x :int) (wch (:pointer (:struct cchar_t))))
(defcfun ("mvwadd_wch" %mvwadd-wch) :int  (win window) (y :int) (x :int) (wch (:pointer (:struct cchar_t))))

(defcfun ("echo_wch"   %echo-wch)   :int                                 (wch (:pointer (:struct cchar_t))))
(defcfun ("wecho_wch"  %wecho-wch)  :int  (win window)                   (wch (:pointer (:struct cchar_t))))
