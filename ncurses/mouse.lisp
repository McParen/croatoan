(in-package :de.anvi.ncurses)

;;; mouse
;;; mouse interface through curses
;;; http://invisible-island.net/ncurses/man/curs_mouse.3x.html

;;; C prototypes

;;; typedef unsigned long mmask_t;
;;; 
;;; typedef struct {
;;;     short id;         /* ID to distinguish multiple devices */
;;;     int x, y, z;      /* event coordinates */
;;;     mmask_t bstate;   /* button state bits */
;;; } MEVENT;
;;; 
;;; bool has_mouse(void);
;;; int getmouse(MEVENT *event);
;;; int ungetmouse(MEVENT *event);
;;; mmask_t mousemask(mmask_t newmask, mmask_t *oldmask);
;;; bool wenclose(const WINDOW *win, int y, int x);
;;; bool mouse_trafo(int* pY, int* pX, bool to_screen);
;;; bool wmouse_trafo(const WINDOW* win, int* pY, int* pX, bool to_screen);
;;; int mouseinterval(int erval);

;;; Low-level CFFI wrappers

(defctype mmask_t :ulong)

(defcstruct mevent (id :short) (x :int) (y :int) (z :int) (bstate mmask_t))

(defcfun ("getmouse"   %getmouse)   :int      (event (:pointer mevent)))
(defcfun ("mousemask"  %mousemask)  mmask_t   (newmask mmask_t) (oldmask (:pointer mmask_t)))

(defcfun ("has_mouse"  %has-mouse)  :boolean)
(defcfun ("ungetmouse" %ungetmouse) :int      (event (:pointer mevent)))
