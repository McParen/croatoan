(in-package :de.anvi.ncurses)

;;; window
;;; create curses windows
;;; http://invisible-island.net/ncurses/man/curs_window.3x.html
;;; http://www.manpagez.com/man/3/newwin/

;;; C prototypes

;; WINDOW *newwin(int nlines, int ncols, int begin_y, int begin_x);
;; int delwin(WINDOW *win);
;; int mvwin(WINDOW *win, int y, int x);
;; WINDOW *subwin(WINDOW *orig, int nlines, int ncols, int begin_y, int begin_x);
;; WINDOW *derwin(WINDOW *orig, int nlines, int ncols, int begin_y, int begin_x);
;; int mvderwin(WINDOW *win, int par_y, int par_x);
;; WINDOW *dupwin(WINDOW *win);

;; void wsyncup(WINDOW *win);
;; int syncok(WINDOW *win, bool bf);
;; void wcursyncup(WINDOW *win);
;; void wsyncdown(WINDOW *win);

;;; Low-level CFFI wrappers

(cffi:defcfun ("newwin"     newwin)     window               (nlines :int) (ncols :int) (begin_y :int) (begin_x :int))
(cffi:defcfun ("delwin"     delwin)     :int   (win window))
(cffi:defcfun ("mvwin"      mvwin)      :int   (win window)  (y :int) (x :int))
(cffi:defcfun ("subwin"     subwin)     window (orig window) (nlines :int) (ncols :int) (begin_y :int) (begin_x :int))
(cffi:defcfun ("derwin"     derwin)     window (orig window) (nlines :int) (ncols :int) (begin_y :int) (begin_x :int))
(cffi:defcfun ("mvderwin"   mvderwin)   :int   (win window)  (par_y :int) (par_x :int))
(cffi:defcfun ("dupwin"     dupwin)     window (win window))

(cffi:defcfun ("wsyncup"    wsyncup)    :void  (win window))
(cffi:defcfun ("syncok"     syncok)     :int   (win window) (bf :boolean))
(cffi:defcfun ("wcursyncup" wcursyncup) :void  (win window))
(cffi:defcfun ("wsyncdown"  wsyncdown)  :void  (win window))
