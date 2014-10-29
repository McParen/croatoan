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

(defcfun ("newwin"     %newwin)     window               (nlines :int) (ncols :int) (begin_y :int) (begin_x :int))
(defcfun ("delwin"     %delwin)     :int   (win window))
(defcfun ("mvwin"      %mvwin)      :int   (win window)  (y :int) (x :int))
(defcfun ("subwin"     %subwin)     window (orig window) (nlines :int) (ncols :int) (begin_y :int) (begin_x :int))
(defcfun ("derwin"     %derwin)     window (orig window) (nlines :int) (ncols :int) (begin_y :int) (begin_x :int))
(defcfun ("mvderwin"   %mvderwin)   :int   (win window)  (par_y :int) (par_x :int))
(defcfun ("dupwin"     %dupwin)     window (win window))

(defcfun ("wsyncup"    %wsyncup)    :void  (win window))
(defcfun ("syncok"     %syncok)     :int   (win window) (bf :boolean))
(defcfun ("wcursyncup" %wcursyncup) :void  (win window))
(defcfun ("wsyncdown"  %wsyncdown)  :void  (win window))
