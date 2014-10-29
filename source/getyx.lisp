(in-package :de.anvi.croatoan)

;; TODO: window auf winpter umstellen.

(defun cursor-position (window)
  "Returns a cons pair of the current cursor coordinates (line-y . column-x) in window."
  (cons (%getcury window)
        (%getcurx window)))

(defun window-begin (window)
  "Returns a cons pair of the top left beginning coordinates (y . x) of window."
  (cons (%getbegy window)
        (%getbegx window)))

(defun subwindow-relative-begin (subwindow)
  "Returns a cons pair (y . x) of beginning coordinates of a subwindow relative to the parent window."
  (cons (%getpary subwindow)
        (%getparx subwindow)))

(defun window-size (window)
  "Returns window size as a cons pair (height . width)."
  (cons (%getmaxy window)
        (%getmaxx window)))

;;; NOTES

#|

Those 4 C macros are defined in terms of other, simpler macros:

#define getyx(win,y,x)          (y = getcury(win), x = getcurx(win))
#define getbegyx(win,y,x)       (y = getbegy(win), x = getbegx(win))
#define getmaxyx(win,y,x)       (y = getmaxy(win), x = getmaxx(win))
#define getparyx(win,y,x)       (y = getpary(win), x = getparx(win))

And those simpler macros are just accessing the window struct.

#define getcurx(win)            ((win) ? (win)->_curx : ERR)
#define getcury(win)            ((win) ? (win)->_cury : ERR)
#define getbegx(win)            ((win) ? (win)->_begx : ERR)
#define getbegy(win)            ((win) ? (win)->_begy : ERR)
#define getmaxx(win)            ((win) ? ((win)->_maxx + 1) : ERR)
#define getmaxy(win)            ((win) ? ((win)->_maxy + 1) : ERR)
#define getparx(win)            ((win) ? (win)->_parx : ERR)
#define getpary(win)            ((win) ? (win)->_pary : ERR)

Those are defined as low-level functions in legacy.lisp.

|#

;;; TODOs

;; combine the 2 win and subwin functions into one by using opaque/is_subwin.

