(in-package :croatoan)

;;; resizeterm
;;; change the curses terminal size
;;; http://invisible-island.net/ncurses/man/resizeterm.3x.html

;;; C prototypes

;; bool is_term_resized(int lines, int columns);
;; int resize_term(int lines, int columns);
;; int resizeterm(int lines, int columns);

;;; Low-level C functions

(defcfun ("is_term_resized" %is-term-resized) :boolean (lines :int) (columns :int))
(defcfun ("resize_term"     %resize-term)     :int     (lines :int) (columns :int))
(defcfun ("resizeterm"      %is-term-resized) :int     (lines :int) (columns :int))

;;; High-level Lisp wrappers

(defun terminal-resized-p (height width)
  "Returns t if resize-terminal would modify a window, and nil otherwise."
  (%is-term-resized height width))

(defun resize-terminal (height width &key use-sigwinch-handler)
  "Resizes the current window to the specified dimensions.

The function attempts to resize all windows. The areas that are
extended are filled with blanks.

It is not possible to resize pads without additional interaction with
the application.

If use-sigwinch-handler is t, add bookkeeping for the SIGWINCH
handler."
  (if use-sigwinch-handler
      (%resizeterm height width)
      (%resize-term height width)))

;;; TODOs

