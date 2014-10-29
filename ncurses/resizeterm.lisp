(in-package :de.anvi.ncurses)

;;; resizeterm
;;; change the curses terminal size
;;; http://invisible-island.net/ncurses/man/resizeterm.3x.html

;;; C prototypes

;; bool is_term_resized(int lines, int columns);
;; int resize_term(int lines, int columns);
;; int resizeterm(int lines, int columns);

;;; Low-level CFFI wrappers

(defcfun ("is_term_resized" %is-term-resized) :boolean (lines :int) (columns :int))
(defcfun ("resize_term"     %resize-term)     :int     (lines :int) (columns :int))
(defcfun ("resizeterm"      %resizeterm)      :int     (lines :int) (columns :int))
