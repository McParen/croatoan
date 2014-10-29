(in-package :de.anvi.ncurses)

;;; extend
;;; miscellaneous curses extensions
;;; http://invisible-island.net/ncurses/man/curs_extend.3x.html

;;; C prototypes

;; const char * curses_version(void);
;; int use_extended_names(bool enable);

;;; Low-level CFFI wrappers

(defcfun ("curses_version"     %curses-version)     :string)
(defcfun ("use_extended_names" %use-extended-names) :int (enable :boolean))
