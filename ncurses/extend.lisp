(in-package :de.anvi.ncurses)

;;; extend
;;; miscellaneous curses extensions
;;; http://invisible-island.net/ncurses/man/curs_extend.3x.html

;;; C prototypes

;; const char * curses_version(void);
;; int use_extended_names(bool enable);

;;; Low-level CFFI wrappers

(cffi:defcfun ("curses_version"     %curses-version)     :string)
(cffi:defcfun ("use_extended_names" %use-extended-names) :int (enable :boolean))
