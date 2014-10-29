(in-package :de.anvi.ncurses)

;;; termattrs
;;; environment query routines
;;; http://invisible-island.net/ncurses/man/curs_termattrs.3x.html

;;; C prototypes

;; int baudrate(void);
;; char erasechar(void);
;; int erasewchar(wchar_t *ch);
;; bool has_ic(void);
;; bool has_il(void);
;; char killchar(void);
;; int killwchar(wchar_t *ch);
;; char *longname(void);
;; attr_t term_attrs(void);
;; chtype termattrs(void);
;; char *termname(void);

;;; Low-level CFFI wrappers

(defcfun ("baudrate"   %baudrate)    :void)
(defcfun ("erasechar"  %erasechar)   :char)
(defcfun ("has_ic"     %has_ic)      :boolean)
(defcfun ("has_il"     %has_il)      :boolean)
(defcfun ("killchar"   %killchar)    :char)
(defcfun ("longname"   %longname)    :string)
(defcfun ("term_attrs" %term-attrs)  attr)
(defcfun ("termattrs"  %termattrs)   chtype)
(defcfun ("termname"   %termname)    :string)

;;; High-level Lisp wrappers

;;; NOTES

;;; TODOs
