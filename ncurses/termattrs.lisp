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

(cffi:defcfun ("baudrate"   baudrate)   :int)
(cffi:defcfun ("erasechar"  erasechar)  :char)
(cffi:defcfun ("has_ic"     has-ic)     :boolean)
(cffi:defcfun ("has_il"     has-il)     :boolean)
(cffi:defcfun ("killchar"   killchar)   :char)
(cffi:defcfun ("longname"   longname)   :string)
(cffi:defcfun ("term_attrs" term-attrs) attr_t)
(cffi:defcfun ("termattrs"  termattrs)  chtype)
(cffi:defcfun ("termname"   termname)   :string)
