(in-package :de.anvi.ncurses)

;;; util
;;; miscellaneous curses utility routines
;;; http://invisible-island.net/ncurses/man/curs_util.3x.html

;;; C prototypes

;; char *unctrl(chtype c);
;; wchar_t *wunctrl(cchar_t *c);
;; char *keyname(int c);
;; char *key_name(wchar_t w);
;; void filter(void);
;; void nofilter(void);
;; void use_env(bool f);
;; int putwin(WINDOW *win, FILE *filep);
;; WINDOW *getwin(FILE *filep);
;; int delay_output(int ms);
;; int flushinp(void);

;;; Low-level CFFI wrappers

(cffi:defcfun ("unctrl"       unctrl)       :string (c chtype))
(cffi:defcfun ("wunctrl"      wunctrl)      :string (c (:pointer (:struct cchar_t))))
(cffi:defcfun ("keyname"      keyname)      :string (c :int))
(cffi:defcfun ("key_name"     key_name)     :string (w wchar_t))
(cffi:defcfun ("filter"       filter)       :void)
(cffi:defcfun ("nofilter"     nofilter)     :void)
(cffi:defcfun ("use_env"      use-env)      :void   (f :boolean))
(cffi:defcfun ("putwin"       putwin)       :int    (win window) (filep :pointer))
(cffi:defcfun ("getwin"       getwin)       window  (filep :pointer))
(cffi:defcfun ("delay_output" delay-output) :int    (ms :int))
(cffi:defcfun ("flushinp"     flushinp)     :int)
