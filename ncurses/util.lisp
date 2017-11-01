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

(defcfun ("unctrl"       %unctrl)       :string (c chtype))
(defcfun ("wunctrl"      %wunctrl)      :string (c (:pointer (:struct cchar_t))))
(defcfun ("keyname"      %keyname)      :string (c :int))
(defcfun ("key_name"     %key_name)     :string (w wchar_t))
(defcfun ("filter"       %filter)       :void)
(defcfun ("nofilter"     %nofilter)     :void)
(defcfun ("use_env"      %use-env)      :void   (f :boolean))
(defcfun ("putwin"       %putwin)       :int    (win window) (filep :pointer))
(defcfun ("getwin"       %getwin)       window  (filep :pointer))
(defcfun ("delay_output" %delay-output) :int    (ms :int))
(defcfun ("flushinp"     %flushinp)     :int)
