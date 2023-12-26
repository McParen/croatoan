(in-package :de.anvi.ncurses)

;;; terminfo
;;; curses interfaces to terminfo database
;;; http://invisible-island.net/ncurses/man/curs_terminfo.3x.html

;;; C prototypes

;; int setupterm(const char *term, int filedes, int *errret);
;; TERMINAL *set_curterm(TERMINAL *nterm);
;; int del_curterm(TERMINAL *oterm);
;; int restartterm(const char *term, int filedes, int *errret);

;; char *tparm(const char *str, ...);
;; char *tparm(const char *str, long p1 ... long p9);

;; int tputs(const char *str, int affcnt, int (*putc)(int));
;; int putp(const char *str);

;; int vidputs(chtype attrs, int (*putc)(int));
;; int vidattr(chtype attrs);
;; int vid_puts(attr_t attrs, short pair, void *opts, int (*putc)(int));
;; int vid_attr(attr_t attrs, short pair, void *opts);

;; int mvcur(int oldrow, int oldcol, int newrow, int newcol);

;; int tigetflag(const char *capname);
;; int tigetnum(const char *capname);
;; char *tigetstr(const char *capname);

;; char *tiparm(const char *str, ...);

;; char *tiparm_s(int expected, int mask, const char *str, ...);
;; int tiscan_s(int *expected, int *mask, const char *str);

;; int setterm(const char *term);

;;; Low-level CFFI wrappers

(cffi:defcfun ("setupterm"   setupterm)   :int     (term :string) (filedes :int) (errret (:pointer :int)))
(cffi:defcfun ("restartterm" restartterm) :int     (term :string) (filedes :int) (errret (:pointer :int)))

(cffi:defcfun ("set_curterm" set-curterm) terminal (nterm terminal))
(cffi:defcfun ("del_curterm" del-curterm) :int     (oterm terminal))

(cffi:defcfun ("putp"        putp)        :int     (str :string))

(cffi:defcfun ("vidattr"     vidattr)     :int     (attrs chtype))
(cffi:defcfun ("vid_attr"    vid-attr)    :int     (attrs attr_t) (pair :short) (opts (:pointer :void)))

(cffi:defcfun ("mvcur"       mvcur)       :int     (oldrow :int) (oldcol :int) (newrow :int) (newcol :int))

(cffi:defcfun ("tigetflag"   tigetflag)   :int     (capname :string))
(cffi:defcfun ("tigetnum"    tigetnum)    :int     (capname :string))

(cffi:defcfun ("tigetstr"    tigetstr)    :pointer (capname :string))

(cffi:defcfun ("tiscan_s"    tiscan-s)    :int     (expected (:pointer :int)) (mask (:pointer :int)) (str :string))

(cffi:defcfun ("setterm"     setterm)     :int     (term :string))
