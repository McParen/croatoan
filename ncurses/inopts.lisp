(in-package :de.anvi.ncurses)

;;; inopts
;;; curses input options
;;; http://invisible-island.net/ncurses/man/curs_inopts.3x.html

;;; C prototypes

;; int cbreak(void);
;; int nocbreak(void);

;; int echo(void);
;; int noecho(void);

;; int intrflush(WINDOW *win, bool bf);
;; int keypad(WINDOW *win, bool bf);
;; int meta(WINDOW *win, bool bf);
;; int nodelay(WINDOW *win, bool bf);
;; int notimeout(WINDOW *win, bool bf);

;; int nl(void);
;; int nonl(void);

;; int raw(void);
;; int noraw(void);
;; void qiflush(void);
;; void noqiflush(void);

;; int halfdelay(int tenths);
;; void timeout(int delay);
;; void wtimeout(WINDOW *win, int delay);

;; int typeahead(int fd);

;; int is_cbreak(void);
;; int is_echo(void);
;; int is_nl(void);
;; int is_raw(void);

;;; Low-level CFFI wrappers

(cffi:defcfun ("cbreak"    cbreak)    :int)
(cffi:defcfun ("nocbreak"  nocbreak)  :int)

(cffi:defcfun ("echo"      echo)      :int)
(cffi:defcfun ("noecho"    noecho)    :int)

(cffi:defcfun ("intrflush" intrflush) :int  (win window) (bf :boolean))
(cffi:defcfun ("keypad"    keypad)    :int  (win window) (bf :boolean))
(cffi:defcfun ("meta"      meta)      :int  (win window) (bf :boolean))
(cffi:defcfun ("nodelay"   nodelay)   :int  (win window) (bf :boolean))
(cffi:defcfun ("notimeout" notimeout) :int  (win window) (bf :boolean))

(cffi:defcfun ("nl"        nl)        :int)
(cffi:defcfun ("nonl"      nonl)      :int)

(cffi:defcfun ("raw"       raw)       :int)
(cffi:defcfun ("noraw"     noraw)     :int)
(cffi:defcfun ("qiflush"   qiflush)   :void)
(cffi:defcfun ("noqiflush" noqiflush) :void)

(cffi:defcfun ("halfdelay" halfdelay) :int  (tenths :int))
(cffi:defcfun ("timeout"   timeout)   :void (delay :int))
(cffi:defcfun ("wtimeout"  wtimeout)  :void (win window) (delay :int))

(cffi:defcfun ("typeahead" typeahead) :int  (fd :int))

(cffi:defcfun ("is_cbreak" is-cbreak) :int)
(cffi:defcfun ("is_echo"   is-echo)   :int)
(cffi:defcfun ("is_nl"     is-nl)     :int)
(cffi:defcfun ("is_raw"    is-raw)    :int)
