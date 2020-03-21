(in-package :de.anvi.ncurses)

;;; inopts
;;; curses input options
;;; http://invisible-island.net/ncurses/man/curs_inopts.3x.html

;;; C prototypes

;; int cbreak(void);
;; int nocbreak(void);
;; int echo(void);
;; int noecho(void);
;; int halfdelay(int tenths);
;; int intrflush(WINDOW *win, bool bf);
;; int keypad(WINDOW *win, bool bf);
;; int meta(WINDOW *win, bool bf);
;; int nodelay(WINDOW *win, bool bf);
;; int raw(void);
;; int noraw(void);
;; void noqiflush(void);
;; void qiflush(void);
;; int notimeout(WINDOW *win, bool bf);
;; void timeout(int delay);
;; void wtimeout(WINDOW *win, int delay);
;; int typeahead(int fd);

;;; Low-level CFFI wrappers

(cffi:defcfun ("cbreak"    %cbreak)    :int)
(cffi:defcfun ("nocbreak"  %nocbreak)  :int)
(cffi:defcfun ("echo"      %echo)      :int)
(cffi:defcfun ("noecho"    %noecho)    :int)
(cffi:defcfun ("halfdelay" %halfdelay) :int  (tenths :int))
(cffi:defcfun ("intrflush" %intrflush) :int  (win window) (bf :boolean))
(cffi:defcfun ("keypad"    %keypad)    :int  (win window) (bf :boolean))
(cffi:defcfun ("meta"      %meta)      :int  (win window) (bf :boolean))
(cffi:defcfun ("nodelay"   %nodelay)   :int  (win window) (bf :boolean))
(cffi:defcfun ("raw"       %raw)       :int)
(cffi:defcfun ("noraw"     %noraw)     :int)
(cffi:defcfun ("noqiflush" %noqiflush) :void)
(cffi:defcfun ("qiflush"   %qiflush)   :void)
(cffi:defcfun ("notimeout" %notimeout) :int  (win window) (bf :boolean))
(cffi:defcfun ("timeout"   %timeout)   :void (delay :int))
(cffi:defcfun ("wtimeout"  %wtimeout)  :void (win window) (delay :int))
(cffi:defcfun ("typeahead" %typeahead) :int  (fd :int))
