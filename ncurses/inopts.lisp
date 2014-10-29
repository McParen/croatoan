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

(defcfun ("cbreak"    %cbreak)    :int)
(defcfun ("nocbreak"  %nocbreak)  :int)
(defcfun ("echo"      %echo)      :int)
(defcfun ("noecho"    %noecho)    :int)
(defcfun ("halfdelay" %halfdelay) :int  (tenths :int))
(defcfun ("intrflush" %intrflush) :int  (win window) (bf :boolean))
(defcfun ("keypad"    %keypad)    :int  (win window) (bf :boolean))
(defcfun ("meta"      %meta)      :int  (win window) (bf :boolean))
(defcfun ("nodelay"   %nodelay)   :int  (win window) (bf :boolean))
(defcfun ("raw"       %raw)       :int)
(defcfun ("noraw"     %noraw)     :int)
(defcfun ("noqiflush" %noqiflush) :void)
(defcfun ("qiflush"   %qiflush)   :void)
(defcfun ("notimeout" %notimeout) :int  (win window) (bf :boolean))
(defcfun ("timeout"   %timeout)   :void (delay :int))
(defcfun ("wtimeout"  %wtimeout)  :void (win window) (delay :int))
(defcfun ("typeahead" %typeahead) :int  (fd :int))
