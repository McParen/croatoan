(in-package :de.anvi.ncurses)

;;; outopts
;;; curses output options
;;; http://invisible-island.net/ncurses/man/curs_outopts.3x.html

;;; C prototypes

;; int clearok(WINDOW *win, bool bf);
;; int idlok(WINDOW *win, bool bf);
;; void idcok(WINDOW *win, bool bf);
;; void immedok(WINDOW *win, bool bf);
;; int leaveok(WINDOW *win, bool bf);
;; int scrollok(WINDOW *win, bool bf);

;; int setscrreg(int top, int bot);
;; int wsetscrreg(WINDOW *win, int top, int bot);

;; int nl(void);
;; int nonl(void);

;;; Low-level CFFI wrappers

(defcfun ("clearok"    %clearok)    :int  (win window) (bf :boolean))
(defcfun ("idlok"      %idlok)      :int  (win window) (bf :boolean))
(defcfun ("idcok"      %idcok)      :void (win window) (bf :boolean))
(defcfun ("immedok"    %immedok)    :void (win window) (bf :boolean))
(defcfun ("leaveok"    %leaveok)    :int  (win window) (bf :boolean))
(defcfun ("scrollok"   %scrollok)   :int  (win window) (bf :boolean))

(defcfun ("setscrreg"  %setscrreg)  :int               (top :int) (bot :int))
(defcfun ("wsetscrreg" %wsetscrreg) :int  (win window) (top :int) (bot :int))

(defcfun ("nl"         %nl)         :void)
(defcfun ("nonl"       %nonl)       :void)
