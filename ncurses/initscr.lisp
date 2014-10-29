(in-package :de.anvi.ncurses)

;;; initscr
;;; screen initialization and manipulation routines
;;; http://invisible-island.net/ncurses/man/curs_initscr.3x.html
;;; http://www.manpagez.com/man/3/curs_initscr/

;;; C prototypes

;; WINDOW *initscr(void);
;; int endwin(void);
;; bool isendwin(void);
;; SCREEN *newterm(char *type, FILE *outfd, FILE *infd);
;; SCREEN *set_term(SCREEN *new);
;; void delscreen(SCREEN *sp);

;;; Low-level CFFI wrappers

(defcfun ("initscr"   %initscr)   window)
(defcfun ("endwin"    %endwin)    :int)
(defcfun ("isendwin"  %isendwin)  :boolean)
(defcfun ("newterm"   %newterm)   screen    (type :string) (outfd file) (infd file))
(defcfun ("set_term"  %set-term)  screen    (new screen))
(defcfun ("delscreen" %delscreen) :void     (sp screen))
