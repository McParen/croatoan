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

(cffi:defcfun ("initscr"   initscr)   window)
(cffi:defcfun ("endwin"    endwin)    :int)
(cffi:defcfun ("isendwin"  isendwin)  :boolean)
(cffi:defcfun ("newterm"   newterm)   screen    (type :string) (outfd file) (infd file))
(cffi:defcfun ("set_term"  set-term)  screen    (new screen))
(cffi:defcfun ("delscreen" delscreen) :void     (sp screen))
