(in-package :croatoan)

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

;;; Low-level C functions

(defcfun ("unctrl" %unctrl) :string (c chtype))
(defcfun ("keyname" %keyname) :string (c :int))

(defcfun ("filter" %filter) :void)
(defcfun ("nofilter" %nofilter) :void)

(defcfun ("use-env" %use-env) :void (f :boolean))

(defcfun ("delay_output" %delay-output) :int (ms :int))
(defcfun ("flushinp" %flushinp) :int)

;;; High-level Lisp wrappers

(defun char-to-string (char)
  "Return a string representing the char."
  (%unctrl char))

(defun key-to-string (key)
  "Return a string representing they key."
  (%keyname key))

;;; NOTES

;; Also see %use-legacy-coding in legacy_coding.lisp.

;;; TODOs

;; [ ] putwin, getwin, FILE pointer.
;; [ ] wunctrl, key_name, for wide chars.

