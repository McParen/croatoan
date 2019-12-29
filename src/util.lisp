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

;;; Commented out as they are already defined in ncurses/util.lisp

;; (defcfun ("unctrl" %unctrl) :string (c chtype))
;; (defcfun ("keyname" %keyname) :string (c :int))

;; (defcfun ("filter" %filter) :void)
;; (defcfun ("nofilter" %nofilter) :void)

;; (defcfun ("use-env" %use-env) :void (f :boolean))

;; (defcfun ("delay_output" %delay-output) :int (ms :int))
;; (defcfun ("flushinp" %flushinp) :int)

;;; High-level Lisp wrappers

(defgeneric char-to-string (char)
  (:documentation "Return a string representing the char."))

(defmethod char-to-string ((char character))
  (char-to-string (char-code char)))

(defmethod char-to-string ((char integer))
  (%unctrl char))

(defgeneric key-to-string (key)
  (:documentation
   "Return a string representing the key.

In particular:

- a   printable   character   become   a   string   cointaining   that
  character (e.g. #\a -> \"a\");

- a control character become a string with the character prefixed with char caret (#\^)
  (e.g Control-j -> \"^J\", note that the character is always upcase
  See: https://en.wikipedia.org/wiki/Control_character#How_control_characters_map_to_keyboards
"))

(defmethod key-to-string ((key character))
  (key-to-string (char-code key)))

(defmethod key-to-string ((key integer))
  (%keyname key))

;;; NOTES

;; Also see %use-legacy-coding in legacy_coding.lisp.

;;; TODOs

;; [ ] putwin, getwin, FILE pointer.
;; [ ] wunctrl, key_name, for wide chars.
