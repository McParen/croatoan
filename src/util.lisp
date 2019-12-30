(in-package :croatoan)

;;; util
;;; miscellaneous curses utility routines
;;; http://invisible-island.net/ncurses/man/curs_util.3x.html

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
