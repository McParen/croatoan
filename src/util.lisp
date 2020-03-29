(in-package :de.anvi.croatoan)

;;; util
;;; miscellaneous curses utility routines
;;; http://invisible-island.net/ncurses/man/curs_util.3x.html

(defgeneric char-to-string (char)
  (:documentation
   "Return a string representing the char. 
   Control characters are displayed using the ^X notation.
   Function keys are not represented by this routine. Use key-to-string instead."))

(defmethod char-to-string ((char character))
  (char-to-string (char-code char)))

(defmethod char-to-string ((char integer))
  (%unctrl char))

(defun string-to-char (str)
  "Take a string representing a character, return the corresponding character.

The char can be a printable, graphical char or a control char in the caret ^A notation."
  (let ((len (length str)))
    (case len
      (1 (coerce str 'character))
      (2 (if (string= str "^?")
             #\rubout
             ;; ^A = 1, A = 65.
             ;; The difference between an upcase char and the corresponding control char is 64.
             (code-char (- (char-code (char-upcase (char str 1))) 64))))
      (otherwise
       (error "string-to-char: ERROR, only graphic and control characters accepted.")))))

(defgeneric key-to-string (key)
  (:documentation
   "Return a string representing the key. 

The key can be a printable character, a control character or a function key.

In particular:

- A printable character becomes a string containing that character (e.g. #\a -> \"a\");

- A control character becomes a string with the (upcased) character prefixed with the caret character (#\^).
  (e.g Control-j -> \"^J\")

- A keyword becomes a string.
  Note that the key names returned by ncurses in general do not correspond to the key names used by croatoan.

  KEY_LEFT vs LEFT
  KEY_F(1) vs F1
  kLFT5    vs CTRL-LEFT

See: https://en.wikipedia.org/wiki/Control_character#How_control_characters_map_to_keyboards"))

(defmethod key-to-string ((key-name symbol))
  "Function key name represented by a keyword."
  (key-to-string (key-name-to-code key-name)))

(defmethod key-to-string ((key character))
  (key-to-string (char-code key)))

(defmethod key-to-string ((key integer))
  (%keyname key))

(defun flush-input ()
  "Throw away any typeahead that has been input by the user and has
not yet been read by the program.

This can be used to clear the input buffer after a key has been held
down for a longer time to prevent that events are handled by the
progrem even after the key is released. (That form of intertia can
happen in games.)

This ncurses function has the same purpose as cl:clear-input."
  (%flushinp))

;;; NOTES

;; Also see %use-legacy-coding in legacy_coding.lisp.

;;; TODOs

;; [ ] putwin, getwin, FILE pointer.
;; [ ] wunctrl, key_name, for wide chars.
