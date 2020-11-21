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
  (ncurses:unctrl char))

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
  "Take a keyword representing a croatoan function key name, return the corresponding ncurses/terminfo name as a string."
  (key-to-string (key-name-to-code key-name)))

(defmethod key-to-string ((key character))
  "Take a lisp character, return a string representing that character in ncurses/terminfo.

Control characters are represented in caret notation, for example ^J."
  (key-to-string (char-code key)))

(defmethod key-to-string ((key integer))
  "Take the integer code representing a character, return a string representing that character in ncurses/terminfo."
  (ncurses:keyname key))

(defun flush-input ()
  "Throw away any typeahead that has been input by the user and has
not yet been read by the program.

This can be used to clear the input buffer after a key has been held
down for a longer time to prevent that events are handled by the
progrem even after the key is released. (That form of intertia can
happen in games.)

This ncurses function has the same purpose as cl:clear-input."
  (ncurses:flushinp))

;; Original source: Lisp cookbook, author unknown, license MIT.
(defun split-string (str &optional (ch #\space))
  "Split the string into a list of words separated by one or more chars."
  (loop for i = 0 then (1+ j)
        for j = (position ch str :start i)
        for word = (subseq str i j)
        unless (zerop (length word))
        collect word
        while j))

(defun wrap-string (string width)
  "Place newlines in a string so that no single line exceeds the given width.

In this simple implementation, pre-existing newlines and multiple spaces are removed."
  (let* (;; the first word is printed without a preceding space
         (first t)
         ;; character position in the current line
         (pos 0)
         ;; first, remove existing newlines from the string.
         (str (substitute #\space #\newline string))
         ;; then split the string into single words, ignoring multiple spaces
         (words (split-string str)))
    ;; output everything to a string instead of standard output
    (with-output-to-string (*standard-output*)
      (loop for word in words do
        (if first
            ;; the first word does not have a space before it
            (progn
              (incf pos (length word))
              (if (<= pos width)
                  (progn
                    (setq first nil)
                    (princ word))
                  (progn
                    (setq pos 0)
                    (princ #\newline)
                    (princ word)
                    (incf pos (length word)))))
            ;; second and subsequent words
            (progn
              (incf pos (1+ (length word))) ; space + word
              (if (<= pos width)
                  (progn
                    (princ #\space)
                    (princ word))
                  (progn
                    (setq pos 0)
                    (princ #\newline)
                    (princ word)
                    (incf pos (length word))))))))))

;;; NOTES

;; Also see ncurses:use-legacy-coding in legacy_coding.lisp.

;;; TODOs

;; [ ] putwin, getwin, FILE pointer.
;; [ ] wunctrl, key_name, for wide chars.
