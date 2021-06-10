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

(defun split-lines (str)
  "Split a string containing newlines into a list of strings."
  (split-string str #\newline))

(defun join-strings (list &optional ch)
  (if ch
      ;; every has to be put in the control string first.
      (format nil (concatenate 'string "~{~A~^" (list ch) "~}") list)
      ;; the default join char is #\space
      (format nil "~{~A~^ ~}" list)))

(defun join-lines (list)
  "Join a list of strings into a string of lines separated by newline."
  (join-strings list #\newline))

(defun wrap-string (string width)
  "Insert newlines in the string so that no single line exceeds the width.

All pre-existing newlines and multiple spaces are removed."
  (let* ((pos 0) ; next char position in the current line
         (str (substitute #\space #\newline string)) ; remove existing newlines from the string
         (words (split-string str))) ; split the string into words, ignore multiple spaces
    (labels ((newline () (terpri) (setq pos 0))
             (space () (princ #\space) (incf pos))
             (prinw (word)
               (let ((len (length word)))
                 (cond
                   ;; long word at pos=0
                   ((and (zerop pos) (> len width))
                    ;; first print width chars
                    (princ (subseq word 0 width))
                    (newline)
                    ;; then recursively print the rest
                    (prinw (subseq word width)))
                   ;; long word at pos>0
                   ((and (not (zerop pos)) (> len width))
                    (if (= width pos) (newline) (space))
                    (let ((pos2 (- width pos)))
                      ;; first print width chars
                      (princ (subseq word 0 pos2))
                      (newline)
                      ;; then print the rest starting from pos2
                      (prinw (subseq word pos2))))
                   ;; fitting word at pos=0
                   ((and (zerop pos) (< len width))
                    (princ word)
                    (incf pos len))
                   ;; fitting word at pos>0
                   ((and (not (zerop pos)) (<= (+ pos 1 len) width))
                    (if (= width pos) (newline) (space))
                    (princ word)
                    (incf pos len))
                   ;; not fitting word at pos>0
                   ((and (not (zerop pos)) (> (+ pos 1 len) width))
                    (newline)
                    (princ word)
                    (incf pos len))))))
      ;; output everything to a string instead of standard output
      (with-output-to-string (*standard-output*)
        (dolist (word words)
          (prinw word))))))

(defun wrap-lines (lines width)
  "Wrap a list of strings so that no single line exceeds the given width."
  (split-lines (wrap-string (join-lines lines) width)))

;;; NOTES

;; Also see ncurses:use-legacy-coding in legacy_coding.lisp.

;;; TODOs

;; [ ] putwin, getwin, FILE pointer.
;; [ ] wunctrl, key_name, for wide chars.
