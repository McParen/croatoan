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

(defmethod key-to-string ((key key))
  "Take a key struct, return the ncurses/terminfo name as a string."
  (when (key-code key)
    (key-to-string (key-code key))))

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
;; (split-string "a b c") => ("a" "b" "c")
;; (split-string "  a  b  c  ") => ("a" "b" "c")
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

;; (join-strings (list "a" "b" "c")) => "a b c"
;; (join-strings (split-string " a  b  c ")) => "a b c"
(defun join-strings (list &optional ch)
  (if ch
      ;; every has to be put in the control string first.
      (format nil (concatenate 'string "~{~A~^" (list ch) "~}") list)
      ;; the default join char is #\space
      (format nil "~{~A~^ ~}" list)))

(defun join-lines (list)
  "Join a list of strings into a string of lines separated by newline."
  (join-strings list #\newline))

(defun wrap-string (string width &optional (split-length 20))
  "Insert newlines in the string so that no single line exceeds the width.

All pre-existing newlines and multiple spaces are removed.

Long words (by default >20 chars) are split instead of being wrapped.
This avoids large gaps when long urls are wrapped, for example."
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
                   ;; split words longer than 20 chars (for example long urls)
                   ((and (not (zerop pos))
                         (> len split-length)
                         (> (+ pos len) width))
                    (if (= width pos)
                        ;; if we have a long word at the end of the line,
                        ;; jump to the next line, then try again.
                        (progn (newline)
                               (prinw word))
                        (progn (space)
                               (let ((pos2 (- width pos)))
                                 (princ (subseq word 0 pos2))
                                 (newline)
                                 (prinw (subseq word pos2))))))
                   ;; fitting word at pos=0
                   ((and (zerop pos) (<= len width))
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

(defun char-width (char)
  "Return the number of columns required to display the wide char.

If char is not printable, for example a control char, return nil.

In order to get a meaningful result from wcwidth, the locale has to
be set, see `setlocale'."
  (let ((retval (ncurses:wcwidth (char-code char))))
    ;; instead of possible return values 0 for #\nul and -1 for all
    ;; other non-printable control chars, return nil.
    (if (plusp retval)
        retval
        nil)))

(defun string-width (str)
  "Return the number of columns required to display the string.

If the string contains a non-printable character, return nil."
  (loop for ch across str
        for cw = (char-width ch)
        if cw
          sum it
        else
          return nil))

(defun control-char-p (char)
  (let ((code (char-code char)))
    (or (<= 0 code 31)
        (<= 127 code 159))))

(defun pair-plist (keys values &optional plist)
  "Return a plist constructed out of separate keys and values lists.

This is supposed to work like cl:pairlis but returning a plist instead
of an alist.

If an initial plist is provided, the new key-value pairs are prepended
to it.

Example 1: (pair-plist '(a b c) '(1 2 3)) => (a 1 b 2 c 3)

Example 2: (pair-plist '(a b) '(1 2) '(x 3 y 4)) => (A 1 B 2 X 3 Y 4)

The resulting plist can be again taken apart into keys and values by
the reverse function `unpair-plist'."
  (let ((new-plist (mapcan #'list keys values)))
    (if plist
        (nconc new-plist plist)
        new-plist)))

#|
CL-USER> (multiple-value-bind (k v) (unpair-plist '(a 1 b 2 c 3))
           (pair-plist k v))
(A 1 B 2 C 3)
|#
(defun unpair-plist (plist)
  "Separate a plist into two lists, returned as separate values.

The first list contains the keys and the second the values.

Example: (unpair-plist '(a 1 b 2 c 3)) => (a b c), (1 2 3)

The resulting keys and values can be combined back into the initial
plist by the function `pair-plist'."
  (loop for (k v) on plist by #'cddr
        collect k into keys
        collect v into vals
        finally (return (values keys vals))))

(defun mapc-plist (fn &rest plist)
  "Mapc function of two args (k v) to successive keys and values in plist.

The function is applied for its side effects, the results are not accumulated."
  (multiple-value-bind (keys values) (unpair-plist plist)
    (mapc fn keys values)))
