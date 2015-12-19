(in-package :de.anvi.croatoan)

;; (add-char scr (code-char ch) :attrs '(:bold) :color-pair '(:red :yellow))
(defun add-char (window char &key attributes color-pair y x)
  "Add the char to the window, then advance the cursor.

If the destination coordinates y and x are given, move the cursor to the
destination first and then add the character."
  (let ((winptr (.winptr window))
        (chtype (logior (typecase char
                          ;; if the char is already an integer chtype
                          (integer char)
                          ;; alternative chars are given as keywords
                          (keyword (acs char))
                          ;; if it is a lisp char, convert it to an integer first
                          (character (char-code char))
                          (t 0))
                        ;; convert the pair to an integer, then bit shift it by 8
                        (if color-pair (ash (pair->number color-pair) 8) 0)
                        ;; the attribute bitmasks already are bitshifted to the
                        ;; correct position in the chtype
                        (if attributes (attrs2chtype attributes) 0))))
    (cond ((and y x)
           (%mvwaddch winptr y x chtype))
          (t
           (%waddch winptr chtype)))))

(defun echo-char (window chtype)
  "Add the rendered character to the window, then refresh the window.

If the destination coordinates Y and X are given, move to the
destination first and then add the character. 

The only difference to add-char and a subsequent refresh is a
performance gain if we know that we only need to output a single
character."
  (let ((winptr (.winptr window)))
    (%wechochar winptr chtype)))

;; just an utility function if you dont want to use (format nil "bla
;; bla ~%") to insert newlines. in C you can simply insert \n.
(defun new-line (window &optional (count 1))
  "Insert count newline characters into window."
  (loop repeat count do (add-char window (char-code #\newline))))

;; pointer to the global/external c acs array, acs_map[].
(defparameter acs-map-array (foreign-symbol-pointer "acs_map"))

;; ncurses maps those standard chars at runtime to the acs characters.
;; here we use it in the function acs.
(defparameter acs-alist
  '(( :ulcorner . #\l )
    ( :llcorner . #\m )
    ( :urcorner . #\k )
    ( :lrcorner . #\j )
    ( :ltee     . #\t )
    ( :rtee     . #\u )
    ( :btee     . #\v )
    ( :ttee     . #\w )
    ( :hline    . #\q )
    ( :vline    . #\x )
    ( :plus     . #\n )
    ( :s1       . #\o )
    ( :s3       . #\p )
    ( :s7       . #\r )
    ( :s9       . #\s )
    ( :diamond  . #\` )
    ( :ckboard  . #\a )
    ( :degree   . #\f )
    ( :plminus  . #\g )
    ( :bullet   . #\~ )
    ( :larrow   . #\, )
    ( :rarrow   . #\+ )
    ( :darrow   . #\. )
    ( :uarrow   . #\- )
    ( :board    . #\h )
    ( :lantern  . #\i )
    ( :block    . #\0 )
    ( :lequal   . #\y )
    ( :gequal   . #\z )
    ( :pi       . #\{ )
    ( :nequal   . #\| )
    ( :sterling . #\} )))

;; ACS, the alternative/extended character set for line drawing.
;; Used by functions: add-char, box and border.
;; 
;; * http://www.melvilletheatre.com/articles/ncurses-extended-characters/index.html
;; * http://tldp.org/HOWTO/NCURSES-Programming-HOWTO/misc.html
;; 
;; Example: (acs 'ULCORNER)
(defun acs (char-name)
  "Take a symbol, return the integer representing the acs char."
  (mem-aref acs-map-array :uint64 (char-code (cdr (assoc char-name acs-alist)))))
;; TODO: how to decide the integer size automatically?

;;; TODOs

;; [ ] add y,x to echo-char?
;; [ ] add type asserts.
;; [ ] add line graphics characters as descibed in the man page.
;; [ ] use char-code to add a lisp char, i.e. to convert it first to C (int) and then add it.
