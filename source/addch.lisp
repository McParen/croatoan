(in-package :de.anvi.croatoan)

;; (add scr #\a :y 10 :x 10)
;; (add scr "b" :y 11 :x 10)
;; (add scr #\a   :y 10 :x 10 :attributes '(:underline) :color-pair '(:yellow :red))
;; (add scr "bat" :y 11 :x 10 :attributes '(:underline :bold) :color-pair '(:black :green))
;; (add scr #\a :position (list 10 10))

(defun add (window object &rest keys &key &allow-other-keys)
  "Add the text object to the window, then advance the cursor.

Currently supported text objects are characters (simple and complex),
characters given by integer codes or keywords, and strings 
(simple and complex).

If the position coordinates y (row) and x (column) are given, move the
cursor to the position first and then add the object.

The position can also be passed in form of a two-element list.

If n is given for a char, write n chars. If n is -1, add as many chars
as will fit on the line.

If n is given for a string, add at most n chars from the string.
If n is -1, add as many chars from the string as will fit on the line."
  (let ((fn (typecase object
              ((or string complex-string)
               #'add-string)
              ((or integer keyword character complex-char)
               #'add-wide-char))))   
     (apply fn window object keys)))

(defun distance-to-eol (window)
  "Return the number of columns from the cursor position to the end of the line in the window."
  (- (width window) (cadr (cursor-position window))))

(defun distance-to-bottom (window)
  "Return the number of lines from the cursor position to the bottom of the window."
  (- (height window) (car (cursor-position window))))

(defun add-char (window char &key attributes color-pair y x position n)
  "Add the narrow (single-byte) char to the window, then advance the cursor.

If the position coordinates y (row) and x (column) are given, move the
cursor to the position first and then add the character.

The position can also be passed in form of a two-element list.

If n is given for a char, write n chars. If n is -1, add as many chars
as will fit on the line.

Example: 

(add-char scr #\a :attributes '(:bold) :color-pair '(:red :yellow))"
  (when (and y x) (move window y x))
  (when position (apply #'move window position))
  (let ((winptr (winptr window))
        (count (if n
                   (if (= n -1)
                       (distance-to-eol window)
                       n)
                   1))
        (chtype (make-chtype char attributes color-pair)))
    (loop
       repeat count
       do (%waddch winptr chtype))))

(defun echo (window char &key attributes color-pair y x position)
  "Add one narrow (single-byte) character to the window, then refresh the window.

If the position coordinates y (row) and x (column) are given, move the
cursor to the position first and then echo the character.

The position can also be passed in form of a two-element list.

The only difference to add-char and a subsequent refresh is a
performance gain if we know that we only need to output a single
character."
  (when (and y x) (move window y x))
  (when position (apply #'move window position))
  (let ((winptr (winptr window))
        (chtype (make-chtype char attributes color-pair)))
    (typecase window
      ;; a pad is a subclass of window, therefore we have to check pad first.
      (pad (%pechochar winptr chtype))
      (window (%wechochar winptr chtype)))))

;; just an utility function if you dont want to use (format nil "bla
;; bla ~%") to insert newlines. in C you can simply insert \n.
(defun new-line (window &optional (count 1))
  "Insert count newline characters into window."
  (loop repeat count do (add-char window (char-code #\newline))))

;; pointer to the global/external c acs array, acs_map[].
;; also see defcvar + get-var-pointer
(defparameter acs-map-array (foreign-symbol-pointer "acs_map"))

;; ncurses maps those standard chars at runtime to the acs characters.
;; here we use it in the function acs.
(defparameter acs-alist
  ;; VT100 symbols
  '(( :upper-left-corner     . #\l )
    ( :lower-left-corner     . #\m )
    ( :upper-right-corner    . #\k )
    ( :lower-right-corner    . #\j )
    ( :tee-pointing-left     . #\u )
    ( :tee-pointing-right    . #\t )
    ( :tee-pointing-up       . #\v )
    ( :tee-pointing-down     . #\w )
    ( :horizontal-line       . #\q )
    ( :vertical-line         . #\x )
    ( :crossover-plus        . #\n )

    ( :scan-line-1           . #\o )
    ( :scan-line-9           . #\s )
    ( :diamond-symbol        . #\` )
    ( :checker-board         . #\a )
    ( :degree-symbol         . #\f )
    ( :plus-minus            . #\g )
    ( :bullet-symbol         . #\~ )

    ;; Teletype 5410v1 symbols
    ( :arrow-pointing-left   . #\, )
    ( :arrow-pointing-right  . #\+ )
    ( :arrow-pointing-down   . #\. )
    ( :arrow-pointing-up     . #\- )
    ( :board                 . #\h )
    ( :lantern-symbol        . #\i )
    ( :solid-square-block    . #\0 )

    ;; ncurses characters
    ( :scan-line-3           . #\p )
    ( :scan-line-7           . #\r )
    ( :less-than-or-equal    . #\y )
    ( :greater-than-or-equal . #\z )
    ( :pi                    . #\{ )
    ( :not-equal             . #\| )
    ( :uk-pound-sterling     . #\} )

    ;; thick line drawing characters
    ( :thick-upper-left-corner      . #\L )
    ( :thick-lower-left-corner      . #\M )
    ( :thick-upper-right-corner     . #\K )
    ( :thick-lower-right-corner     . #\J )
    ( :thick-tee-pointing-left      . #\U )
    ( :thick-tee-pointing-right     . #\T )
    ( :thick-tee-pointing-up        . #\V )
    ( :thick-tee-pointing-down      . #\W )
    ( :thick-horizontal-line        . #\Q )
    ( :thick-vertical-line          . #\X )
    ( :thick-crossover-plus         . #\N )

    ;; double-line drawing characters
    ( :double-upper-left-corner     . #\C )
    ( :double-lower-left-corner     . #\D )
    ( :double-upper-right-corner    . #\B )
    ( :double-lower-right-corner    . #\A )
    ( :double-tee-pointing-left     . #\G )
    ( :double-tee-pointing-right    . #\F )
    ( :double-tee-pointing-up       . #\H )
    ( :double-tee-pointing-down     . #\I )
    ( :double-horizontal-line       . #\R )
    ( :double-vertical-line         . #\Y )
    ( :double-crossover-plus        . #\E )))

#|

For 64bit builds of ncurses 6.0, chtype is an unsigned int:

#if 1 && defined(_LP64)
typedef unsigned chtype;
typedef unsigned mmask_t;
#else
typedef uint32_t chtype;
typedef uint32_t mmask_t;
#endif

For 64bit builds of ncurses 5.9, chtype is an unsigned long:

#if 0 && defined(_LP64)
typedef unsigned chtype;
typedef unsigned mmask_t;
#else
typedef unsigned long chtype;
typedef unsigned long mmask_t;
#endif

acs_map[] is an chtype array:

#if 0 || NCURSES_REENTRANT
NCURSES_WRAPPED_VAR(chtype*, acs_map);
#define acs_map NCURSES_PUBLIC_VAR(acs_map())
#else
extern NCURSES_EXPORT_VAR(chtype) acs_map[];
#endif

|#

;; ACS, the alternative/extended character set for line drawing.
;; Used by functions: add-char, box and border.
;; 
;; * http://www.melvilletheatre.com/articles/ncurses-extended-characters/index.html
;; * http://tldp.org/HOWTO/NCURSES-Programming-HOWTO/misc.html
;; 
;; Example: (acs 'ULCORNER)
(defun acs (char-name)
  "Take a symbol, return the integer representing the acs char."
  (mem-aref acs-map-array 'chtype (char-code (cdr (assoc char-name acs-alist)))))
