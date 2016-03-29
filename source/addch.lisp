(in-package :de.anvi.croatoan)

;; Example: (add-char scr #\a :attributes '(:bold) :color-pair '(:red :yellow))
(defun add-char (window char &key attributes color-pair y x)
  "Add the char to the window, then advance the cursor.

If the destination coordinates y and x are given, move the cursor to the
destination first and then add the character."
  (let ((winptr (.winptr window))
        (chtype (char2chtype char attributes color-pair)))
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
  '(( :upper-left-corner     . #\l )
    ( :lower-left-corner     . #\m )
    ( :upper-right-corner    . #\k )
    ( :lower-right-corner    . #\j )
    ( :tee-pointing-right    . #\t )
    ( :tee-pointing-left     . #\u )
    ( :tee-pointing-up       . #\v )
    ( :tee-pointing-down     . #\w )
    ( :horizontal-line       . #\q )
    ( :vertical-line         . #\x )
    ( :crossover-plus        . #\n )
    ( :scan-line-1           . #\o )
    ( :scan-line-3           . #\p )
    ( :scan-line-7           . #\r )
    ( :scan-line-9           . #\s )
    ( :diamond-symbol        . #\` )
    ( :board                 . #\h )
    ( :checker-board         . #\a )
    ( :degree-symbol         . #\f )
    ( :plus-minus            . #\g )
    ( :bullet-symbol         . #\~ )
    ( :arrow-pointing-left   . #\, )
    ( :arrow-pointing-right  . #\+ )
    ( :arrow-pointing-down   . #\. )
    ( :arrow-pointing-up     . #\- )
    ( :lantern-symbol        . #\i )
    ( :solid-square-block    . #\0 )
    ( :less-than-or-equal    . #\y )
    ( :greater-than-or-equal . #\z )
    ( :pi                    . #\{ )
    ( :not-equal             . #\| )
    ( :uk-pound-sterling     . #\} )))

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
  (if (string= (subseq (%curses-version) 8 11) "6.0")
      (mem-aref acs-map-array :unsigned-int (char-code (cdr (assoc char-name acs-alist))))
      (mem-aref acs-map-array :unsigned-long (char-code (cdr (assoc char-name acs-alist))))))

;; TODO: how to decide the integer size automatically?

;;; TODOs

;; [ ] add y,x to echo-char?
;; [ ] add type asserts.
;; [ ] add line graphics characters as descibed in the man page.
;; [ ] use char-code to add a lisp char, i.e. to convert it first to C (int) and then add it.
