(in-package :de.anvi.croatoan)

(defun add-wide-char-utf-8 (window char &key attributes color-pair y x n)
  "Add the wide (multi-byte) char to the window, then advance the cursor.

If the destination coordinates y and x are given, move the cursor to the
destination first and then add the character.

If n is given, write n chars. If n is -1, as many chars will be added
as will fit on the line."
  (when (and y x) (move window y x))
  (let ((count  (if n
                   (if (= n -1)
                       (distance-to-eol window)
                       n)
                   1))
        (code-point (typecase char
                      (integer char)
                      (character (char-code char)))))
    (typecase char
      (complex-char
       ;; if we have a complex char, use its own attributes and colors.
       (loop repeat count do
            (mapc #'(lambda (ch) (add-char window ch :attributes (.attributes char) :color-pair (.color-pair char)))
                  (unicode-to-utf-8 (char-code (.simple-char char))))))
      ;; if we have a lisp char or an integer, use the attributes and colors passed as arguments.
      (t
       (loop repeat count do
            (mapc #'(lambda (ch) (add-char window ch :attributes attributes :color-pair color-pair))
                  (unicode-to-utf-8 code-point)))))))

;; we can use this for add-wide-char, echo-wide-char, insert-wide-char, set-wide-background-char
(defun funcall-make-cchar_t-ptr (fn winptr char attr_t color-pair-number count)
  "Create a cchar_t and apply function fn count times to winptr and cchar_t.

cchar_t is a C struct representing a wide complex-char in ncurses.

This function is a wrapper around %setcchar and should not be used elsewhere."
  (with-foreign-objects ((ptr '(:struct cchar_t))
                         (wch 'wchar_t 5))
    (dotimes (i 5) (setf (mem-aref wch 'wchar_t i) 0))
    (setf (mem-aref wch 'wchar_t) char)
    (%setcchar ptr wch attr_t color-pair-number (null-pointer))
    (if (= count 1)
        (funcall fn winptr ptr)
        (dotimes (i count) (funcall fn winptr ptr)) )))

(defun funcall-make-cchar_t (fn window char attributes color-pair n)
  "Assemble a cchar_t out of a char, attributes and a color-pair.

char can be a lisp character, an ACS keyword, an integer code point or
a complex char.

attributes should be a list of valid attribute keywords.

color-pair should be a list of a foreground and background color keyword.

attributes and color-pair can be nil.

If char is a complex char, attributes and color-pair are ignored."
  (let ((winptr (.winptr window))
        (ch
         (typecase char
           ;; if we have a lisp char or an integer, use the attributes and colors passed as arguments.
           (integer char)
           (character (char-code char))
           (keyword (wacs char))
           ;; if we have a complex char, use its own attributes and colors.
           (complex-char (if (.simple-char char)
                             (let ((sch (.simple-char char)))
                               (typecase sch
                                 (integer sch)
                                 (character (char-code sch))
                                 (keyword (wacs sch))
                                 (otherwise (error "unknown character type"))))
                             ;; this means that the default simple char is space, otherwise
                             ;; we can not set complex background chars.
                             ;; TODO: set this here or as initform for complex-char?
                             32))
           (otherwise (error "unknown character type"))))
        (attr_t
         (typecase char
           (complex-char (attrs2chtype (.attributes char)))
           (otherwise    (attrs2chtype attributes))))
        ;; we just need the pair number here, NOT the bit-shifted color attribute.
        ;; we need the color attribute for chtypes.
        (color-pair-number
         (typecase char
           (complex-char (if (.color-pair char) (pair->number (.color-pair char)) 0))
           (otherwise    (if color-pair         (pair->number color-pair)         0))))
        (count (if n
                   (if (= n -1)
                       (distance-to-eol window)
                       n)
                   1)))
    ;; After the parameters are assembled, call the lower-level function that actually
    ;; uses %setcchar to create a cchar_t pointer and passes it to fn.
    (funcall-make-cchar_t-ptr fn winptr ch attr_t color-pair-number count)))

(defun add-wide-char (window char &key attributes color-pair y x n)
  "Add the wide (multi-byte) char to the window, then advance the cursor.

If the destination coordinates y and x are given, move the cursor to the
destination first and then add the character.

If n is given, write n chars. If n is -1, as many chars will be added
as will fit on the line."
  (when (and y x) (move window y x))
  (funcall-make-cchar_t #'%wadd-wch window char attributes color-pair n))

(defun echo-wide-char (window char &key attributes color-pair y x)
  "Add one wide (multi-byte) character to the window, then refresh the window.

If the destination coordinates Y and X are given, move to the
destination first and then echo the character. 

The only difference to add-wide-char and a subsequent refresh is a
performance gain if we know that we only need to output a single
character."
  (when (and y x) (move window y x))
  (let ((count 1)
        ;; for some reason, there is a special echo function for pads.
        (fn (typecase window
              (pad #'%pecho-wchar)
              (window #'%wecho-wchar))))
    (funcall-make-cchar_t fn window char attributes color-pair count)))

;; wide-char equivalents of the ACS chars.
;; since reading _nc_wacs doesnt work like it worked with acs_map,
;; plan B is a direct translation from ACS names to unicode code points.
;; source for the codes is ncurses/widechar/lib_wacs.c
(defparameter wide-acs-alist
  ;; VT100 symbols
  '(( :upper-left-corner     . #x250C )       ; #\BOX_DRAWINGS_LIGHT_DOWN_AND_RIGHT           / 0xE2 0x94 0x8C
    ( :lower-left-corner     . #x2514 )       ; #\BOX_DRAWINGS_LIGHT_UP_AND_RIGHT             / 0xE2 0x94 0x94
    ( :upper-right-corner    . #x2510 )       ; #\BOX_DRAWINGS_LIGHT_DOWN_AND_LEFT            / 0xE2 0x94 0x90
    ( :lower-right-corner    . #x2518 )       ; #\BOX_DRAWINGS_LIGHT_UP_AND_LEFT              / 0xE2 0x94 0x98
    ( :tee-pointing-left     . #x2524 )       ; #\BOX_DRAWINGS_LIGHT_VERTICAL_AND_LEFT        / 0xE2 0x94 0xA5
    ( :tee-pointing-right    . #x251C )       ; #\BOX_DRAWINGS_LIGHT_VERTICAL_AND_RIGHT       / 0xE2 0x94 0x9C
    ( :tee-pointing-up       . #x2534 )       ; #\BOX_DRAWINGS_LIGHT_UP_AND_HORIZONTAL        / 0xE2 0x94 0xB4
    ( :tee-pointing-down     . #x252C )       ; #\BOX_DRAWINGS_LIGHT_DOWN_AND_HORIZONTAL      / 0xE2 0x94 0xAC
    ( :horizontal-line       . #x2500 )       ; #\BOX_DRAWINGS_LIGHT_HORIZONTAL               / 0xE2 0x94 0x80
    ( :vertical-line         . #x2502 )       ; #\BOX_DRAWINGS_LIGHT_VERTICAL                 / 0xE2 0x94 0x82
    ( :crossover-plus        . #x253C )       ; #\BOX_DRAWINGS_LIGHT_VERTICAL_AND_HORIZONTAL  / 0xE2 0x94 0xBC

    ( :scan-line-1           . #x23BA )       ; #\HORIZONTAL_SCAN_LINE-1                      / 0xE2 0x8E 0xBA
    ( :scan-line-9           . #x23BD )       ; #\HORIZONTAL_SCAN_LINE-9                      / 0xE2 0x8E 0xBD
    ( :diamond-symbol        . #x25C6 )       ; #\BLACK_DIAMOND                               / 0xE2 0x97 0x86
    ( :checker-board         . #x2592 )       ; #\MEDIUM_SHADE                                / 0xE2 0x96 0x92 
    ( :degree-symbol         . #x00B0 )       ; #\DEGREE_SIGN                                 / 0xC2 0xB0
    ( :plus-minus            . #x00B1 )       ; #\PLUS-MINUS_SIGN                             / 0xC2 0xB1
    ( :bullet-symbol         . #x00B7 )       ; #\MIDDLE_DOT                                  / 0xC2 0xB7

    ;; Teletype 5410v1 symbols
    ( :arrow-pointing-left   . #x2190 )       ; #\LEFTWARDS_ARROW                             / 0xE2 0x86 0x90
    ( :arrow-pointing-right  . #x2192 )       ; #\RIGHTWARDS_ARROW                            / 0xE2 0x86 0x92
    ( :arrow-pointing-down   . #x2193 )       ; #\DOWNWARDS_ARROW                             / 0xE2 0x86 0x93
    ( :arrow-pointing-up     . #x2191 )       ; #\UPWARDS_ARROW                               / 0xE2 0x86 0x91
    ( :board                 . #x2592 )       ; #\MEDIUM_SHADE                                / 0xE2 0x96 0x92
    ( :lantern-symbol        . #x2603 )       ; #\SNOWMAN                                     / 0xE2 0x98 0x83
    ( :solid-square-block    . #x25AE )       ; #\BLACK_VERTICAL_RECTANGLE                    / 0xE2 0x96 0xAE

    ;; ncurses characters
    ( :scan-line-3           . #x23BB )       ; #\HORIZONTAL_SCAN_LINE-3                      / 0xE2 0x8E 0xBB
    ( :scan-line-7           . #x23BC )       ; #\HORIZONTAL_SCAN_LINE-7                      / 0xE2 0x8E 0xBC
    ( :less-than-or-equal    . #x2264 )       ; #\LESS-THAN_OR_EQUAL_TO                       / 0xE2 0x89 0xA4
    ( :greater-than-or-equal . #x2265 )       ; #\GREATER-THAN_OR_EQUAL_TO                    / 0xE2 0x89 0xA5
    ( :pi                    . #x03C0 )       ; #\GREEK_SMALL_LETTER_PI                       / 0xCF 0x80
    ( :not-equal             . #x2260 )       ; #\NOT_EQUAL_TO                                / 0xE2 0x89 0xA0
    ( :uk-pound-sterling     . #x00A3 )       ; #\POUND_SIGN                                  / 0xC2 0xA3

    ;; thick line drawing characters
    ( :thick-upper-left-corner   . #x250F )   ; #\BOX_DRAWINGS_HEAVY_DOWN_AND_RIGHT           / 0xE2 0x94 0x8F
    ( :thick-lower-left-corner   . #x2517 )   ; #\BOX_DRAWINGS_HEAVY_UP_AND_RIGHT             / 0xE2 0x94 0x97
    ( :thick-upper-right-corner  . #x2513 )   ; #\BOX_DRAWINGS_HEAVY_DOWN_AND_LEFT            / 0xE2 0x94 0x93
    ( :thick-lower-right-corner  . #x251B )   ; #\BOX_DRAWINGS_HEAVY_UP_AND_LEFT              / 0xE2 0x94 0x9B
    ( :thick-tee-pointing-left   . #x2523 )   ; #\BOX_DRAWINGS_HEAVY_VERTICAL_AND_LEFT        / 0xE2 0x94 0xA3
    ( :thick-tee-pointing-right  . #x252B )   ; #\BOX_DRAWINGS_HEAVY_VERTICAL_AND_RIGHT       / 0xE2 0x94 0xAB
    ( :thick-tee-pointing-up     . #x253B )   ; #\BOX_DRAWINGS_HEAVY_UP_AND_HORIZONTAL        / 0xE2 0x94 0xBB
    ( :thick-tee-pointing-down   . #x2533 )   ; #\BOX_DRAWINGS_HEAVY_DOWN_AND_HORIZONTAL      / 0xE2 0x94 0xB3
    ( :thick-horizontal-line     . #x2501 )   ; #\BOX_DRAWINGS_HEAVY_HORIZONTAL               / 0xE2 0x94 0x81
    ( :thick-vertical-line       . #x2503 )   ; #\BOX_DRAWINGS_HEAVY_VERTICAL                 / 0xE2 0x94 0x83
    ( :thick-crossover-plus      . #x254B )   ; #\BOX_DRAWINGS_HEAVY_VERTICAL_AND_HORIZONTAL  / 0xE2 0x95 0x8B

    ;; double-line drawing characters
    ( :double-upper-left-corner  . #x2554 )   ; #\BOX_DRAWINGS_DOUBLE_DOWN_AND_RIGHT          / 0xE2 0x95 0x94
    ( :double-lower-left-corner  . #x255A )   ; #\BOX_DRAWINGS_DOUBLE_UP_AND_RIGHT            / 0xE2 0x95 0x9A
    ( :double-upper-right-corner . #x2557 )   ; #\BOX_DRAWINGS_DOUBLE_DOWN_AND_LEFT           / 0xE2 0x95 0x97
    ( :double-lower-right-corner . #x255D )   ; #\BOX_DRAWINGS_DOUBLE_UP_AND_LEFT             / 0xE2 0x95 0x9D
    ( :double-tee-pointing-left  . #x2563 )   ; #\BOX_DRAWINGS_DOUBLE_VERTICAL_AND_LEFT       / 0xE2 0x95 0xA3
    ( :double-tee-pointing-right . #x2560 )   ; #\BOX_DRAWINGS_DOUBLE_VERTICAL_AND_RIGHT      / 0xE2 0x95 0xA0
    ( :double-tee-pointing-up    . #x2569 )   ; #\BOX_DRAWINGS_DOUBLE_UP_AND_HORIZONTAL       / 0xE2 0x95 0xA9
    ( :double-tee-pointing-down  . #x2566 )   ; #\BOX_DRAWINGS_DOUBLE_DOWN_AND_HORIZONTAL     / 0xE2 0x95 0xA6
    ( :double-horizontal-line    . #x2550 )   ; #\BOX_DRAWINGS_DOUBLE_HORIZONTAL              / 0xE2 0x95 0x90
    ( :double-vertical-line      . #x2551 )   ; #\BOX_DRAWINGS_DOUBLE_VERTICAL                / 0xE2 0x95 0x91
    ( :double-crossover-plus     . #x256C ))) ; #\BOX_DRAWINGS_DOUBLE_VERTICAL_AND_HORIZONTAL / 0xE2 0x95 0xAC

(defun wacs (char-name)
  "Take a keyword symbol, return the wide unicode integer representing the ACS char."
  (cdr (assoc char-name wide-acs-alist)))
