(in-package :de.anvi.ansi-escape)

;;; Basic terminal control functions based on 7bit escape sequences
;;; according to ANSI X3.64 / ECMA 48 / ISO/IEC 6429 / VT10X / XTerm

;; ECMA-6: 7bit character set 0-127
;; ECMA-35: Bit notation 01/07
;; ECMA-48: ANSI escape sequences

;; 1-char 7bit controls C0
;; 1-char 8bit controls C1
;; escape sequences
;; 7bit CSI sequences
;; 8bit CSI sequences

;; Acronym Character Decimal Octal  Hexadecimal Code
;; DEL     #\rubout  127     #o177  #x7f        07/15
;; ESC     #\esc      27     #o33   #x1b        01/11
;; SP      #\space    32     #o40   #x20        02/00

;; code x/y = column/line
;; 7bit code table = x-column 0-7 / y-line 0-15

;; x/y:        x       y 
;; Bit:    7 6 5 4 3 2 1
;; Weight: 4 2 1 8 4 2 1

;; 200530 add a stream argument to every function
;; add windows as gray streams

;;(defmacro define-control-function ())
;;(defmacro define-control-sequence (name args))

;; ESC [ Pn1 ; Pn2 H
;; CSI Pn1 ; Pn2 H
;; CSI n ; m H
;; CUP
;; cursor-position

;; TODO 200530 write csi in terms of esc?
;; no because CSI params are separated with ; while esc params arent separated

;; See 5.4 for the overall format of control sequences

;; Set:        C1
;; Section:    8.3.16
;; Name:       Control Sequence Introducer
;; Mnemonic:   CSI
;; 7bit Chars: ESC [
;; 7bit Byte:  01/11 05/11
;; 8bit Byte:  09/11 (not used here)
(defparameter *csi* (coerce (list #\esc #\[) 'string)
  "A two-character string representing the 7bit control sequence introducer CSI.")

(defun esc (&rest params)
  "Write an ESC control sequence. The parameters are not separated."
  (format t "~A~{~A~}" #\esc params))

(defun csi (final-char &rest params)
  "Write a CSI control sequence. The params are separated by a semicolon."
  ;; only the params are separated with ; the other chars are not separated.
  ;; ~^; = add ; to every list item except the last
  (format t "~A~{~A~^;~}~A" *csi* params final-char))

;; Sequence Syntax
;; C   A single character
;; Ps  A single numeric parameter
;; Pm  Several numeric parameters Ps separated by a semicolon ;

;;; ESC sequences ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Name:       Reset to initial state
;; Mnemonic:   RIS
;; Final char: c
;; Final byte: 06/03
;; Sequence:   ESC c
;; Parameters: none
;; Default:    none
;; Reference:  ANSI 5.72, ECMA 8.3.105
(defun reset-to-initial-state ()
  "Reset the terminal to its initial state.

In particular, turn on cooked and echo modes and newline translation,
turn off raw and cbreak modes, reset any unset special characters.

A reset is useful after a program crashes and leaves the terminal in
an undefined, unusable state."
  (esc "c"))

(setf (fdefinition 'ris) #'reset-to-initial-state)

;;; CSI sequences ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Cursor control functions

;; Name:       Cursor up
;; Mnemonic:   CUU
;; Final char: A
;; Final byte: 04/01
;; Sequence:   CSI Pn A
;; Parameters: Pn = m
;; Default:    Pn = 1
;; Reference:  ANSI 5.17, ECMA 8.3.22
(defun cursor-up (&optional (m 1))
  "Move the cursor m lines up."
  (csi "A" m))

(setf (fdefinition 'cuu) #'cursor-up)

;; Name:       Cursor down
;; Mnemonic:   CUD
;; Final char: B
;; Final byte: 04/02
;; Sequence:   CSI Pn B
;; Parameters: Pn = m
;; Default:    Pn = 1
;; Reference:  ANSI 5.14, ECMA 8.3.19
(defun cursor-down (&optional (m 1))
  "Move the cursor m lines down."
  (csi "B" m))

(setf (fdefinition 'cud) #'cursor-down)

;; Name:       Cursor forward
;; Mnemonic:   CUF
;; Final char: C
;; Final byte: 04/03
;; Sequence:   CSI Pn C
;; Parameters: Pn = n
;; Default:    Pn = 1
;; Reference:  ANSI 5.15, ECMA 8.3.20
;; Notice:     ECMA name: Cursor right
(defun cursor-forward (&optional (n 1))
  "Move the cursor n columns in the forward direction (to the right)."
  (csi "C" n))

(setf (fdefinition 'cuf) #'cursor-forward)

;; Name:       Cursor backward
;; Mnemonic:   CUB
;; Final char: D
;; Final byte: 04/04
;; Sequence:   CSI Pn D
;; Parameters: Pn = n
;; Default:    Pn = 1
;; Reference:  ANSI 5.13, ECMA 8.3.18
;; Notice:     ECMA name: Cursor left
(defun cursor-backward (&optional (n 1))
  "Move the cursor n columns in the backward direction (to the left)."
  (csi "D" n))

(setf (fdefinition 'cub) #'cursor-backward)

;; Name:       Cursor next line
;; Mnemonic:   CNL
;; Final char: E
;; Final byte: 04/05
;; Sequence:   CSI Pn E
;; Parameters: Pn = m
;; Default:    Pn = 1
;; Reference:  ANSI 5.7, ECMA 8.3.12
(defun cursor-next-line (&optional (m 1))
  "Move the cursor m columns down to column 1."
  (csi "E" m))

(setf (fdefinition 'cnl) #'cursor-next-line)

;; Name:       Cursor preceding line
;; Mnemonic:   CPL
;; Final char: F
;; Final byte: 04/06
;; Sequence:   CSI Pn F
;; Parameters: Pn = m
;; Default:    Pn = 1
;; Reference:  ANSI 5.8, ECMA 8.3.13
(defun cursor-preceding-line (&optional (m 1))
  "Move the cursor m columns up to column 1."
  (csi "F" m))

(setf (fdefinition 'cpl) #'cursor-preceding-line)

;; Name:       Cursor horizontal absolute
;; Mnemonic:   CHA
;; Final char: G
;; Final byte: 04/07
;; Sequence:   CSI Pn G
;; Parameters: Pn = n
;; Default:    Pn = 1
;; Reference:  ANSI 5.5, ECMA 8.3.9
;; Notice:     ECMA name: Cursor character absolute
(defun cursor-horizontal-absolute (&optional (n 1))
  "Set the cursor horizontal position to the n-th column in the current line."
  (csi "G" n))

(setf (fdefinition 'cha) #'cursor-horizontal-absolute)

;; Name:       Cursor position
;; Mnemonic:   CUP
;; Final char: H
;; Final byte: 04/08
;; Sequence:   CSI Pn1 ; Pn2 H
;; Parameters: Pn1 = m line, Pn2 = n column
;; Defaults:   Pn1 = 1; Pn2 = 1
;; Reference:  ANSI 5.16, ECMA 8.3.21
(defun cursor-position (&optional (line 1) (column 1))
  "Move the cursor to m-th line and n-th column of the screen.

The line and column numbering is one-based.

Without arguments, the cursor is placed in the home position (1 1),
the top left corner."
  (csi "H" line column))

(setf (fdefinition 'cup) #'cursor-position)

;; Name:       Vertical position absolute
;; Mnemonic:   VPA
;; Final char: d
;; Final byte: 06/04
;; Sequence:   CSI Pn d
;; Parameters: Pn = m
;; Default:    Pn = 1
;; Reference:  ANSI 5.96, ECMA 8.3.158
;; Notice:     ECMA name: Line position absolute
(defun vertical-position-absolute (&optional (m 1))
  "Set the cursor vertical position to the m-th line in the current column."
  (csi "d" m))

(setf (fdefinition 'vpa) #'vertical-position-absolute)

;; Name:       Vertical position relative
;; Mnemonic:   VPR
;; Final char: e
;; Final byte: 06/05
;; Sequence:   CSI Pn e
;; Parameters: Pn = m
;; Default:    Pn = 1
;; Reference:  ANSI 5.97, ECMA 8.3.160
;; Notice:     ECMA name: Line position forward
(defun vertical-position-relative (&optional (m 1))
  "Move the cursor vertical position down by m lines in the current column.

This has the same effect as cursor-down (cud)."
  (csi "e" m))

(setf (fdefinition 'vpr) #'vertical-position-relative)

;; Name:       Vertical position backward
;; Mnemonic:   VPB
;; Final char: k
;; Final byte: 06/11
;; Sequence:   CSI Pn k
;; Parameters: Pn = m
;; Default:    Pn = 1
;; Reference:  ECMA 8.3.159
;; Notice:     ECMA name: Line position backward
(defun vertical-position-backward (&optional (m 1))
  "Move the cursor vertical position up by m lines in the current column.

This has the same effect as cursor-up (cuu)."
  (csi "k" m))

(setf (fdefinition 'vpb) #'vertical-position-backward)

(defun save-cursor-position ()
  "Save cursor position. Move cursor to the saved position using restore-cursor-position."
  (csi "s"))

(setf (fdefinition 'scosc) #'save-cursor-position)

(defun restore-cursor-position ()
  "Move cursor to the position saved using save-cursor-position."
  (csi "u"))

(setf (fdefinition 'scorc) #'restore-cursor-position)

;; Name:       Erase in display
;; Mnemonic:   ED
;; Final char: J
;; Final byte: 04/10
;; Sequence:   CSI Ps J
;; Parameters: Ps = mode
;; Defaults:   Ps = 0
;; Reference:  ANSI 5.29, ECMA 8.3.39
;; Notice:     ECMA name: Erase in page
(defun erase-in-display (&optional (mode 0))
  "Erase some or all characters on the screen depending on the selected mode.

Mode 0 (erase-below, default) erases all characters from the cursor to
the end of the screen.

Mode 1 (erase-above) erases all characters from the beginning of the
screen to the cursor.

Mode 2 (erase) erases all characters on the screen.

Mode 3 (erase-saved-lines, xterm) erases all characters on the screen
including the scrollback buffer."
  (csi "J" mode))

(setf (fdefinition 'ed) #'erase-in-display)

(defun erase-below ()
  "Erases all characters from the cursor to the end of the screen."
  (erase-in-display 0))

(defun erase-above ()
  "Erases all characters from the beginning of the screen to the cursor."
  (erase-in-display 1))

(defun erase ()
  "Erase all characters on the screen."
  (erase-in-display 2))

(defun erase-saved-lines ()
  "Erase all characters on the screen including the scrollback buffer."
  (erase-in-display 3))

;; Name:       Erase in line
;; Mnemonic:   EL
;; Final char: K
;; Final byte: 04/11
;; Sequence:   CSI Ps K
;; Parameters: Ps = mode
;; Defaults:   Ps = 0
;; Reference:  ANSI 5.31, ECMA 8.3.41
(defun erase-in-line (&optional (mode 0))
  "Erase some or all characters on the current line depending on the selected mode.

Mode 0 (erase-right, default) erases all characters from the cursor to
the end of the line.

Mode 1 (erase-left) erases all characters from the beginning of the
line to the cursor.

Mode 2 (erase-line) erases all characters on the line."
  (csi "K" mode))

(setf (fdefinition 'el) #'erase-in-line)

(defun erase-right ()
  "Erases all characters from the cursor to the end of the line."
  (erase-in-line 0))

(defun erase-left ()
  "Erases all characters from the beginning of the line to the cursor."
  (erase-in-line 1))

(defun erase-line ()
  "Erases all characters on the current line."
  (erase-in-line 2))

;; Name:        Select Graphic Rendition
;; Mnemonic:    SGR
;; Final char:  m
;, Final byte:  06/13
;; Sequence:    CSI Pm m
;; Parameters:  See documentation string. 
;; Defaults:    Pm = 0
;; Reference:   ANSI 5.77, ECMA 8.3.117
(defun select-graphic-rendition (&rest params)
  "Set character attributes and foreground and background colors.

 0  turn off all previous attributes, set normal, default rendition

 1  bold, increased intensity
 2  faint, dim, decreased intensity
 3  italic, standout
 4  single underline
 5  slow blinking
 6  rapid blinking
 7  negative, reverse image
 8  invisible, hidden, concealed
 9  crossed-out
21  double underline

22  turn off bold and faint/dim, set normal intensity
23  turn off italic, standout
24  turn off single, double underline
25  turn off blinking
27  turn off negative, reverse image
28  turn off hidden, invisible
29  turn off crossed-out

Foreground colors:

30  black
31  red
32  green
33  yellow
34  blue
35  magenta
36  cyan
37  white
39  default foreground color

38 5 n      set the color n from a default 256-color palette
38 2 r g b  set the color by directly giving its RGB components

Background colors:

40  black
41  red
42  green
43  yellow
44  blue
45  magenta
46  cyan
47  white
49  default background color

48 5 n      set the color n from a default 256-color palette
48 2 r g b  set the color by directly giving its RGB components"
  (apply #'csi "m" params))

(setf (fdefinition 'sgr) #'select-graphic-rendition)

;; the terminal sends ^[[11;16R or ESC[n;mR to the application
;; as if we read it through read-line
(defun device-status-report ()
  (csi "6n"))

;;; DEC private mode

;; Set (enable, turn on) 

(defun dec-private-mode-set (mode)
  "Set (turn on, enable) a DEC private mode.

Implemented modes:

  25 show or hide the cursor
1047 alternate or normal screen buffer"
  (csi "h" "?" mode))

(setf (fdefinition 'decset) #'dec-private-mode-set)

(defun show-cursor ()
  (dec-private-mode-set 25))

(defun use-alternate-screen-buffer ()
  (dec-private-mode-set 1047))

;; Reset (disable, turn off)

(defun dec-private-mode-reset (mode)
  "Reset (turn off, disable) a DEC private mode."
  (csi "l" "?" mode))

(setf (fdefinition 'decrst) #'dec-private-mode-reset)

(defun hide-cursor ()
  (dec-private-mode-reset 25))

(defun use-normal-screen-buffer ()
  (dec-private-mode-reset 1047))
