(in-package :de.anvi.ansi-escape)

;;; Basic terminal control functions based on 7bit escape sequences
;;; according to ANSI X3.64 / ECMA 48 / ISO/IEC 6429 / VT10X / XTerm

;; https://github.com/pnathan/cl-ansi-text
;; https://github.com/vindarel/cl-ansi-term
;; http://wiki.call-cc.org/eggref/5/ansi-escape-sequences
;; https://bluesock.org/~willkg/dev/ansi.html
;; http://www.lihaoyi.com/post/BuildyourownCommandLinewithANSIescapecodes.html
;; http://www.termsys.demon.co.uk/vtansi.htm
;; https://wiki.bash-hackers.org/scripting/terminalcodes
;; https://github.com/thrig/cl-minterm/blob/master/minterm.lisp
;; http://ascii-table.com/ansi-escape-sequences.php
;; https://www.man7.org/linux/man-pages/man4/console_codes.4.html
;; https://invisible-island.net/xterm/ctlseqs/ctlseqs.html
;; https://stackoverflow.com/questions/4842424/list-of-ansi-color-escape-sequences
;; https://vt100.net/docs/vt510-rm/chapter4.html
;; https://www.gnu.org/software/screen/manual/html_node/Control-Sequences.html
;; http://bitsavers.trailing-edge.com/pdf/tektronix/410x/070-4526-01_4105_PgmrRef_Sep83.pdf
;; https://ttssh2.osdn.jp/manual/4/en/about/ctrlseq.html

;; http://man7.org/linux/man-pages/man4/console_codes.4.html
;; Based initially on http://en.wikipedia.org/wiki/ANSI_escape_code,
;; but it doesn't have very many definitions; this page, however, is
;; comprehensive and excellent:
;; http://bjh21.me.uk/all-escapes/all-escapes.txt
;; http://www.xfree86.org/current/ctlseqs.html

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

;; code x/y = column/row
;; 7bit code table = x-column 0-7 / y-row 0-15

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

;;; 8.2.7 Cursor control functions

;; Name:       Cursor up
;; Mnemonic:   CUU
;; Final char: A
;; Final byte: 04/01
;; Sequence:   CSI Pn A
;; Parameters: Pn = n
;; Default:    Pn = 1
(defun cursor-up (&optional (m 1))
  "Move the cursor m rows up."
  (csi "A" m))

(setf (fdefinition 'cuu) #'cursor-up)

;; Name:       Cursor down
;; Mnemonic:   CUD
;; Final char: B
;; Final byte: 04/02
;; Sequence:   CSI Pn B
;; Parameters: Pn = n
;; Default:    Pn = 1
(defun cursor-down (&optional (m 1))
  "Move the cursor m rows down."
  (csi "B" m))

(setf (fdefinition 'cud) #'cursor-down)

;; Name:       Cursor right
;; Mnemonic:   CUF
;; Final char: C
;; Final byte: 04/03
;; Sequence:   CSI Pn C
;; Parameters: Pn = n
;; Default:    Pn = 1
(defun cursor-right (&optional (n 1))
  "Move the cursor n columns to the right."
  (csi "C" n))

(setf (fdefinition 'cuf) #'cursor-right)

;; Name:       Cursor left
;; Mnemonic:   CUB
;; Final char: D
;; Final byte: 04/04
;; Sequence:   CSI Pn D
;; Parameters: Pn = n
;; Default:    Pn = 1
(defun cursor-left (&optional (n 1))
  "Move the cursor n columns to the left."
  (csi "D" n))

(setf (fdefinition 'cub) #'cursor-left)

;; Name:       Cursor position
;; Mnemonic:   CUP
;; Final char: H
;; Final byte: 04/08
;; Sequence:   CSI Pn1 ; Pn2 H
;; Parameters: Pn1 = m line, Pn2 = n column
;; Defaults:   Pn1 = 1; Pn2 = 1
(defun cursor-position (&optional (line 1) (column 1))
  "Move the cursor to m-th line and n-th column of the screen.

The line and column numbering is one-based."
  (csi "H" line column))

(setf (fdefinition 'cup) #'cursor-position)

;; Name:       Erase in display
;; Mnemonic:   ED
;; Final char: J
;; Final byte: 04/10
;; Sequence:   CSI Ps J
;; Parameters: Ps = mode
;; Defaults:   Ps = 0
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
  (erase-in-line 0))

(defun erase-left ()
  (erase-in-line 1))

(defun erase-line ()
  (erase-in-line 2))

;; Name:        Select Graphic Rendition
;; Mnemonic:    SGR
;; Final char:  m
;, Final byte:  06/13
;; Sequence:    CSI Pm m
;; Parameters:  See documentation string. 
;; Defaults:    Pm = 0
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
