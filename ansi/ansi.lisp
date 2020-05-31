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

;; https://www.man7.org/linux/man-pages/man4/console_codes.4.html
;; https://invisible-island.net/xterm/ctlseqs/ctlseqs.html
;; https://stackoverflow.com/questions/4842424/list-of-ansi-color-escape-sequences

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

;; CSI = ESC [
(defparameter *csi* (coerce (list #\esc #\[) 'string)
  "7bit CSI control sequence introducer.")

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

;; Section:    8.3.21
;; Name:       Cursor position
;; Mnemonic:   CUP
;; Final char: H
;; Final byte: 04/08
;; Sequence:   CSI Pn1 ; Pn2 H
;; Parameters: Pn1 = line, Pn2 = column
;; Defaults:   Pn1 = 1; Pn2 = 1
(defun cursor-position (&optional (line 1) (column 1))
  "Move the cursor to the 1-based line and column number."
  (csi "H" line column))

;; Name:     Erase in page
;; Mnemonic: ED
;; Sequence: CSI Ps J
;; Defaults: Ps = 0
(defun erase-in-page (&optional (n 0))
  (csi "J" n))

(defun erase-below ()
  (erase-in-page 0))

(defun erase-above ()
  (erase-in-page 1))

(defun erase ()
  (erase-in-page 2))

(defun erase-scrollback ()
  (erase-in-page 3))

;; Mnemonic: EL
;; Name:     Erase in line
;; Sequence: CSI Ps K
;; Defaults: Ps = 0
(defun erase-in-line (&optional (n 0))
  (csi "K" n))

(defun erase-right ()
  (erase-in-line 0))

(defun erase-left ()
  (erase-in-line 1))

(defun erase-line ()
  (erase-in-line 2))

;; the terminal sends ^[[11;16R or ESC[n;mR to the application
;; as if we read it through read-line
(defun device-status-report ()
  (csi "6n"))

;; 8.3.117
;; Name:        Select Graphic Rendition
;; Mnemonic:    SGR
;; Sequence:    CSI Pm m
(defun sgr (&rest params)
  "Set character attributes and foreground and background colors."
  (apply #'csi "m" params))
