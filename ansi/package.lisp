(defpackage #:de.anvi.ansi-escape
  (:documentation "Terminal control sequences based on ANSI X3.64")
  (:use #:common-lisp)
  ;; ed has to be shadowed because of a conflict with common-lisp:ed
  (:shadow #:ed)
  (:nicknames #:ansi-escape #:esc)
  (:export

   ;; ansi.lisp
   ;; ESC sequences
   #:ris #:reset-to-initial-state

   ;; CSI sequences
   ;; Cursor control
   #:cuu #:cursor-up
   #:cud #:cursor-down
   #:cuf #:cursor-forward
   #:cub #:cursor-backward
   #:cnl #:cursor-next-line
   #:cpl #:cursor-preceding-line
   #:cha #:cursor-horizontal-absolute
   #:cup #:cursor-position
   #:vpa #:vertical-position-absolute
   #:vpr #:vertical-position-relative
   #:vpb #:vertical-position-backward
   #:scosc #:save-cursor-position
   #:scorc #:restore-cursor-position

   #:ed #:erase-in-display #:erase-below #:erase-above #:erase #:erase-saved-lines
   #:el #:erase-in-line #:erase-right #:erase-left #:erase-line

   #:sgr #:select-graphic-rendition

   #:device-status-report

   ;; DEC private mode set and reset
   #:decset #:dec-private-mode-set
   #:decrst #:dec-private-mode-reset
   #:show-cursor #:hide-cursor
   #:use-alternate-screen-buffer #:use-normal-screen-buffer

   ;; common.lisp
   #:clear
   #:home

   ;; stty.lisp
   #:set-tty-mode))

(defpackage #:de.anvi.ansi-escape.test
  (:documentation "Tests and examples for the ANSI escape sequences.")
  (:use #:common-lisp #:de.anvi.ansi-escape)
  ;; when the esc package is :used, we have to resolve name conflict with common-lisp:ed
  (:shadowing-import-from #:common-lisp #:ed)
  (:export

   #:t01
   #:t02
   #:t03
   #:t04
   #:t05
   #:t06
   #:t07
   #:t08
   #:t09))
