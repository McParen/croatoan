(defpackage #:de.anvi.ansi-escape
  (:documentation "Terminal control sequences based on ANSI X3.64")
  (:use #:common-lisp)
  ;; ed has to be shadowed because of a conflict with common-lisp:ed
  (:shadow #:ed)
  (:nicknames #:ansi-escape #:esc)
  (:export

   ;; ansi.lisp
   #:cuu #:cursor-up
   #:cud #:cursor-down
   #:cuf #:cursor-right
   #:cub #:cursor-left
   #:cup #:cursor-position

   #:ed #:erase-in-display #:erase-below #:erase-above #:erase #:erase-saved-lines
   #:el #:erase-in-line #:erase-right #:erase-left #:erase-line
   
   #:sgr #:select-graphic-rendition

   ;; common.lisp
   #:clear
   #:home))

(defpackage #:de.anvi.ansi-escape.test
  (:documentation "Tests and examples for the ANSI escape sequences.")
  (:use #:common-lisp #:de.anvi.ansi-escape)
  ;; when the esc package is :used, we have to resolve name conflict with common-lisp:ed
  (:shadowing-import-from #:common-lisp #:ed)
  (:export

   #:t01
   #:t02
   #:t03))
