(defpackage #:de.anvi.ansi-escape
  (:documentation "Terminal control sequences based on ANSI X3.64")
  (:use #:common-lisp)
  (:nicknames #:ansi-escape #:esc)
  (:export

   ;; ansi.lisp
   #:cursor-position
   #:erase
   #:sgr

   ;; common.lisp
   #:clear
   #:home))

(defpackage #:de.anvi.ansi-escape.test
  (:documentation "Tests and examples for the ANSI escape sequences.")
  (:use #:common-lisp #:de.anvi.ansi-escape)
  (:nicknames #:a)
  (:export

   #:t01
   #:t02))
