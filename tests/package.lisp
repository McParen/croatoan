(defpackage #:de.anvi.croatoan.tests
  (:documentation "Misc functions for testing and debugging.")
  (:use #:common-lisp #:cffi #:de.anvi.ncurses #:de.anvi.croatoan)

  ;; Test functions from the REPL: (a:matrix)
  ;;(:nicknames #:a)

  (:export

   ;; ncurses.lisp
   #:nctest
   #:nctest2

   ;; clos.lisp
   #:t00
   #:t01
   #:t02
   #:t03
   #:t03a
   #:t03b
   #:t03b2
   #:t03c
   #:t04
   #:t04a
   #:t04b
   #:t05
   #:t06
   #:t06a
   #:t07
   #:t07a
   #:t08
   #:t08a
   #:t08b
   #:t09
   #:t09a
   #:t10
   #:t10a
   #:t11
   #:t11a
   #:t12
   #:t12a
   #:t13
   #:t14
   #:t14a
   #:t14b
   #:t15
   #:t15a
   #:t15b
   #:t16
   #:t16a
   #:t16b
   #:t16c
   #:t17
   #:t17a
   #:t18
   #:t19
   #:t19a
   
   ;; clos.lisp
   #:matrix
   #:snake
   #:tetris
   #:pipes

   ;; evolution.lisp
   #:evolve4 ))
