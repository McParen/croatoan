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
   #:t03c
   #:t04
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
   #:t11
   #:t11a
   #:t12
   #:t13

   ;; clos.lisp
   #:matrix
   #:snake
   #:tetris

   ;; evolution.lisp
   #:evolve4 ))
