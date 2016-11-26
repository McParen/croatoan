(defpackage #:de.anvi.croatoan.tests
  (:documentation "Tests and examples demonstrating the use of the Croatoan API.")
  (:use #:common-lisp #:cffi #:de.anvi.ncurses #:de.anvi.croatoan)

  ;; Test functions from the REPL: (a:matrix)
  ;;(:nicknames #:a)

  (:export

   ;; ncurses.lisp
   #:nctest
   #:nctest2

   ;; unicode.lisp
   #:ut01
   #:ut02
   #:ut03
   #:ut04

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
   #:t08c
   #:t09
   #:t09a
   #:t09b
   #:t10
   #:t10a
   #:t11
   #:t11a
   #:t12
   #:t12a
   #:t12b
   #:t12c
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
   #:t19b
   #:t19c
   #:t19d
   #:t19e
   #:t19f
   #:t20
   #:t21
   #:t22
   #:t23
   #:t24
   #:t25

   ;; clos.lisp
   #:matrix
   #:snake
   #:tetris
   #:pipes

   ;; evolution.lisp
   #:evolve5 ))
