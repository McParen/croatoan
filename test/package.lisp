(defpackage #:de.anvi.croatoan.test
  (:documentation "Tests and examples demonstrating the use of the croatoan API.")
  (:use #:common-lisp #:cffi #:de.anvi.ncurses #:de.anvi.croatoan)
  (:shadowing-import-from #:de.anvi.croatoan callback)
  (:export

   ;; ncurses.lisp
   #:nctest
   #:nctest2
   #:nctest3
   #:nctest4
   #:nctest5

   ;; unicode.lisp
   #:ut01
   #:ut02
   #:ut02b
   #:ut03
   #:ut04

   ;; clos.lisp
   #:t00
   #:t01
   #:t02
   #:t02a
   #:t02b
   #:t02c
   #:t03
   #:t03a
   #:t03a2
   #:t03b
   #:t03b2
   #:t03b3
   #:t03c
   #:t03d
   #:t03d2
   #:t03e
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
   #:t09c
   #:t10
   #:t10a
   #:t11
   #:t11a
   #:t12
   #:t12a
   #:t12b
   #:t12c
   #:t12c2
   #:t12d
   #:t13
   #:t14
   #:t14a
   #:t14b
   #:t14c
   #:t15
   #:t15a
   #:t15b
   #:t16
   #:t16a
   #:t16b
   #:t16c
   #:t16d
   #:t16e
   #:t16f
   #:t16g
   #:t16h
   #:t16i
   #:t17
   #:t17a
   #:t18
   #:t18a
   #:t19
   #:t19a
   #:t19b
   #:t19b2
   #:t19c
   #:t19c2
   #:t19c3
   #:t19d
   #:t19e
   #:t19e2
   #:t19f
   #:t19g
   #:t20
   #:t20a
   #:t20b
   #:t20c
   #:t21
   #:t22
   #:t23
   #:t24
   #:t25
   #:t26
   #:t27
   #:t28
   #:t28a
   #:t29
   #:t30
   #:t31
   #:t32

   ;; clos.lisp
   #:matrix
   #:matrix2
   #:matrix3
   #:matrix4
   #:snake
   #:snake2
   #:tetris
   #:pipes

   ;; evolution.lisp
   #:evolve5
   #:evolve6))
