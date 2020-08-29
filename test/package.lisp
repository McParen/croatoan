(defpackage #:de.anvi.ncurses.test
  (:documentation "Tests and examples for the low-level ncurses CFFI bindings.")
  (:use #:common-lisp #:de.anvi.ncurses)
  (:export

   ;; ncurses.lisp
   #:nctest
   #:nctest2
   #:nctest3
   #:nctest4
   #:nctest5
   #:nctest6
   #:nctest7
   #:nctest8
   #:nctest9

   ;; unicode.lisp
   #:ut01
   #:ut02
   #:ut02b
   #:ut03
   #:ut04
   ;; Minimal example of creating a cchar and setting it as a window background.
   #:ut05))

(defpackage #:de.anvi.croatoan.test
  (:documentation "Tests and examples for the high-level CLOS API.")
  (:use #:common-lisp #:de.anvi.croatoan)
  (:export

   ;; clos.lisp
   #:t00
   #:t01
   #:t02
   #:t02a
   #:t02b
   #:t02c
   #:t02d
   #:t03
   #:t03a
   #:t03a2
   #:t03b
   #:t03b1
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
   #:t10b
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
   #:t16e1
   #:t16e2
   #:t16e3
   #:t16e4
   #:t16e5
   #:t16f
   #:t16g
   #:t16h
   #:t16i
   #:t16j
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
   #:t21a
   #:t22
   #:t23
   #:t24
   #:t25
   #:t26
   #:t26a
   #:t27
   #:t28
   #:t28a
   #:t28b
   #:t29
   #:t30
   #:t31
   #:t31a
   #:t31b
   #:t31c
   #:t32
   #:t33

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
   #:evolve6

   ;; game-of-life.lisp
   #:gol))
