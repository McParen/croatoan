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
   ;; Use new pair functions: find-pair, alloc-pair, free-pair and reset-color-pairs.
   #:nctest10
   ;; Use wresize to resize a window.
   #:nctest11

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
   #:t01a
   #:t01b
   #:t01c
   #:t02
   #:t02a
   #:t02b
   #:t02c
   #:t03
   #:t03a
   #:t03b
   #:t03c
   #:t03d
   #:t03e
   #:t03f
   #:t03g
   #:t03h
   #:t03i
   #:t03j
   #:t04
   #:t04a
   #:t04b
   #:t05
   #:t06
   #:t06a
   #:t06b
   #:t07
   #:t07a
   #:t07b
   #:t07c
   #:t09
   #:t09a
   #:t09b
   #:t09c
   #:t10
   #:t10a1
   #:t10a
   #:t10b
   #:t11
   #:t11a
   #:t11b
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
   #:t16e6
   #:t16f
   #:t16g
   #:t16h
   #:t16i
   #:t16j
   #:t16j1
   #:t16j2
   #:t16k
   #:t17
   #:t17a
   #:t18
   #:t18a
   #:t19
   #:t19a
   #:t19b
   #:t19b2
   #:t19b3
   #:t19c
   #:t19c2
   #:t19c3
   #:t19d
   #:t19e
   #:t19e2
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
   #:t29a
   #:t30
   #:t31
   #:t31a
   #:t31b
   #:t31c
   #:t32
   #:t33
   #:t34
   #:t35
   #:t35a
   #:t35b
   #:t42
   #:t42a
   #:t42b
   #:t43
   #:t43a

   ;; clos.lisp
   #:snake
   #:snake2
   #:snake3
   #:pipes
   #:matrix
   #:matrix2
   #:matrix3
   #:matrix4
   #:robots
   #:pong
   #:tetris

   ;; dialog.lisp
   #:dlg01
   #:dlg02
   #:dlg03
   #:dlg04
   #:dlg05

   ;; evolution.lisp
   #:evolve5
   #:evolve6

   ;; game-of-life.lisp
   #:gol))
