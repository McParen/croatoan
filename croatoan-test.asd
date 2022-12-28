(asdf:defsystem :croatoan-test
  :description "Tests and examples for the croatoan CLOS API and the low-level ncurses CFFI bindings."
  :author "Anton Vidovic <anton.vidovic@gmx.de>"
  :licence "MIT"
  :version "0.0.1"
  :depends-on (:croatoan)
  :pathname "test/"
  :serial t
  :components

  ((:file "package")

   ;; core ncurses tests
   (:file "ncurses")
   (:file "unicode")

   ;; high-level clos api
   (:file "clos")
   (:file "dialog")
   (:file "tetris")
   (:file "evolution")
   (:file "game-of-life")))
