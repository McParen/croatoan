(asdf:defsystem :croatoan-test
  :description "Tests and examples for the croatoan CLOS API and the low-level ncurses CFFI bindings."
  :author "Anton Vidovic <anton.vidovic@gmx.de>"
  :licence "MIT"
  :version "0.0.1"
  :depends-on (:croatoan)
  :components
  ((:module "test"
            :serial t
            :components ((:file "package")

                         ;; base %ncurses tests
                         (:file "ncurses")   
                         (:file "unicode")
                         
                         ;; high-level clos api
                         (:file "clos")
                         (:file "tetris")
                         (:file "evolution")))))
