(asdf:defsystem :croatoan-tests
  :description "Tests and examples for the croatoan CLOS API and the low-level ncurses CFFI bindings."
  :author "Anton Vidovic <anton.vidovic@gmx.de>"
  :licence "MIT"
  :version "0.0.1"
  :depends-on (:croatoan)
  :components
  ((:module "tests"
            :serial t
            :components ((:file "package")

                         ;; base %ncurses tests
                         (:file "ncurses")   
                         (:file "unicode")
                         
                         ;; high-level clos api
                         (:file "clos")
                         (:file "evolution")))))
