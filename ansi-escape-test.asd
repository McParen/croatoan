(asdf:defsystem :ansi-escape-test
  :description "Tests and examples for the ANSI escape sequences."
  :author "Anton Vidovic <anton.vidovic@gmx.de>"
  :licence "MIT"
  :version "0.0.1"
  :depends-on (:ansi-escape)
  :pathname "ansi/"
  :serial t
  :components

  ((:file "package")
   (:file "test")))
