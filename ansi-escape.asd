(asdf:defsystem :ansi-escape
  :description "Terminal control functions based on ANSI escape sequences."
  :author "Anton Vidovic <anton.vidovic@gmx.de>"
  :licence "MIT"
  :version "0.0.1"
  :pathname "ansi/"
  :components
  
  ((:file "package")
   (:file "ansi")
   (:file "common")))
