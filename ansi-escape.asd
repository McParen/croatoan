(asdf:defsystem :ansi-escape
  :description "Terminal control functions based on ANSI escape sequences."
  :author "Anton Vidovic <anton.vidovic@gmx.de>"
  :licence "MIT"
  :version "0.0.1"
  :depends-on (#+sbcl :sb-posix)
  :pathname "ansi/"
  :components
  
  ((:file "package")
   (:file "ansi")
   (:file "common")
   (:file "stty")))
