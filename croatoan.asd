(asdf:defsystem :croatoan
  :description "High-level Lisp CLOS bindings for the ncurses terminal library."
  :author "Anton Vidovic <anton.vidovic@gmx.de>"
  :licence "MIT"
  :version "0.0.1"
  :depends-on (:croatoan-ncurses :trivial-gray-streams :bordeaux-threads)
  :pathname "src/"
  :components

  ((:file "package")
   (:file "classes")
   (:file "queue")            ; simple thread-safe queue
   (:file "croatoan")         ; macros, event handling
   (:file "gray_streams")
   (:file "utf8")

   ;; Ncurses core functions
   (:file "addch")
   (:file "add_wch")
   (:file "addstr")
   (:file "attr")
   (:file "beep")
   (:file "bkgd")
   (:file "bkgrnd")           ; window complex background manipulation routines
   (:file "border")
   (:file "border_set")       ; create curses borders or lines using complex characters and renditions
   (:file "clear")
   (:file "color")
   (:file "delch")
   (:file "deleteln")
   (:file "getch")
   (:file "define_key")       ; define a keycode
   (:file "get_wch")
   (:file "getstr")
   (:file "initscr")
   (:file "inopts")
   (:file "inch")
   (:file "in_wch")
   (:file "inwstr")           ; get a string of wide (multi-byte) characters from a curses window
   (:file "inchstr")
   (:file "insch")
   (:file "ins_wch")          ; insert a complex character and rendition into a window
   (:file "insstr")
   (:file "instr")
   (:file "kernel")
   (:file "mouse")
   (:file "move")
   (:file "panel")
   (:file "refresh")
   (:file "touch")
   (:file "util")             ; miscellaneous utils
   (:file "wresize")
   
   ;; Extension libraries
   (:file "form")             ; curses extension for programming forms
   (:file "textarea")         ; form element textarea
   (:file "menu")             ; curses extension for programming menus
   (:file "shape")))          ; curses extension for plotting shapes
