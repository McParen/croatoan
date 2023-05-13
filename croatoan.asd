(asdf:defsystem :croatoan
  :description "High-level Lisp CLOS bindings for the ncurses terminal library."
  :author "Anton Vidovic <anton.vidovic@gmx.de>"
  :licence "MIT"
  :version "0.1"
  :depends-on (:croatoan-ncurses
               :trivial-gray-streams
               :bordeaux-threads
               (:feature :sbcl (:require "sb-introspect")))
  :pathname "src/"
  :components

  ((:file "package")
   (:file "character")        ; complex character and string
   (:file "stack")            ; stack extension for curses (ncurses term: panel)
   (:file "grid")             ; Utility to track the display of items in a scrollable mxn grid
   (:file "classes")
   (:file "croatoan")         ; macros, event handling
   (:file "menu")             ; curses extension for programming menus
   (:file "form")             ; curses extension for programming forms
   (:file "field")
   (:file "textarea")         ; form element textarea
   (:file "hook")
   (:file "queue")            ; simple thread-safe queue
   (:file "gray_streams")
   (:file "utf8")
   (:file "dialog")           ; misc dialog boxes

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
   (:file "refresh")
   (:file "scroll")
   (:file "touch")
   (:file "util")             ; miscellaneous utils
   (:file "wresize")

   ;; Extension libraries
   (:file "shape")))          ; curses extension for plotting shapes
