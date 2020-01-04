(asdf:defsystem :croatoan
  :description "Common Lisp bindings for the ncurses terminal library."
  :author "Anton Vidovic <anton.vidovic@gmx.de>"
  :licence "MIT"
  :version "0.0.1"
  :depends-on (:cffi :trivial-gray-streams :bordeaux-threads)
  :components

  ;; Basic CFFI wrapper for libncursesw and libncurses
  ((:module "ncurses"
            :components
            ((:file "package")
             (:file "ncurses")

             ;; complete
             (:file "addch")            ; add a character (with attributes) to a curses window, then advance the cursor
             (:file "add_wch")          ; add a complex character and rendition to a curses window, then advance the cursor
             (:file "addstr")           ; add a string of characters to a curses window and advance cursor
             (:file "attr")             ; curses character and window attribute control routines
             (:file "beep")             ; curses bell and screen flash routines
             (:file "bkgd")             ; curses window background manipulation routines
             (:file "bkgrnd")           ; window complex background manipulation routines
             (:file "border")           ; create curses borders, horizontal and vertical lines
             (:file "border_set")       ; create curses borders or lines using complex characters and renditions
             (:file "clear")            ; clear all or part of a curses window
             (:file "color")            ; curses color manipulation routines
             (:file "default_colors")   ; use terminal's default colors
             (:file "define_key")       ; define a keycode
             (:file "delch")            ; delete character under the cursor in a curses window
             (:file "deleteln")         ; delete and insert lines in a curses window
             (:file "extend")           ; curses window properties
             (:file "getch")            ; get (or push back) characters from curses terminal keyboard
             (:file "get_wch")          ; get (or push back) a wide (multi-byte) character from curses terminal keyboard
             (:file "getcchar")         ; Get a wide character string and rendition from a cchar_t or set a cchar_t from a wide-character string
             (:file "getstr")           ; accept character strings from curses terminal keyboard
             (:file "getyx")            ; get curses cursor and window coordinates
             (:file "inch")             ; get a character and attributes from a curses window
             (:file "in_wch")           ; extract a wide (multi-byte) character and rendition from a window
             (:file "insch")            ; insert a character before cursor in a curses window
             (:file "ins_wch")          ; insert a complex character and rendition into a window
             (:file "instr")            ; get a string of characters from a curses window
             (:file "inwstr")           ; extract a string of wchar_t characters from a curses window
             (:file "inchstr")          ; get a string of characters (and attributes) from a curses window
             (:file "insstr")           ; insert string before cursor in a curses window
             (:file "initscr")          ; screen initialization and manipulation routines
             (:file "inopts")           ; curses input options
             (:file "kernel")           ; low-level curses routines
             (:file "keybound")         ; return definition of keycode
             (:file "key_defined")      ; check if a keycode is defined
             (:file "legacy")           ; get curses cursor and window coordinates, attributes
             (:file "legacy_coding")    ; legacy coding
             (:file "mouse")            ; mouse interface through curses
             (:file "move")             ; move curses window cursor
             (:file "opaque")           ; curses window properties
             (:file "outopts")          ; curses output options
             (:file "pad")              ; create and display curses pads
             (:file "refresh")          ; refresh curses windows and lines
             (:file "resizeterm")       ; change the curses terminal size
             (:file "scroll")           ; scroll a curses window
             (:file "slk")              ; curses soft label routines
             (:file "termattrs")        ; environment query routines
             (:file "touch")            ; curses refresh control routines
             (:file "util")             ; miscellaneous curses utility routines
             (:file "variables")        ; curses global variables
             (:file "window")           ; create curses windows
             (:file "wresize")))        ; resize a curses window

   ;; CLOS api on top of CFFI ncurses.
   (:module "src"
            :depends-on ("ncurses")
            :components
            ((:file "package")
             (:file "classes")
             (:file "queue")
             (:file "croatoan")
             (:file "gray_streams")
             (:file "utf8")

             (:file "util")             ; miscellanea utils
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
             (:file "wresize")

             ;; Extension libraries
             (:file "form")             ; curses extension for programming forms
             (:file "menu")             ; curses extension for programming menus
             (:file "shape")))))        ; curses extension for plotting shapes
