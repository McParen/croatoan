(asdf:defsystem :croatoan-ncurses
  :description "Low-level Lisp CFFI bindings for the ncurses terminal library."
  :author "Anton Vidovic <anton.vidovic@gmx.de>"
  :licence "MIT"
  :version "0.3"
  :depends-on (:cffi)
  :pathname "ncurses/"
  :components

  ((:file "package")
   (:file "ncurses")          ; library file names, ncurses file types

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
   (:file "keyok")            ; enable or disable a keycode
   (:file "legacy")           ; get curses cursor and window coordinates, attributes
   (:file "legacy_coding")    ; legacy coding
   (:file "mouse")            ; mouse interface through curses
   (:file "move")             ; move curses window cursor
   (:file "new_pair")         ; new curses color-pair functions
   (:file "opaque")           ; curses window properties
   (:file "outopts")          ; curses output options
   (:file "pad")              ; create and display curses pads
   (:file "refresh")          ; refresh curses windows and lines
   (:file "resizeterm")       ; change the curses terminal size
   (:file "scroll")           ; scroll a curses window
   (:file "slk")              ; curses soft label routines
   (:file "termattrs")        ; get and set terminal attributes with curses
   (:file "terminfo")         ; curses interfaces to terminfo database
   (:file "touch")            ; curses refresh control routines
   (:file "util")             ; miscellaneous curses utility routines
   (:file "variables")        ; curses global variables
   (:file "window")           ; create curses windows
   (:file "wresize")          ; resize a curses window

   (:file "libc")))           ; setlocale
