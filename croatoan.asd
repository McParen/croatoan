(asdf:defsystem :croatoan
  :description "Common Lisp bindings for ncurses."
  :author "Anton Vidovic <anton.vidovic@gmx.de>"
  :licence "MIT"
  :version "0.0.1"
  :depends-on (:cffi)
  :components 

  ;; Basic CFFI wrapper for libncurses.
  ((:module "ncurses"
            :components
            ((:file "package")
             (:file "ncurses")

             ;; complete
             (:file "addch")            ; add a character (with attributes) to a curses window, then advance the cursor
             (:file "addstr")           ; add a string of characters to a curses window and advance cursor
             (:file "attr")             ; curses character and window attribute control routines
             (:file "beep")             ; curses bell and screen flash routines
             (:file "bkgd")             ; curses window background manipulation routines 
             (:file "border")           ; create curses borders, horizontal and vertical lines 
             (:file "clear")            ; clear all or part of a curses window
             (:file "color")            ; curses color manipulation routines
             (:file "default_colors")   ; use terminal's default colors
             (:file "define_key")       ; define a keycode
             (:file "delch")            ; delete character under the cursor in a curses window
             (:file "extend")           ; curses window properties
             (:file "getch")            ; get (or push back) characters from curses terminal keyboard
             (:file "getstr")           ; accept character strings from curses terminal keyboard
             (:file "getyx")            ; get curses cursor and window coordinates
             (:file "inch")             ; get a character and attributes from a curses window
             (:file "insch")            ; insert a character before cursor in a curses window
             (:file "instr")            ; get a string of characters from a curses window
             (:file "inchstr")          ; get a string of characters (and attributes) from a curses window
             (:file "insstr")           ; insert string before cursor in a curses window
             (:file "initscr")          ; screen initialization and manipulation routines
             (:file "inopts")           ; curses input options
             (:file "keybound")         ; return definition of keycode
             (:file "key_defined")      ; check if a keycode is defined
             (:file "legacy")           ; get curses cursor and window coordinates, attributes
             (:file "legacy_coding")    ; legacy coding
             (:file "move")             ; move curses window cursor
             (:file "opaque")           ; curses window properties
             (:file "outopts")          ; curses output options
             (:file "pad")              ; create and display curses pads
             (:file "refresh")          ; refresh curses windows and lines
             (:file "resizeterm")       ; change the curses terminal size
             (:file "scroll")           ; scroll a curses window
             (:file "slk")              ; curses soft label routines
             (:file "termattrs")        ; environment query routines
             (:file "variables")        ; curses global variables
             (:file "window")           ; create curses windows
             (:file "wresize")          ; resize a curses window

             ;; incomplete
             (:file "kernel")           ; low-level curses routines
             (:file "touch")            ; curses refresh control routines
             (:file "util")))           ; miscellaneous curses utility routines

   ;; CLOS api on top of CFFI ncurses.
   (:module "source"
            :depends-on ("ncurses")
            :components
            ((:file "package")
             (:file "croatoan")
             (:file "classes")

             (:file "addch")
             (:file "addstr")
             (:file "attr")
             (:file "bkgd")
             (:file "border")
             (:file "clear")
             (:file "getch")
             (:file "initscr")
             (:file "inopts")
             (:file "kernel")
             (:file "move")
             (:file "refresh")
             (:file "touch") ))

   ;; tests for both the CFFI ncurses wrappers and the CLOS API.
   (:module "tests"
            :depends-on ("source")
            :components
            ((:file "package")

             ;; all base %ncurses tests belong in this file.
             ;; all other files should stay croatoan only.
             (:file "ncurses")   

             (:file "clos")
             (:file "evolution") ))))

;;; TODO not ported to closified croatoan:
;;             (:file "acs")
;;             (:file "alert")
;;             (:file "kletva")
;;             (:file "matrix")
;;             (:file "scroll")
;;             (:file "snake")


;;; Not even started:

;; (:file "mouse")
;; mouse
;; mouse interface through curses
;; http://invisible-island.net/ncurses/man/curs_mouse.3x.html


;;; Useless, no need to implement.

;; (:file "print")
;; ship binary data to printer
;; http://invisible-island.net/ncurses/man/curs_print.3x.html

;; (:file "printw")
;; (:file "scanw")

;; (:file "scr_dump")
;; (:file "termcap")

;; (:file "threads")
;; http://invisible-island.net/ncurses/man/curs_threads.3x.html

;; http://invisible-island.net/ncurses/man/curs_sp_funcs.3x.html

;; http://invisible-island.net/ncurses/man/terminfo.5.html



;;; Wide character equivalents:

;; (:file "border_set") 
;; http://invisible-island.net/ncurses/man/curs_border_set.3x.html

;; getcchar
;; (:file "add_wch")
;; (:file "get_wch")
;; (:file "addwstr")
;; (:file "get_wstr")
;; (:file "add_wchstr")
;; (:file "in_wch")
;; (:file "in_wchstr")
;; (:file "inwstr")
;; (:file "ins_wch")
;; (:file "ins_wstr")

;; eine funktion in pad.lisp
