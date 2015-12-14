(defpackage #:de.anvi.croatoan
  ;;(:use #:common-lisp #:cffi #:de.anvi.ncurses #:sb-gray)
  (:use #:common-lisp #:cffi #:de.anvi.ncurses #:trivial-gray-streams)
  (:export 

   ;; croatoan.lisp
   with-screen
   event-case

   ;; classes
   complex-char
   window
   screen
   sub-window

   ;; accessors
   .simple-char
   .attributes
   .color-pair
   .width
   .height
   .position
   .cursor-position
   .winptr
   .input-blocking
   .enable-fkeys
   .enable-scrolling
   .scrolling-region
   .insert-enabled
   .background
   .input-echoing
   .input-reading
   .cursor-visibility
   .source

   ;; addch / add a character (with attributes) to a curses window, then advance the cursor
   add-char
   echo-char
   new-line
   acs

   ;; addstr / add a string of characters to a curses window and advance cursor
   add-string

   ;; attr / curses character and window attribute control routines
   convert-char
   change-attributes
   add-attributes
   remove-attributes
   
   ;; beep / curses bell and screen flash routines
   alert

   ;; bkgd / curses window background manipulation routines

   ;; border / create curses borders, horizontal and vertical lines
   box

   ;; clear / clear all or part of a curses window
   clear

   ;; color / curses color manipulation routines

   ;; default_colors / use terminal's default colors

   ;; define_key / define a keycode

   ;; delch / delete character under the cursor in a curses window
   delete-char

   ;; getch / get (or push back) characters from curses terminal keyboard
   get-char
   unget-char
   key-supported-p
   function-key
   function-key-p
   key-pressed-p
   get-event

   ;; getstr / accept character strings from curses terminal keyboard
   get-string

   ;; getyx / get curses cursor and window coordinates

   ;; inch / get a character and attributes from a curses window

   ;; insch / insert a character before cursor in a curses window
   insert-char

   ;; instr / get a string of characters from a curses window
   extract-string

   ;; inchstr / get a string of characters (and attributes) from a curses window

   ;; insstr / insert string before cursor in a curses window

   ;; initscr / Screen initialization and manipulation routines
   end-screen

   ;; inopts / Input options.

   ;; kernel / low-level curses routines

   ;; keybound / return definition of keycode

   ;; key_defined / check if a keycode is defined

   ;; legacy / get curses cursor and window coordinates, attributes

   ;; legacy_coding

   ;; mouse / mouse interface through curses
   set-mouse-event
   get-mouse-event

   ;; move / move curses window cursor
   move
   move-by
   move-to

   ;; opaque / curses window properties

   ;; outopts / curses output options

   ;; pad / create and display curses pads

   ;; refresh / refresh curses windows and lines
   refresh
   refresh-marked
   mark-for-refresh
   mark-for-redraw

   ;; resizeterm / change the curses terminal size

   ;; scroll / scroll a curses window

   ;; slk / curses soft label routines

   ;; termattrs / environment query routines

   ;; touch / curses refresh control routines
   touch

   ;; util / miscellaneous curses utility routines

   ;; variables / curses global variables

   ;; window / create curses windows

   ;; wresize / resize a curses window
   resize

   ))
