(defpackage #:de.anvi.croatoan
  (:documentation "High-level Lisp interface to the basic CFFI Ncurses API.")
  ;;(:use #:common-lisp #:cffi #:de.anvi.ncurses #:sb-gray)
  (:use #:common-lisp #:cffi #:de.anvi.ncurses #:trivial-gray-streams)
  (:nicknames #:croatoan)
  (:export

   ;; croatoan.lisp
   with-screen
   event-case
   save-excursion

   ;; classes
   complex-char
   complex-string
   window
   screen
   sub-window
   decorated-window
   menu-window
   menu-item
   dialog-window
   pad
   sub-pad

   ;; accessors
   .simple-char
   .attributes
   .color-pair
   .complex-char-array
   .width
   .height
   .position
   .cursor-position
   .border
   .stacked
   .visible
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
   .closed-p

   .items
   .checklist
   .type
   .current-item-number
   .current-item-mark
   .cyclic-selection
   .max-item-length
   .name
   .value
   .window
   .sub-window
   .message-pad
   .message-text
   .message-height
   .message-pad-coordinates
   
   ;; addch / add a character (with attributes) to a curses window, then advance the cursor
   add
   add-char
   echo
   new-line
   acs

   ;; add_wch / add a wide complex character to a curses window, then advance the cursor
   add-wide-char
   add-wide-char-utf-8
   echo-wide-char
   
   ;; addstr / add a string of characters to a curses window and advance cursor
   add-string

   ;; attr / curses character and window attribute control routines
   *ansi-color-list*
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

   ;; deleteln / delete and insert lines in a curses window
   delete-line
   insert-line

   ;; getch / get (or push back) characters from curses terminal keyboard
   get-char
   unget-char
   key-supported-p
   function-key
   function-key-p
   key-pressed-p
   get-event

   ;; get_wch / get (or push back) a wide (multi-byte) character from curses terminal keyboard
   get-wide-char
   get-wide-event

   ;; getstr / accept character strings from curses terminal keyboard
   get-string

   ;; getyx / get curses cursor and window coordinates

   ;; inch / get a character and attributes from a curses window
   extract-complex-char

   ;; in_wch / extract a wide character and rendition from a window
   extract-wide-char

   ;; insch / insert a character before cursor in a curses window
   insert-char

   ;; ins_wch / insert a complex character and rendition into a window
   insert-wide-char

   ;; instr / get a string of characters from a curses window
   extract-string

   ;; inwstr / get a string of wide (multi-byte) characters from a curses window
   extract-wide-string
   
   ;; inchstr / get a string of characters (and attributes) from a curses window
   extract-complex-string

   ;; insstr / insert string before cursor in a curses window
   insert-string

   ;; initscr / Screen initialization and manipulation routines
   end-screen

   ;; inopts / Input options.

   ;; kernel / low-level curses routines

   ;; keybound / return definition of keycode

   ;; key_defined / check if a keycode is defined

   ;; legacy / get curses cursor and window coordinates, attributes

   ;; legacy_coding

   ;; menu / curses extension for programming menus
   draw-menu
   update-menu
   select-item
   current-item

   ;; mouse / mouse interface through curses
   set-mouse-event
   get-mouse-event

   ;; move / move curses window cursor
   move
   move-to
   move-window

   ;; opaque / curses window properties

   ;; outopts / curses output options

   ;; pad / create and display curses pads

   ;; panel / panel stack extension for curses
   raise
   raise-to-top
   lower
   lower-to-bottom
   empty-stack
   refresh-stack

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
