(defpackage #:de.anvi.croatoan
  (:documentation "High-level Lisp interface to the basic CFFI Ncurses API.")
  (:use #:common-lisp #:cffi #:de.anvi.ncurses #:trivial-gray-streams)
  (:shadow callback)
  (:nicknames #:croatoan #:crt)
  (:export

   ;; croatoan.lisp
   with-screen
   with-window
   with-windows
   event-case
   keymap
   find-keymap
   run-event-loop
   exit-event-loop
   bind
   unbind
   save-excursion

   ;; classes
   complex-char
   complex-string
   window
   screen
   sub-window
   decorated-window
   menu
   menu-window
   menu-item
   dialog-window
   pad
   sub-pad
   field
   form
   form-window
   button
   label
   shape

   ;; accessors
   simple-char
   attributes
   color-pair
   complex-char-array
   width
   height
   location
   location-y
   location-x
   cursor-position
   cursor-position-y
   cursor-position-x
   draw-border-p
   stackedp
   visiblep
   winptr
   input-blocking
   frame-rate
   function-keys-enabled-p
   scrolling-enabled-p
   scrolling-region
   insert-mode-p
   bindings
   background
   input-echoing-p
   input-buffering-p
   process-control-chars-p
   newline-translation-enabled-p
   cursor-visible-p
   source-location

   ;; Predicates
   closed-p
   
   ;; menu
   items
   menu-type
   current-item-number
   current-item
   current-item-mark
   cyclic-selection-p
   max-item-length
   name
   value
   message-pad
   message-text
   message-height
   message-pad-coordinates
   menu-map

   ;; form
   buffer
   elements
   style
   max-buffer-length
   callback
   title
   find-element
   field-map
   form-map
   button-map
   cancel-form
   accept-form
   reset-form
   
   ;; shape
   origin-x
   origin-y
   coordinates
   plot-char
   
   ;; addch / add a character (with attributes) to a curses window, then advance the cursor
   add
   add-char
   echo
   echo-char
   new-line
   acs
   wacs

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
   draw-border

   ;; border_set / create curses borders or lines using complex characters and renditions
   draw-wide-border
   
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

   ;; form / curses extension for programming forms
   remove-nth
   replace-nth
   insert-nth
   edit
   field-buffer-to-string
   
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
   extract-char

   ;; in_wch / extract a wide character and rendition from a window
   extract-wide-char

   ;; insch / insert a character before cursor in a curses window
   insert
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
   select
   return-from-menu
   exit-menu-event-loop
   accept-selection
   update-redraw-menu
   toggle-item-checkbox

   ;; mouse / mouse interface through curses
   set-mouse-event
   get-mouse-event

   ;; move / move curses window cursor
   move
   move-direction
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

   ;; shape / shape plotting extension for ncurses
   draw-shape
   shape-extent
   merge-shapes
   fill-shape
   line
   angle-line
   polygon
   triangle
   quadrilateral
   rectangle
   circle
	  
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
