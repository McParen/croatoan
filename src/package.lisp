(defpackage #:de.anvi.croatoan
  (:documentation "High-level Lisp interface to the basic CFFI Ncurses API.")
  (:use #:common-lisp #:trivial-gray-streams)
  (:nicknames #:croatoan #:crt)
  (:export

   ;; croatoan.lisp
   with-screen
   with-window
   with-windows

   hook
   hooks
   before-event-hook
   after-event-hook
   before-submenu-hook
   
   event-case
   keymap
   define-keymap
   find-keymap
   run-event-loop
   exit-event-loop
   bind
   unbind
   save-excursion
   dogrid

   ;; classes
   complex-char
   complex-string
   widget
   window
   screen
   sub-window
   panel
   menu
   checklist
   menu-window
   menu-panel
   menu-item
   dialog-window
   pad
   sub-pad
   field
   textarea
   form
   form-window
   msgbox
   button
   label
   checkbox
   shape

   ;; accessors
   simple-char
   attributes
   color-pair
   default-color-pair
   fgcolor
   bgcolor
   complex-char-array
   width
   height
   dimensions
   window-position
   position-y
   position-x
   cursor-position
   cursor-position-y
   cursor-position-x
   borderp
   stackedp
   visiblep
   winptr
   input-blocking
   frame-rate
   function-keys-enabled-p
   scrolling-enabled-p
   scrolling-region
   insert-mode-p
   toggle-insert-mode
   bindings
   background
   input-echoing-p
   input-buffering-p
   process-control-chars-p
   newline-translation-enabled-p
   cursor-visible-p
   source-position
   center-position
   random-position

   ;; Predicates
   closed-p
   complex-char=

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
   element-position
   activep
   style
   max-buffer-length
   callback
   title
   find-element
   field-map
   form-map
   button-map
   accept
   cancel
   return-element-value
   return-form-values
   reset-field
   reset-form
   checkedp
   draw
   move-start-of-line
   move-end-of-line
   move-previous-char
   move-next-char

   ;; queue
   submit
   process
   queue
   simple-queue
   job-error
   enqueue
   dequeue

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
   put
   put-char
   put-string

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
   use-terminal-colors-p

   ;; define_key / define a keycode
   define-function-key

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
   key-name-to-code
   key-code-to-name
   delete-function-key
   add-function-key
   function-key-p
   key-pressed-p
   get-event

   ;; get_wch / get (or push back) a wide (multi-byte) character from curses terminal keyboard
   get-wide-char
   get-wide-event
   wait-for-event

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
   goto
   get-direction
   move-direction
   move-window

   ;; opaque / curses window properties

   ;; outopts / curses output options

   ;; pad / create and display curses pads

   ;; panel / panel stack extension for curses
   *main-stack*
   stack
   stack-push
   stack-pop
   stack-move
   empty-stack
   stack-empty-p

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
   draw-line

   ;; slk / curses soft label routines

   ;; termattrs / environment query routines

   ;; touch / curses refresh control routines
   touch

   ;; util / miscellaneous curses utility routines
   char-to-string
   string-to-char
   key-to-string
   flush-input
   split-string
   wrap-string

   ;; variables / curses global variables

   ;; window / create curses windows

   ;; wresize / resize a curses window
   resize

   ;; complex-string/char utility-functions

   make-background
   complex-string-format
   complex-string-length
   text-width
   text-slice
   nconcat-complex-string
   concat-complex-string
   complex-string->chars-string
   text-ellipsize
   text-right-pad
   ))
