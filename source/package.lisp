(defpackage #:de.anvi.croatoan
  (:use #:common-lisp #:cffi #:de.anvi.ncurses #:sb-gray)
  (:export 

   ;; croatoan.lisp
   with-screen

   ;; classes
   complex-char
   .simple-char
   .attributes
   .color-pair

   window
   .width
   .height
   .origin
   .cursor-position
   .winptr
   .input-blocking
   .enable-fkeys
   .enable-scrolling
   .scrolling-region
   .background-char
;   .attributes

   screen
   .input-echoing
   .input-reading
   .cursor-visibility

   ;; methods
   convert-char

   ;; addch / add a character (with attributes) to a curses window, then advance the cursor
   add-char
   echo-char
   new-line
   acs

   ;; addstr / add a string of characters to a curses window and advance cursor
   add-string

   ;; attr / curses character and window attribute control routines
   add-attribute
   remove-attribute
   set-attributes
   set-color
   change-attribute
   pair-number
   attribute

   ;; beep / curses bell and screen flash routines
   alert

   ;; bkgd / curses window background manipulation routines
   set-background-char
   get-background-char

   ;; border / create curses borders, horizontal and vertical lines
   border
   box
   hline
   vline

   ;; clear / clear all or part of a curses window
   clear

   ;; color / curses color manipulation routines
   init-color-mode
   colors-supported-p
   color-changeable-p

   ;; default_colors / use terminal's default colors
   use-default-colors
   assume-default-colors

   ;; define_key / define a keycode
   define-key

   ;; delch / delete character under the cursor in a curses window
   delete-char

   ;; getch / get (or push back) characters from curses terminal keyboard
   get-char
   unget-char
   key-supported-p
   function-key
   function-key-p

   ;; getstr / accept character strings from curses terminal keyboard
   get-string

   ;; getyx / get curses cursor and window coordinates
   cursor-position
   window-begin
   subwindow-relative-begin
   window-size

   ;; inch / get a character and attributes from a curses window
   extract-char

   ;; insch / insert a character before cursor in a curses window
   insert-char

   ;; instr / get a string of characters from a curses window
   extract-string

   ;; inchstr / get a string of characters (and attributes) from a curses window
   extract-rendered-string

   ;; insstr / insert string before cursor in a curses window
   insert-string

   ;; initscr / Screen initialization and manipulation routines
   ;;init-screen
   end-screen
   end-refreshed-p
   new-terminal
   set-current-terminal

   ;; inopts / Input options.
   flush-on-interrupt
   enable-8bit-char-input
   io-queue-flush
   escape-sequence-delay
   type-ahead-fd

   ;; keybound / return definition of keycode
   key-description

   ;; key_defined / check if a keycode is defined
   key-defined-p

   ;; legacy / get curses cursor and window coordinates, attributes

   ;; legacy_coding
   set-char-representation

   ;; move / move curses window cursor
   move
   move-by
   move-to

   ;; opaque / curses window properties
   redraw-on-clear-p
   insert-delete-char-p
   insert-delete-line-p
   immediately-refresh-p
   function-keys-p
   leave-cursor-on-refresh-p
   input-blocking-p
   escape-sequence-delay
   pad-p
   enable-scrolling-p
   subwindow-p
   touch-parent-windows-p
   get-parent-window
   get-scrolling-region

   ;; outopts / curses output options
   redraw-on-clear
   insert-delete-line
   insert-delete-char
   immediately-refresh
   leave-cursor-on-refresh
   enable-scrolling
   set-scrolling-region
   newline-translation

   ;; pad / create and display curses pads
   new-pad
   new-subpad
   refresh-pad
   no-output-refresh-pad
   pad-echo-char

   ;; refresh / refresh curses windows and lines
   refresh
   update
   mark-for-redraw

   ;; resizeterm / change the curses terminal size
   resize-terminal
   terminal-resized-p

   ;; scroll / scroll a curses window
   scroll

   ;; variables / curses global variables
   +color-pairs+
   +colors+
   +screen-columns+
   +esc-delay+
   +screen-lines+
   +tab-size+

   ;; window / create curses windows
   new-window
   delete-window
   move-window
   new-subwindow
   move-subwindow
   duplicate-window

   ;; wresize / resize a curses window
   resize-window

   ;;; still incomplete:

   ;; touch / curses refresh control routines
   touch

   ;; util / miscellaneous curses utility routines
   char-to-string
   key-to-string

   ;; slk / curses soft label routines

   ;; termattrs / environment query routines

   ;; kernel / low-level curses routines
   ;set-cursor-visibility

   ))

