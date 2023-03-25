(defpackage #:de.anvi.ncurses
  (:documentation "Low-level CFFI bindings to the Ncurses C API. Not meant to be used directly.")
  (:use #:common-lisp)
  (:nicknames #:ncurses)
  (:export

   ;; addch / add a character (with attributes) to a curses window, then advance the cursor
   addch
   waddch
   mvaddch
   mvwaddch
   echochar
   wechochar

   ;; add_wch / add a complex character and rendition to a curses window, then advance the cursor
   add-wch
   wadd-wch
   mvadd-wch
   mvwadd-wch
   echo-wchar
   wecho-wchar

   ;; addstr / add a string of characters to a curses window and advance cursor
   addstr
   addnstr
   waddstr
   waddnstr
   mvaddstr
   mvaddnstr
   mvwaddstr
   mvwaddnstr

   ;; attr / curses character and window attribute control routines
   attroff
   wattroff
   attron
   wattron
   attrset
   wattrset
   color-set
   wcolor-set
   standend
   wstandend
   standout
   wstandout
   attr-get
   wattr-get
   attr-off
   wattr-off
   attr-on
   wattr-on
   attr-set
   wattr-set
   chgat
   wchgat
   mvchgat
   mvwchgat

   ;; beep / curses bell and screen flash routines
   beep
   flash

   ;; bkgd / curses window background manipulation routines
   bkgdset
   wbkgdset
   bkgd
   wbkgd
   getbkgd

   ;; bkgrnd / window complex background manipulation routines
   bkgrnd
   wbkgrnd
   bkgrndset
   wbkgrndset
   getbkgrnd
   wgetbkgrnd

   ;; border / create curses borders, horizontal and vertical lines
   border
   wborder
   box
   hline
   whline
   vline
   wvline
   mvhline
   mvwhline
   mvvline
   mvwvline

   ;; border_set / create curses borders or lines using complex characters and renditions
   border-set
   wborder-set
   box-set
   hline-set
   whline-set
   mvhline-set
   mvwhline-set
   vline-set
   wvline-set
   mvvline-set
   mvwvline-set

   ;; clear / clear all or part of a curses window
   erase
   werase
   clear
   wclear
   clrtobot
   wclrtobot
   clrtoeol
   wclrtoeol

   ;; color / curses color manipulation routines
   start-color
   has-colors
   can-change-color
   init-pair
   init-color
   pair-content
   color-content
   init-extended-pair
   init-extended-color
   extended-pair-content
   extended-color-content
   reset-color-pairs
   color-pair
   pair-number
   +COLOR-BLACK+
   +COLOR-RED+
   +COLOR-GREEN+
   +COLOR-YELLOW+
   +COLOR-BLUE+
   +COLOR-MAGENTA+
   +COLOR-CYAN+
   +COLOR-WHITE+

   ;; default_colors / use terminal's default colors
   use-default-colors
   assume-default-colors

   ;; define_key / define a keycode
   define-key

   ;; delch / delete character under the cursor in a curses window
   delch
   wdelch
   mvdelch
   mvwdelch

   ;; deleteln / delete and insert lines in a curses window
   deleteln
   wdeleteln
   insdelln
   winsdelln
   insertln
   winsertln

   ;; extend / miscellaneous curses extensions
   curses-version
   use-extended-names

   ;; getcchar / Get a wide character string and rendition from a cchar_t or set a cchar_t from a wide-character string
   getcchar
   setcchar

   ;; getch / get (or push back) characters from curses terminal keyboard
   getch
   wgetch
   mvgetch
   mvwgetch
   ungetch
   has-key

   ;; get_wch / get (or push back) a wide (multi-byte) character from curses terminal keyboard
   get-wch
   wget-wch
   mvget-wch
   mvwget-wch
   unget-wch
   wint_t

   ;; getstr / accept character strings from curses terminal keyboard
   getstr
   getnstr
   wgetstr
   wgetnstr
   mvgetstr
   mvgetnstr
   mvwgetstr
   mvwgetnstr

   ;; getyx / get curses cursor and window coordinates
   getyx
   getparyx
   getbegyx
   getmaxyx

   ;; inch / get a character and attributes from a curses window
   inch
   winch
   mvinch
   mvwinch

   ;; in_wch / extract a wide character and rendition from a window
   in-wch
   mvin-wch
   win-wch
   mvwin-wch

   ;; inchstr / get a string of characters (and attributes) from a curses window
   inchstr
   inchnstr
   winchstr
   winchnstr
   mvinchstr
   mvinchnstr
   mvwinchstr
   mvwinchnstr

   ;; initscr / Screen initialization and manipulation routines
   initscr
   endwin
   isendwin
   newterm
   set-term
   delscreen

   ;; inopts / Input options.
   cbreak
   nocbreak
   echo
   noecho
   halfdelay
   intrflush
   keypad
   meta
   nodelay
   raw
   noraw
   noqiflush
   qiflush
   notimeout
   timeout
   wtimeout
   typeahead

   ;; insch / insert a character before cursor in a curses window
   insch
   winsch
   mvinsch
   mvwinsch

   ;; ins_wch / insert a complex character and rendition into a window
   ins-wch
   wins-wch
   mvins-wch
   mvwins-wch

   ;; insstr / insert string before cursor in a curses window
   insstr
   insnstr
   winsstr
   winsnstr
   mvinsstr
   mvinsnstr
   mvwinsstr
   mvwinsnstr

   ;; instr / get a string of characters from a curses window
   instr
   innstr
   winstr
   winnstr
   mvinstr
   mvinnstr
   mvwinstr
   mvwinnstr

   ;; inwstr / extract a string of wchar_t characters from a curses window
   inwstr
   innwstr
   winwstr
   winnwstr
   mvinwstr
   mvinnwstr
   mvwinwstr
   mvwinnwstr

   ;; kernel / low-level curses routines
   def-prog-mode
   def-shell-mode
   reset-prog-mode
   reset-shell-mode
   resetty
   savetty
   getsyx
   setsyx
   ripoffline
   curs-set
   napms

   ;; keybound / return definition of keycode
   keybound

   ;; key_defined / check if a keycode is defined
   key-defined

   ;; keyok / enable or disable a keycode
   keyok

   ;; legacy / get curses cursor and window coordinates, attributes
   getattrs
   getbegx
   getbegy
   getcurx
   getcury
   getmaxx
   getmaxy
   getparx
   getpary

   ;; legacy_coding
   use-legacy-coding

   ;; mouse / mouse interface through curses
   has-mouse
   getmouse
   ungetmouse
   mousemask
   wenclose
   mouse-trafo
   wmouse-trafo
   mouseinterval

   ;; move / move curses window cursor
   move
   wmove

   ;; new_pair / new curses color-pair functions
   alloc-pair
   find-pair
   free-pair

   ;; ncurses / CRT screen handling and optimization package
   *library-name*
   *library-file-name*
   libncurses
   libncursesw
   close-library
   chtype
   wchar_t
   wint_t
   attr_t
   cchar
   cchar_t
   ptr-cchar_t
   cchar-attr
   cchar-chars
   cchar-colors
   mmask_t
   mevent
   ERROR
   OK

   ;; opaque / curses window properties
   is-cleared
   is-idcok
   is-idlok
   is-immedok
   is-keypad
   is-leaveok
   is-nodelay
   is-notimeout
   is-pad
   is-scrollok
   is-subwin
   is-syncok
   wgetparent
   wgetscrreg

   ;; outopts / curses output options
   clearok
   idlok
   idcok
   immedok
   leaveok
   scrollok
   setscrreg
   wsetscrreg
   nl
   nonl

   ;; pad / create and display curses pads
   newpad
   subpad
   prefresh
   pnoutrefresh
   pechochar
   pecho-wchar

   ;; refresh / refresh curses windows and lines
   refresh
   wrefresh
   wnoutrefresh
   doupdate
   redrawwin
   wredrawln

   ;; resizeterm / change the curses terminal size
   is-term-resized
   resize-term
   resizeterm

   ;; scroll / scroll a curses window
   scroll
   scrl
   wscrl

   ;; slk / curses soft label routines
   slk-init
   slk-set
   slk-refresh
   slk-noutrefresh
   slk-label
   slk-clear
   slk-restore
   slk-touch
   slk-attron
   slk-attroff
   slk-attrset
   slk-attr-on
   slk-attr-off
   slk-attr-set
   slk-attr
   slk-color

   ;; termattrs / environment query routines
   baudrate
   erasechar
   has-ic
   has-il
   killchar
   longname
   term-attrs
   termattrs
   termname

   ;; touch / curses refresh control routines
   touchwin
   touchline
   untouchwin
   wtouchln
   is-linetouched
   is-wintouched

   ;; util / miscellaneous curses utility routines
   unctrl
   keyname
   filter
   nofilter
   use-env
   putwin
   getwin
   delay-output
   flushinp

   ;; variables / curses global variables
   COLOR-PAIRS
   COLORS
   COLS
   ESCDELAY
   LINES
   TABSIZE
   curscr
   newscr
   stdscr

   ;; window / create curses windows
   newwin
   delwin
   mvwin
   subwin
   derwin
   mvderwin
   dupwin
   wsyncup
   syncok
   wcursyncup
   wsyncdown

   ;; wresize / resize a curses window
   wresize

   ;; libc
   setlocale
   +LC-CTYPE+
   +LC-NUMERIC+
   +LC-TIME+
   +LC-COLLATE+
   +LC-MONETARY+
   +LC-MESSAGES+
   +LC-ALL+
   +LC-PAPER+
   +LC-NAME+
   +LC-ADDRESS+
   +LC-TELEPHONE+
   +LC-MEASUREMENT+
   +LC-IDENTIFICATION+
   ))
