(in-package :de.anvi.croatoan)

(defun get-char (window &key y x)
  "Read in a C char (single byte) from the keyboard and return it.

If the destination coordinates y (row) and x (column) are given, move
the cursor to the destination first and then read a single byte.

The window from which the char is read is automatically refreshed."
  (let ((winptr (winptr window)))
    (cond ((and y x)
           (ncurses:mvwgetch winptr y x))
          (t
           (ncurses:wgetch winptr)))))

(defun unget-char (chtype)
  "Take a simple C chtype and put it back into the read buffer.

It will be read with the next call to get-char."
  (ncurses:ungetch chtype))

(defun key-supported-p (code)
  "Returns t if the code is recognized by the current terminal as a key."
  (ncurses:has-key code))

;; keys above the first 0-255 chars cannot fit in a char variable to be returned by getch.
;; keys with codes 0400 (256) to 0777 (511) are hard-coded (Solaris).
;; 0400 is returned from get_wch when a function key is returned (but only when keypad is enabled).
;; when they return a normal wide char wchar_t, they return OK. (see get_wch.lisp/get-wide-char).
(defparameter *key-alist*
                                       ; #define       oct   dec  cap  curses.h comment
  '((#o400 . :key-code-yes)            ; KEY_CODE_YES  0400  256       /* A wchar_t contains a key code */
    (#o401 . #s(key :name :break))     ; KEY_BREAK     0401  257       /* Break key (unreliable) */
                                       ; KEY_MIN       0401  257       /* Minimum curses key */
    (#o402 . #s(key :name :down))      ; KEY_DOWN      0402  258  kd   /* down-arrow key */
    (#o403 . #s(key :name :up))        ; KEY_UP        0403  259  ku   /* up-arrow key */
    (#o404 . #s(key :name :left))      ; KEY_LEFT      0404  260  kl   /* left-arrow key */
    (#o405 . #s(key :name :right))     ; KEY_RIGHT     0405  261  kr   /* right-arrow key */
    (#o406 . #s(key :name :home))      ; KEY_HOME      0406  262  kh   /* home key */
    (#o407 . #s(key :name :backspace)) ; KEY_BACKSPACE 0407  263  kb   /* backspace key */
    (#o410 . #s(key :name :f0))        ; KEY_F0        0410  264       /* Function keys. Space for 64 keys. */
                                       ; KEY_F(n)      (KEY_F0+(n))    /* Value of function key n */
    ;; F1 to F12
    (#o411 . #s(key :name :f1))
    (#o412 . #s(key :name :f2))
    (#o413 . #s(key :name :f3))
    (#o414 . #s(key :name :f4))
    (#o415 . #s(key :name :f5))
    (#o416 . #s(key :name :f6))
    (#o417 . #s(key :name :f7))
    (#o420 . #s(key :name :f8))
    (#o421 . #s(key :name :f9))
    (#o422 . #s(key :name :f10))
    (#o423 . #s(key :name :f11))
    (#o424 . #s(key :name :f12))

    ;; Shift+F1 (F13) to Shift+F12 (F24)
    (#o425 . #s(key :name :f1 :shift t))
    (#o426 . #s(key :name :f2 :shift t))
    (#o427 . #s(key :name :f3 :shift t))
    (#o430 . #s(key :name :f4 :shift t))
    (#o431 . #s(key :name :f5 :shift t))
    (#o432 . #s(key :name :f6 :shift t))
    (#o433 . #s(key :name :f7 :shift t))
    (#o434 . #s(key :name :f8 :shift t))
    (#o435 . #s(key :name :f9 :shift t))
    (#o436 . #s(key :name :f10 :shift t))
    (#o437 . #s(key :name :f11 :shift t))
    (#o440 . #s(key :name :f12 :shift t))

    ;; Ctrl+F1 (F25) to Ctrl+F12 (F36)
    (#o441 . #s(key :name :f1 :ctrl t))
    (#o442 . #s(key :name :f2 :ctrl t))
    (#o443 . #s(key :name :f3 :ctrl t))
    (#o444 . #s(key :name :f4 :ctrl t))
    (#o445 . #s(key :name :f5 :ctrl t))
    (#o446 . #s(key :name :f6 :ctrl t))
    (#o447 . #s(key :name :f7 :ctrl t))
    (#o450 . #s(key :name :f8 :ctrl t))
    (#o451 . #s(key :name :f9 :ctrl t))
    (#o452 . #s(key :name :f10 :ctrl t))
    (#o453 . #s(key :name :f11 :ctrl t))
    (#o454 . #s(key :name :f12 :ctrl t))

    ;; Shift+Ctrl-F1 (F37) to Shift+Ctrl+F12 (F48)
    (#o455 . #s(key :name :f1 :shift t :ctrl t))
    (#o456 . #s(key :name :f2 :shift t :ctrl t))
    (#o457 . #s(key :name :f3 :shift t :ctrl t))
    (#o460 . #s(key :name :f4 :shift t :ctrl t))
    (#o461 . #s(key :name :f5 :shift t :ctrl t))
    (#o462 . #s(key :name :f6 :shift t :ctrl t))
    (#o463 . #s(key :name :f7 :shift t :ctrl t))
    (#o464 . #s(key :name :f8 :shift t :ctrl t))
    (#o465 . #s(key :name :f9 :shift t :ctrl t))
    (#o466 . #s(key :name :f10 :shift t :ctrl t))
    (#o467 . #s(key :name :f11 :shift t :ctrl t))
    (#o470 . #s(key :name :f12 :shift t :ctrl t))

    ;; Alt+F1 (F49) to Alt+F12 (F60)
    (#o471 . #s(key :name :f1 :alt t))
    (#o472 . #s(key :name :f2 :alt t))
    (#o473 . #s(key :name :f3 :alt t))
    (#o474 . #s(key :name :f4 :alt t))
    (#o475 . #s(key :name :f5 :alt t))
    (#o476 . #s(key :name :f6 :alt t))
    (#o477 . #s(key :name :f7 :alt t))
    (#o500 . #s(key :name :f8 :alt t))
    (#o501 . #s(key :name :f9 :alt t))
    (#o502 . #s(key :name :f10 :alt t))
    (#o503 . #s(key :name :f11 :alt t))
    (#o504 . #s(key :name :f12 :alt t))

    ;; Shift+Alt+F1 (F61) to Shift+Alt+F3 (F63)
    (#o505 . #s(key :name :f1 :shift t :alt t))
    (#o506 . #s(key :name :f2 :shift t :alt t))
    (#o507 . #s(key :name :f3 :shift t :alt t))
                                                           ; #define       oct   dec  cap  curses.h comment
    (#o510 . #s(key :name :delete-line))                   ; KEY_DL        0510  328       /* delete-line key */
    (#o511 . #s(key :name :insert-line))                   ; KEY_IL        0511  329       /* insert-line key */
    (#o512 . #s(key :name :delete))                        ; KEY_DC        0512  330       /* delete-character key */
    (#o513 . #s(key :name :insert))                        ; KEY_IC        0513  331       /* insert-character key */
    (#o514 . #s(key :name :exit-insert-char))              ; KEY_EIC       0514  332       /* sent by rmir or smir in insert mode */
    (#o515 . #s(key :name :clear-screen))                  ; KEY_CLEAR     0515  333       /* clear-screen or erase key */
    (#o516 . #s(key :name :clear-to-end-of-screen))        ; KEY_EOS       0516  334       /* clear-to-end-of-screen key */
    (#o517 . #s(key :name :clear-to-end-of-line))          ; KEY_EOL       0517  335       /* clear-to-end-of-line key */
    (#o520 . #s(key :name :down :shift t))                 ; KEY_SF        0520  336       /* scroll-forward, shift-arrow-down key */
    (#o521 . #s(key :name :up :shift t))                   ; KEY_SR        0521  337       /* scroll-backward, scroll-reverse, shift-arrow-up key */
    (#o522 . #s(key :name :page-down))                     ; KEY_NPAGE     0522  338       /* next-page, page-down key */
    (#o523 . #s(key :name :page-up))                       ; KEY_PPAGE     0523  339       /* previous-page, page-up key */
    (#o524 . #s(key :name :set-tab))                       ; KEY_STAB      0524  340       /* set-tab key */
    (#o525 . #s(key :name :clear-tab))                     ; KEY_CTAB      0525  341       /* clear-tab key */
    (#o526 . #s(key :name :clear-all-tabs))                ; KEY_CATAB     0526  342       /* clear-all-tabs key */
    (#o527 . #s(key :name :enter))                         ; KEY_ENTER     0527  343  @8   /* enter/send key */
    (#o530 . #s(key :name :soft-reset))                    ; KEY_SRESET    0530  344       /* Soft (partial) reset (unreliable) */
    (#o531 . #s(key :name :reset))                         ; KEY_RESET     0531  345       /* Reset or hard reset (unreliable) */
    (#o532 . #s(key :name :print))                         ; KEY_PRINT     0532  346       /* print key */
    (#o533 . #s(key :name :home-down))                     ; KEY_LL        0533  347  kH   /* lower-left key (home down) */

    ;; 3x3 keypad layout:
    ;;
    ;;   A1  |  UP  |  A3
    ;; ------+------+-------
    ;;  LEFT |  B2  | RIGHT
    ;; ------+------+-------
    ;;   C1  | DOWN |  C3
    ;;
    ;; https://pubs.opengroup.org/onlinepubs/7908799/xcurses/curses.h.html
    ;;
    (#o534 . #s(key :name :keypad-upper-left))             ; KEY_A1        0534  348       /* upper left of keypad */
    (#o535 . #s(key :name :keypad-upper-right))            ; KEY_A3        0535  349       /* upper right of keypad */
    (#o536 . #s(key :name :keypad-center))                 ; KEY_B2        0536  350       /* center of keypad */
    (#o537 . #s(key :name :keypad-lower-left))             ; KEY_C1        0537  351       /* lower left of keypad */
    (#o540 . #s(key :name :keypad-lower-right))            ; KEY_C3        0540  352       /* lower right of keypad */

                                                           ; #define       oct   dec  cap  curses.h comment
    (#o541 . #s(key :name :back-tab))                      ; KEY_BTAB      0541  353       /* back-tab key = Shift+Tab */
    (#o542 . #s(key :name :begin))                         ; KEY_BEG       0542  354       /* begin key */
    (#o543 . #s(key :name :cancel))                        ; KEY_CANCEL    0543  355  @2   /* cancel key */
    (#o544 . #s(key :name :close))                         ; KEY_CLOSE     0544  356  @3   /* close key */
    (#o545 . #s(key :name :command))                       ; KEY_COMMAND   0545  357  @4   /* command key */
    (#o546 . #s(key :name :copy))                          ; KEY_COPY      0546  358  @5   /* copy key */
    (#o547 . #s(key :name :create))                        ; KEY_CREATE    0547  359  @6   /* create key */
    (#o550 . #s(key :name :end))                           ; KEY_END       0550  360  @7   /* end key */
    (#o551 . #s(key :name :exit))                          ; KEY_EXIT      0551  361  @9   /* exit key */
    (#o552 . #s(key :name :find))                          ; KEY_FIND      0552  362  @0   /* find key */
    (#o553 . #s(key :name :help))                          ; KEY_HELP      0553  363  %1   /* help key */
    (#o554 . #s(key :name :mark))                          ; KEY_MARK      0554  364  %2   /* mark key */
    (#o555 . #s(key :name :message))                       ; KEY_MESSAGE   0555  365  %3   /* message key */
    (#o556 . #s(key :name :move))                          ; KEY_MOVE      0556  366  %4   /* move key */
    (#o557 . #s(key :name :next))                          ; KEY_NEXT      0557  367  %5   /* next key */
    (#o560 . #s(key :name :open))                          ; KEY_OPEN      0560  368  %6   /* open key */
    (#o561 . #s(key :name :options))                       ; KEY_OPTIONS   0561  369  %7   /* options key */
    (#o562 . #s(key :name :previous))                      ; KEY_PREVIOUS  0562  370  %8   /* previous key */
    (#o563 . #s(key :name :redo))                          ; KEY_REDO      0563  371  %0   /* redo key */
    (#o564 . #s(key :name :reference))                     ; KEY_REFERENCE 0564  372  &1   /* reference key */
    (#o565 . #s(key :name :refresh))                       ; KEY_REFRESH   0565  373  &2   /* refresh key */
    (#o566 . #s(key :name :replace))                       ; KEY_REPLACE   0566  374  &3   /* replace key */
    (#o567 . #s(key :name :restart))                       ; KEY_RESTART   0567  375  &4   /* restart key */
    (#o570 . #s(key :name :resume))                        ; KEY_RESUME    0570  376  &5   /* resume key */
    (#o571 . #s(key :name :save))                          ; KEY_SAVE      0571  377  &6   /* save key */
    (#o572 . #s(key :name :begin :shift t))                ; KEY_SBEG      0572  378  &9   /* shifted begin key */
    (#o573 . #s(key :name :cancel :shift t))               ; KEY_SCANCEL   0573  379  &0   /* shifted cancel key */
    (#o574 . #s(key :name :command :shift t))              ; KEY_SCOMMAND  0574  380  *1   /* shifted command key */
    (#o575 . #s(key :name :copy :shift t))                 ; KEY_SCOPY     0575  381  *2   /* shifted copy key */
    (#o576 . #s(key :name :create :shift t))               ; KEY_SCREATE   0576  382  *3   /* shifted create key */
    (#o577 . #s(key :name :delete :shift t))               ; KEY_SDC       0577  383  *4   /* shifted delete-character key */
    (#o600 . #s(key :name :delete-line :shift t))          ; KEY_SDL       0600  384  *5   /* shifted delete-line key */
    (#o601 . #s(key :name :select))                        ; KEY_SELECT    0601  385  *6   /* select key */
    (#o602 . #s(key :name :end :shift t))                  ; KEY_SEND      0602  386  *7   /* shifted end key */
    (#o603 . #s(key :name :clear-to-end-of-line :shift t)) ; KEY_SEOL      0603  387  *8   /* shifted clear-to-end-of-line key */
    (#o604 . #s(key :name :exit :shift t))                 ; KEY_SEXIT     0604  388  *9   /* shifted exit key */
    (#o605 . #s(key :name :find :shift t))                 ; KEY_SFIND     0605  389  *0   /* shifted find key */
    (#o606 . #s(key :name :help :shift t))                 ; KEY_SHELP     0606  390  #1   /* shifted help key */
    (#o607 . #s(key :name :home :shift t))                 ; KEY_SHOME     0607  391  #2   /* shifted home key */
    (#o610 . #s(key :name :insert :shift t))               ; KEY_SIC       0610  392  #3   /* shifted insert-character key */
    (#o611 . #s(key :name :left :shift t))                 ; KEY_SLEFT     0611  393  #4   /* shifted left-arrow key */
    (#o612 . #s(key :name :message :shift t))              ; KEY_SMESSAGE  0612  394  %a   /* shifted message key */
    (#o613 . #s(key :name :move :shift t))                 ; KEY_SMOVE     0613  395  %b   /* shifted move key */
    (#o614 . #s(key :name :next :shift t))                 ; KEY_SNEXT     0614  396  %c   /* shifted next key */
    (#o615 . #s(key :name :options :shift t))              ; KEY_SOPTIONS  0615  397  %d   /* shifted options key */
    (#o616 . #s(key :name :previous :shift t))             ; KEY_SPREVIOUS 0616  398  %e   /* shifted previous key */
    (#o617 . #s(key :name :print :shift t))                ; KEY_SPRINT    0617  399  %f   /* shifted print key */
    (#o620 . #s(key :name :redo :shift t))                 ; KEY_SREDO     0620  400  %g   /* shifted redo key */
    (#o621 . #s(key :name :replace :shift t))              ; KEY_SREPLACE  0621  401  %h   /* shifted replace key */
    (#o622 . #s(key :name :right :shift t))                ; KEY_SRIGHT    0622  402  %i   /* shifted right-arrow key */
    (#o623 . #s(key :name :resume :shift t))               ; KEY_SRSUME    0623  403  %j   /* shifted resume key */
    (#o624 . #s(key :name :save :shift t))                 ; KEY_SSAVE     0624  404  !1   /* shifted save key */
    (#o625 . #s(key :name :suspend :shift t))              ; KEY_SSUSPEND  0625  405  !2   /* shifted suspend key */
    (#o626 . #s(key :name :undo :shift t))                 ; KEY_SUNDO     0626  406  !3   /* shifted undo key */
    (#o627 . #s(key :name :suspend))                       ; KEY_SUSPEND   0627  407  &7   /* suspend key */
    (#o630 . #s(key :name :undo))                          ; KEY_UNDO      0630  408  &8   /* undo key */
    (#o631 . :mouse)                                       ; KEY_MOUSE     0631  409  Km   /* Mouse event has occurred */
    (#o632 . #s(key :name :resize))                        ; KEY_RESIZE    0632  410       /* Terminal resize event */
    (#o633 . :event)                                       ; KEY_EVENT     0633  411       /* We were interrupted by an event, only available if ncurses is built with --enable-wgetch-events */
    (#o777 . :key-max)))                                   ; KEY_MAX       0777  511       /* Maximum key value is 0633 = 511 */

(defun code-key (code)
  "Return a key struct representing the key code returned by ncurses:getch.

Return nil if the code is unknown.

An existing but unknown code can be added with add-function-key.

A new escape sequence and a new code can be added with define-function-key.

This function is analogous to cl:code-char but for function key structs."
  (let ((pair (assoc code *key-alist*)))
    (when pair
      (cdr pair))))

(defun key-code (key)
  "Return the code associated with a function key struct."
  (let ((pair (rassoc key *key-alist* :test #'equalp)))
    (when pair
      (car pair))))

#|

Extended function keys (function keys with modifiers):

- Function key modifiers are encoded in the capability name.

- The first character, k, shows that the capability is a function key.

- The code for shift, 1, is omitted, because shifted keys are already
  encoded by upcased characters.

- The modifier code, when decreased by 1, contains a bitmask for the
  three supported modifiers Shift, Alt and Control and the combinations.

--

Modifier encoding:

code    dec     bin     mods
----------------------------------------------
1       0       000
2       1       001     S       Shift
3       2       010     A             Alt
4       3       011     AS      Shift Alt
5       4       100     C                 Ctrl
6       5       101     SC      Shift     Ctrl
7       6       110     AC            Alt Ctrl
8       7       111     SAC     Shift Alt Ctrl

--

Terminfo capability table:

key     nomod   S       A       SA      C       SC      AC      SAC
---------------------------------------------------------------------
up      kcuu1   kUP     kUP3    kUP4    kUP5    kUP6    kUP7    kUP8
down    kcud1   kDN     kDN3    kDN4    kDN5    kDN6    kDN7    kDN8
left    kcuf1   kLFT    kLFT3   kLFT4   kLFT5   kLFT6   kLFT7   kLFT8
right   kcub1   kRIT    kRIT3   kRIT4   kRIT5   kRIT6   kRIT7   kRIT8
end     kend    kEND    kEND3   kEND4   kEND5   kEND6   kEND7   kEND8
home    khome   kHOM    kHOM3   kHOM4   kHOM5   kHOM6   kHOM7   kHOM8
insert  kich1   kIC     kIC3    kIC4    kIC5    kIC6    kIC7    kIC8
delete  kdch1   kDC     kDC3    kDC4    kDC5    kDC6    kDC7    kDC8
ppage   kpp     kPRV    kPRV3   kPRV4   kPRV5   kPRV6   kPRV7   kPRV8
npage   knp     kNXT    kNXT3   kNXT4   kNXT5   kNXT6   kNXT7   kNXT8
f1      kf1     kf13    kf49    kf61    kf25    kf37
f2      kf2     kf14    kf50    kf62    kf26    kf38
f3      kf3     kf15    kf51    kf63    kf27    kf39
f4      kf4     kf16    kf52            kf28    kf40
f5      kf5     kf17    kf53            kf29    kf41
f6      kf6     kf18    kf54            kf30    kf42
f7      kf7     kf19    kf55            kf31    kf43
f8      kf8     kf20    kf56            kf32    kf44
f9      kf9     kf21    kf57            kf33    kf45
f10     kf10    kf22    kf58            kf34    kf46
f11     kf11    kf23    kf59            kf35    kf47
f12     kf12    kf24    kf60            kf36    kf48

|#

(defparameter *extended-key-caps*
  '(;; up
    ("kUP3" . #s(key :name :up          :alt t))
    ("kUP4" . #s(key :name :up :shift t :alt t))
    ("kUP5" . #s(key :name :up                 :ctrl t))
    ("kUP6" . #s(key :name :up :shift t        :ctrl t))
    ("kUP7" . #s(key :name :up          :alt t :ctrl t))
    ("kUP8" . #s(key :name :up :shift t :alt t :ctrl t))
    ;; down
    ("kDN3" . #s(key :name :down          :alt t))
    ("kDN4" . #s(key :name :down :shift t :alt t))
    ("kDN5" . #s(key :name :down                 :ctrl t))
    ("kDN6" . #s(key :name :down :shift t        :ctrl t))
    ("kDN7" . #s(key :name :down          :alt t :ctrl t))
    ("kDN8" . #s(key :name :down :shift t :alt t :ctrl t))
    ;; left
    ("kLFT3" . #s(key :name :left          :alt t))
    ("kLFT4" . #s(key :name :left :shift t :alt t))
    ("kLFT5" . #s(key :name :left                 :ctrl t))
    ("kLFT6" . #s(key :name :left :shift t        :ctrl t))
    ("kLFT7" . #s(key :name :left          :alt t :ctrl t))
    ("kLFT8" . #s(key :name :left :shift t :alt t :ctrl t))
    ;; right
    ("kRIT3" . #s(key :name :right          :alt t))
    ("kRIT4" . #s(key :name :right :shift t :alt t))
    ("kRIT5" . #s(key :name :right                 :ctrl t))
    ("kRIT6" . #s(key :name :right :shift t        :ctrl t))
    ("kRIT7" . #s(key :name :right          :alt t :ctrl t))
    ("kRIT8" . #s(key :name :right :shift t :alt t :ctrl t))
    ;; end
    ("kEND3" . #s(key :name :end          :alt t))
    ("kEND4" . #s(key :name :end :shift t :alt t))
    ("kEND5" . #s(key :name :end                 :ctrl t))
    ("kEND6" . #s(key :name :end :shift t        :ctrl t))
    ("kEND7" . #s(key :name :end          :alt t :ctrl t))
    ("kEND8" . #s(key :name :end :shift t :alt t :ctrl t))
    ;; home
    ("kHOM3" . #s(key :name :home          :alt t))
    ("kHOM4" . #s(key :name :home :shift t :alt t))
    ("kHOM5" . #s(key :name :home                 :ctrl t))
    ("kHOM6" . #s(key :name :home :shift t        :ctrl t))
    ("kHOM7" . #s(key :name :home          :alt t :ctrl t))
    ("kHOM8" . #s(key :name :home :shift t :alt t :ctrl t))
    ;; ins
    ("kIC3" . #s(key :name :insert          :alt t))
    ("kIC4" . #s(key :name :insert :shift t :alt t))
    ("kIC5" . #s(key :name :insert                 :ctrl t))
    ("kIC6" . #s(key :name :insert :shift t        :ctrl t))
    ("kIC7" . #s(key :name :insert          :alt t :ctrl t))
    ("kIC8" . #s(key :name :insert :shift t :alt t :ctrl t))
    ;; del
    ("kDC3" . #s(key :name :delete          :alt t))
    ("kDC4" . #s(key :name :delete :shift t :alt t))
    ("kDC5" . #s(key :name :delete                 :ctrl t))
    ("kDC6" . #s(key :name :delete :shift t        :ctrl t))
    ("kDC7" . #s(key :name :delete          :alt t :ctrl t))
    ("kDC8" . #s(key :name :delete :shift t :alt t :ctrl t))
    ;; ppage, pgup
    ("kPRV3" . #s(key :name :page-up          :alt t))
    ("kPRV4" . #s(key :name :page-up :shift t :alt t))
    ("kPRV5" . #s(key :name :page-up                 :ctrl t))
    ("kPRV6" . #s(key :name :page-up :shift t        :ctrl t))
    ("kPRV7" . #s(key :name :page-up          :alt t :ctrl t))
    ("kPRV8" . #s(key :name :page-up :shift t :alt t :ctrl t))
    ;; npage, pgdn
    ("kNXT3" . #s(key :name :page-down          :alt t))
    ("kNXT4" . #s(key :name :page-down :shift t :alt t))
    ("kNXT5" . #s(key :name :page-down                 :ctrl t))
    ("kNXT6" . #s(key :name :page-down :shift t        :ctrl t))
    ("kNXT7" . #s(key :name :page-down          :alt t :ctrl t))
    ("kNXT8" . #s(key :name :page-down :shift t :alt t :ctrl t))))

;; Function keys with all three active modifiers (C,M,S) that for some
;; reason are missing in the xterm terminfo.
(defparameter *extended-key-xterm-sequences*
  (list
   (cons "kUP8"  (list #\esc #\[ #\1 #\; #\8 #\A))
   (cons "kDN8"  (list #\esc #\[ #\1 #\; #\8 #\B))
   (cons "kRIT8" (list #\esc #\[ #\1 #\; #\8 #\C))
   (cons "kLFT8" (list #\esc #\[ #\1 #\; #\8 #\D))
   (cons "kEND8" (list #\esc #\[ #\1 #\; #\8 #\F))
   (cons "kHOM8" (list #\esc #\[ #\1 #\; #\8 #\H))
   (cons "kIC8"  (list #\esc #\[ #\2 #\; #\8 #\~))
   (cons "kDC8"  (list #\esc #\[ #\3 #\; #\8 #\~))
   (cons "kPRV8" (list #\esc #\[ #\5 #\; #\8 #\~))
   (cons "kNXT8" (list #\esc #\[ #\6 #\; #\8 #\~))))

;; called from :around window when :enable-function-keys is t
(defun add-extended-function-keys ()
  "Check if common extended function keys are supported by the terminal.

In most cases these extended keys are existing function keys with
additional modifiers.

If the keys are supported, add them to the key alist, so they can be
returned as valid events."
  (let (new-keys)
    (mapc (lambda (x)
            (destructuring-bind (cap . key) x
              (let ((seq (tigetstr cap)))
                ;; if a key is supported by the terminal, tigetstr will
                ;; return its definition, i.e. the escape sequence.
                (when (and (stringp seq)
                           (function-key-code seq))
                  ;; once we have the escape sequence, we can retrieve
                  ;; the key code which will be returned by getch.
                  (push (cons (function-key-code seq) key) new-keys)))))
          *extended-key-caps*)
    (when new-keys
      (setf *key-alist* (append *key-alist* (nreverse new-keys)))))
  ;; if an 8-cap (key with 3 active modifiers) has not been added,
  ;; pass the xterm seq to define-function-key.
  (mapc (lambda (x)
          (destructuring-bind (cap . seq) x
            (unless (or (stringp (tigetstr cap))
                        (function-key-code seq))
              (define-function-key (cdr (assoc cap *extended-key-caps* :test #'equalp)) seq))))
        *extended-key-xterm-sequences*))

(defmacro access-alist (key find-fn test-fn get-value-fn default)
  "Helper macro for 'key-name-to-code' and 'key-code-to-name'."
  (let ((pair (gensym)))
    `(let ((,pair (,find-fn ,key *key-alist* :test (function ,test-fn))))
       (if ,pair
           (,get-value-fn ,pair)
           ,default))))

(defun key-name-to-code (name &optional (default nil))
  "Return the first code (an integer) from the given keyname (a keyword).

Since we can have more than one key struct with the same name, we can here
only return the first code, which would be the key without modifiers.

If the code does not exist, return the value of the optional parameter: 'default'."
  (let ((pair (rassoc name *key-alist* :key #'key-name)))
    (if pair
        (car pair)
        default)))

(defun key-code-to-name (code &optional (default nil))
  "Take an integer representing the function key code, return a
keyword representing the function key name.

If a name to the given code is not in the key list, return the value
of the optional parameter: 'default'."
  (let ((pair (assoc code *key-alist*)))
    (if pair
        (if (keywordp (cdr pair))
            (cdr pair)
            (key-name (cdr pair)))
        default)))

(defgeneric delete-function-key (object)
  (:documentation
   "Delete one or more (code . key) mappings from *key-alist*."))

(defmethod delete-function-key ((key-name symbol))
  "Delete all keys with the given keyword name from *key-alist*.

Since more than one key can have the same name, becuse key structs can
have different modifier slots, all the keys with the given name are deleted."
  (setf *key-alist* (remove-if (lambda (a)
                                 (if (keywordp (cdr a))
                                     (eq key-name (cdr a))
                                     (eq key-name (key-name (cdr a)))))
                               *key-alist*)))

(defmethod delete-function-key ((key-code integer))
  "Delete the key with the given unique code from *key-alist*."
  (setf *key-alist* (remove-if (lambda (a)
                                 (= key-code (car a)))
                               *key-alist*)))

(defmethod delete-function-key ((key key))
  "Take a key struct, delete the mapping (code . key) from *key-alist*."
  (setf *key-alist* (remove-if (lambda (a)
                                 (equalp key (cdr a)))
                               *key-alist*)))

(defun add-function-key (key code)
  "Add a new mapping (code . key) to *key-alist*.

The codes and keys have to be unique, so if the alist already contains
either the key or the code, any existing mapping will be replaced by
the new one."
  ;; if either key struct or code already exist, remove them from the alist first.
  (let ((alist (remove-if (lambda (a)
                            (or (equalp (cdr a) key)
                                (=      (car a) code)))
                          *key-alist*)))
    ;; then add a new mapping
    (setf *key-alist* (acons code key alist))))

;; this has to run after initscr, because only then the keys are read from terminfo
(defun add-function-key-cap (cap key)
  "Check if a terminal supports an extended function key capability and add it to key-alist."
  (let ((seq (tigetstr cap)))
    (when (stringp seq)
      (add-function-key key
                        (function-key-code seq)))))

;; TODO 200319 we cant add new code numbers if these numbers are already taken by ncurses
;; whats the highest number pre-registered by ncurses?
(defun gen-unused-key-code ()
  "Generate and return an unused key code.

Used by define-function-key when a new escape sequence is added.

To avoid conflicts with existing ncurses code numbers, new numbers start at 1024."
  (let ((new-code (1+ (reduce #'max *key-alist* :key #'car))))
    (if (> new-code 1023)
        new-code
        1024)))

(defun function-key-p (code)
  "Take a single-byte key code returned by get-char, return t if the
number is a known function key, or nil if it is either a char or an
unknown key.

Used in get-event to check the return value of get-char.

get-wide-char/event has a different way to check for function keys."
  (and (> code 255)
       (key-code-to-name code nil)))

;; http://rosettacode.org/wiki/Keyboard_input/Keypress_check
;; Returns t if a key has been pressed and a char can be read by get-char.
;; Requires input-blocking for window to be set to nil.
(defun key-pressed-p (window)
  (let ((ch (get-char window)))
    ;; ncurses get-char returns -1 when no key was pressed.
    (unless (= ch -1)
      ;; if a key was pressed, put it back into the input buffer
      ;; so it can be rad by the next call to get-char.
      (unget-char ch)
      ;; Return t.
      t)))

(defun get-key-event (ch)
  "If the event is mouse, return a mouse event, otherwise a normal event."
  (let ((ev (code-key ch)))
    (if ev
        ;; we have to return mouse as a keyword, because we have to get the struct
        ;; in a second step from get-mouse-event.
        (if (eq ev :mouse)
            ;; a mouse event returns 3 values, see mouse.lisp
            (multiple-value-bind (mev y x mods) (get-mouse-event)
              (make-instance 'mouse-event
                             ;; we only can make the mouse struct here
                             :key (make-key :name mev
                                            :ctrl  (if (member :ctrl  mods) t nil)
                                            :alt   (if (member :alt   mods) t nil)
                                            :shift (if (member :shift mods) t nil))
                             :code ch
                             :y y
                             :x x))
            ;; for normal function keys, return a struct and the code.
            ;; if the key is unknown, ev is nil, but the code is returned.
            (make-instance 'event :key ev :code ch))
        ;; if we have a function key defined in ncurses, but not in the key-alist,
        ;; the event is returned with :unknown as key.
        (make-instance 'event :key (make-key :name :unknown) :code ch))))

;; works only when input-blocking is set to nil. enable-fkeys should also be t.
;; events can be handled with case.
;; events can be nil (no key pressed), characters #\a and function keys like :up, :down, etc.
;; todo: mouse, resizekey

(defun get-event (window)
  "Read a single-byte char from window, return an event object.

The object contains the event key (a simple character or a keyword for
function keys) and the integer key code.

The following chars can be returned:

1. Regular single-byte control and graphic characters with codes 0-255
   are returned as lisp characters.
   Multi-byte characters are returned as a sequence of chars/integers.
   To return a multi-byte char as a single char, use get-wide-event.

2. If input-blocking of the window is set to nil, non-events (idle
   time when no real events occur) are returned as nil with the code -1.

3. If enable-fkeys is set to t, function keys are converted by ncurses
   from an escape sequence to an integer >255 and returned by
   get-event as a lisp keyword name.
   If enable-fkeys is nil, the whole sequence is read and returned."

  ;; doesnt really get a "char", but a single byte, which can be a char.
  (let ((ch (get-char window)))
    (cond
      ;; -1 means no key has been pressed.
      ((= ch -1)
       (make-instance 'event :key nil :code ch))
      ;; 0-255 are regular chars, which can be converted to lisp chars with code-char.
      ((and (>= ch 0) (<= ch 255))
       (make-instance 'event :key (code-char ch) :code ch))
      ;; if the code belongs to a registered function key, return a key struct.
      ((function-key-p ch)
       (get-key-event ch))
      (t
       ;; if we have a code without a corresponding key, return :unknown as key.
       (make-instance 'event :key :unknown :code ch)))))
