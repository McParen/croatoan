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

;; takes a simple C chtype and puts it back into the read buffer.
;; it will be read with the next get-char.
(defun unget-char (chtype)
  (ncurses:ungetch chtype))

;; takes an C int denoting a key. returns t or nil.
;; checks whether a function key is supported by the current terminal.
(defun key-supported-p (key-char)
  (ncurses:has-key key-char))

(defparameter *key-alist*
  ;; keys above the first 0-255 chars cannot fit in a char variable any more.
  '((:key-code-yes              . 256)
    ;; returned from wide get functions when a function key is returned (but only when keypad is enabled).
    ;; when they return a normal wide char wchar_t, they return OK.

    ;; (:key-min       . 257) ; minimum key value
    (:key-break                 . 257)
    (:key-arrow-down            . 258) ; DOWN      kd
    (:key-arrow-up              . 259) ; UP        ku
    (:key-arrow-left            . 260) ; LEFT      kl
    (:key-arrow-right           . 261) ; RIGHT     kr
    (:key-home                  . 262) ; HOME      kh
    (:key-backspace             . 263) ; BACKSPACE kb
    (:key-f0                    . 264)
    (:key-f1                    . 265)
    (:key-f2                    . 266)
    (:key-f3                    . 267)
    (:key-f4                    . 268)
    (:key-f5                    . 269)
    (:key-f6                    . 270)
    (:key-f7                    . 271)
    (:key-f8                    . 272)
    (:key-f9                    . 273)
    (:key-f10                   . 274)
    (:key-f11                   . 275)
    (:key-f12                   . 276)
    (:key-f13                   . 277)
    (:key-f14                   . 278)
    (:key-f15                   . 279)
    (:key-f16                   . 280)
    (:key-f17                   . 281)
    (:key-f18                   . 282)
    (:key-f19                   . 283)
    (:key-f20                   . 284)
    (:key-f21                   . 285)
    (:key-f22                   . 286)
    (:key-f23                   . 287)
    (:key-f24                   . 288)
    (:key-f25                   . 289)
    (:key-f26                   . 290)
    (:key-f27                   . 291)
    (:key-f28                   . 292)
    (:key-f29                   . 293)
    (:key-f30                   . 294)
    (:key-f31                   . 295)
    (:key-f32                   . 296)
    (:key-f33                   . 297)
    (:key-f34                   . 298)
    (:key-f35                   . 299)
    (:key-f36                   . 300)
    (:key-f37                   . 301)
    (:key-f38                   . 302)
    (:key-f39                   . 303)
    (:key-f40                   . 304)
    (:key-f41                   . 305)
    (:key-f42                   . 306)
    (:key-f43                   . 307)
    (:key-f44                   . 308)
    (:key-f45                   . 309)
    (:key-f46                   . 310)
    (:key-f47                   . 311)
    (:key-f48                   . 312)
    (:key-f49                   . 313)
    (:key-f50                   . 314)
    (:key-f51                   . 315)
    (:key-f52                   . 316)
    (:key-f53                   . 317)
    (:key-f54                   . 318)
    (:key-f55                   . 319)
    (:key-f56                   . 320)
    (:key-f57                   . 321)
    (:key-f58                   . 322)
    (:key-f59                   . 323)
    (:key-f60                   . 324)
    (:key-f61                   . 325)
    (:key-f62                   . 326)
    (:key-f63                   . 327)
    (:key-delete-line           . 328) ; DL
    (:key-insert-line           . 329) ; IL
    (:key-delete-char           . 330) ; DC
    (:key-insert-char           . 331) ; IC
    (:key-exit-insert-char      . 332) ; EIC
    (:key-clear-screen          . 333) ; CLEAR
    (:key-clear-end-of-screen   . 334) ; EOS
    (:key-clear-end-of-line     . 335) ; EOL
    (:key-shift-arrow-down      . 336) ; SF, :key-scroll-forward
    (:key-shift-arrow-up        . 337) ; SR, :key-scroll-reverse
    (:key-next-page             . 338) ; NPAGE
    (:key-previous-page         . 339) ; PPAGE
    (:key-set-tab               . 340) ; STAB
    (:key-clear-tab             . 341) ; CTAB
    (:key-clear-all-tabs        . 342) ; CATAB
    (:key-enter                 . 343) ; ENTER, send, @8
    (:key-soft-reset            . 344) ; SRESET
    (:key-reset                 . 345) ; RESET
    (:key-print                 . 346) ; PRINT, copy
    (:key-home-down             . 347) ; LL, bottom, termcap kH
    (:key-keypad-upper-left     . 348) ; A1
    (:key-keypad-upper-right    . 349) ; A3
    (:key-keypad-center         . 350) ; B2
    (:key-keypad-lower-left     . 351) ; C1
    (:key-keypad-lower-right    . 352) ; C3

#|

https://pubs.opengroup.org/onlinepubs/7908799/xcurses/curses.h.html

3x3 keypad layout:

  A1  |  UP  |  A3
------+------+-------
 LEFT |  B2  | RIGHT
------+------+-------
  C1  | DOWN |  C3

|#

    (:key-back-tab                . 353) ; BTAB, Shift + TAB = #\LATIN_SMALL_LETTER_S_WITH_CARON = sch
    (:key-beginning               . 354) ; BEG
    (:key-cancel                  . 355) ; CANCEL    @2
    (:key-close                   . 356) ; CLOSE     @3
    (:key-command                 . 357) ; COMMAND   @4
    (:key-copy                    . 358) ; COPY      @5
    (:key-create                  . 359) ; CREATE    @6
    (:key-end                     . 360) ; Ende      @7
    (:key-exit                    . 361) ; EXIT      @9
    (:key-find                    . 362) ; FIND      @0
    (:key-help                    . 363) ; HELP      %1
    (:key-mark                    . 364) ; MARK      %2
    (:key-message                 . 365) ; MESSAGE   %3
    (:key-move                    . 366) ; MOVE      %4
    (:key-next                    . 367) ; NEXT      %5
    (:key-open                    . 368) ; OPEN      %6
    (:key-options                 . 369) ; OPTIONS   %7
    (:key-previous                . 370) ; PREVIOUS  %8
    (:key-redo                    . 371) ; REDO      %0
    (:key-reference               . 372) ; REFERENCE &1
    (:key-refresh                 . 373) ; REFRESH   &2
    (:key-replace                 . 374) ; REPLACE   &3
    (:key-restart                 . 375) ; RESTART   &4
    (:key-resume                  . 376) ; RESUME    &5
    (:key-save                    . 377) ; SAVE      &6
    (:key-shift-begin             . 378) ; SBEG      &9
    (:key-shift-cancel            . 379) ; SCANCEL   &0
    (:key-shift-command           . 380) ; SCOMMAND  *1
    (:key-shift-copy              . 381) ; SCOPY     *2
    (:key-shift-create            . 382) ; SCREATE   *3
    (:key-shift-delete-char       . 383) ; SDC       *4
    (:key-shift-delete-line       . 384) ; SDL       *5
    (:key-select                  . 385) ; SELECT    *6
    (:key-shift-end               . 386) ; SEND      *7
    (:key-shift-clear-end-of-line . 387) ; SEOL      *8
    (:key-shift-exit              . 388) ; SEXIT     *9
    (:key-shift-find              . 389) ; SFIND     *0
    (:key-shift-help              . 390) ; SHELP     #1
    (:key-shift-home              . 391) ; SHOME     #2
    (:key-shift-insert-char       . 392) ; SIC       #3
    (:key-shift-arrow-left        . 393) ; SLEFT     #4
    (:key-shift-message           . 394) ; SMESSAGE  %a
    (:key-shift-move              . 395) ; SMOVE     %b
    (:key-shift-next-page         . 396) ; SNEXT     %c
    (:key-shift-options           . 397) ; SOPTIONS  %d
    (:key-shift-previous-page     . 398) ; SPREVIOUS %e
    (:key-shift-print             . 399) ; SPRINT    %f
    (:key-shift-redo              . 400) ; SREDO     %g
    (:key-shift-replace           . 401) ; SREPLACE  %h
    (:key-shift-arrow-right       . 402) ; SRIGHT    %i
    (:key-shift-resume            . 403) ; SRSUME    %j
    (:key-shift-save              . 404) ; SSAVE     !1
    (:key-shift-suspend           . 405) ; SSUSPEND  !2
    (:key-shift-undo              . 406) ; SUNDO     !3
    (:key-suspend                 . 407) ; SUSPEND   &7
    (:key-undo                    . 408) ; UNDO      &8
    (:mouse                       . 409) ; MOUSE     Km
    (:resize                      . 410) ; RESIZE
    (:key-event                   . 411) ; only available if ncurses is built with --enable-wgetch-events
    (:key-max                     . 511))) ; maximum key value

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

;; Modifiers are given in the order they occur in the bitmask: shift-alt-ctrl.
(defparameter *extended-key-caps*
  '(;; up
    ("kUP3" . :key-alt-arrow-up)
    ("kUP4" . :key-shift-alt-arrow-up)
    ("kUP5" . :key-ctrl-arrow-up)
    ("kUP6" . :key-shift-ctrl-arrow-up)
    ("kUP7" . :key-alt-ctrl-arrow-up)
    ("kUP8" . :key-shift-alt-ctrl-arrow-up)
    ;; down
    ("kDN3" . :key-alt-arrow-down)
    ("kDN4" . :key-shift-alt-arrow-down)
    ("kDN5" . :key-ctrl-arrow-down)
    ("kDN6" . :key-shift-ctrl-arrow-down)
    ("kDN7" . :key-alt-ctrl-arrow-down)
    ("kDN8" . :key-shift-alt-ctrl-arrow-down)
    ;; left
    ("kLFT3" . :key-alt-arrow-left)
    ("kLFT4" . :key-shift-alt-arrow-left)
    ("kLFT5" . :key-ctrl-arrow-left)
    ("kLFT6" . :key-shift-ctrl-arrow-left)
    ("kLFT7" . :key-alt-ctrl-arrow-left)
    ("kLFT8" . :key-shift-alt-ctrl-arrow-left)
    ;; right
    ("kRIT3" . :key-alt-arrow-right)
    ("kRIT4" . :key-shift-alt-arrow-right)
    ("kRIT5" . :key-ctrl-arrow-right)
    ("kRIT6" . :key-shift-ctrl-arrow-right)
    ("kRIT7" . :key-alt-ctrl-arrow-right)
    ("kRIT8" . :key-shift-alt-ctrl-arrow-right)
    ;; end
    ("kEND3" . :key-alt-end)
    ("kEND4" . :key-shift-alt-end)
    ("kEND5" . :key-ctrl-end)
    ("kEND6" . :key-shift-ctrl-end)
    ("kEND7" . :key-alt-ctrl-end)
    ("kEND8" . :key-shift-alt-ctrl-end)
    ;; home
    ("kHOM3" . :key-alt-home)
    ("kHOM4" . :key-shift-alt-home)
    ("kHOM5" . :key-ctrl-home)
    ("kHOM6" . :key-shift-ctrl-home)
    ("kHOM7" . :key-alt-ctrl-home)
    ("kHOM8" . :key-shift-alt-ctrl-home)
    ;; ins
    ("kIC3" . :key-alt-insert-char)
    ("kIC4" . :key-shift-alt-insert-char)
    ("kIC5" . :key-ctrl-insert-char)
    ("kIC6" . :key-shift-ctrl-insert-char)
    ("kIC7" . :key-alt-ctrl-isnert-char)
    ("kIC8" . :key-shift-alt-ctrl-insert-char)
    ;; del
    ("kDC3" . :key-alt-delete-char)
    ("kDC4" . :key-shift-alt-delete-char)
    ("kDC5" . :key-ctrl-delete-char)
    ("kDC6" . :key-shift-ctrl-delete-char)
    ("kDC7" . :key-alt-ctrl-delete-char)
    ("kDC8" . :key-shift-alt-ctrl-delete-char)
    ;; ppage, pgup
    ("kPRV3" . :key-alt-previous-page)
    ("kPRV4" . :key-shift-alt-previous-page)
    ("kPRV5" . :key-ctrl-previous-page)
    ("kPRV6" . :key-shift-ctrl-previous-page)
    ("kPRV7" . :key-alt-ctrl-previous-page)
    ("kPRV8" . :key-shift-alt-ctrl-previous-page)
    ;; npage, pgdn
    ("kNXT3" . :key-alt-next-page)
    ("kNXT4" . :key-shift-alt-next-page)
    ("kNXT5" . :key-ctrl-next-page)
    ("kNXT6" . :key-shift-ctrl-next-page)
    ("kNXT7" . :key-alt-ctrl-next-page)
    ("kNXT8" . :key-shift-alt-ctrl-next-page)))

;; called from :around window after enabling function keys.
(defun add-extended-function-keys ()
  "Check if common extended function keys are supported by the terminal.

In most cases these extended keys are existing function keys with
additional modifiers.

If the keys are supported, add them to the key alist, so they can be
returned as valid events."
  (let (new-keys)
    (mapc (lambda (x)
            (destructuring-bind (cap . name) x
              (let ((seq (tigetstr cap)))
                ;; if a key is supported by the terminal, tigetstr will
                ;; return its definition, i.e. the escape sequence.
                (when (and (stringp seq)
                           (function-key-code seq))
                  ;; once we have the escape sequence, we can retrieve
                  ;; the key code which will be returned by getch.
                  (push (cons name (function-key-code seq)) new-keys)))))
          *extended-key-caps*)
    (when new-keys
      (setf *key-alist* (append *key-alist* (nreverse new-keys))))))

(defmacro access-alist (key find-fn test-fn get-value-fn default)
  "Helper macro for 'key-name-to-code' and 'key-code-to-name'."
  (let ((pair (gensym)))
    `(let ((,pair (,find-fn ,key *key-alist* :test (function ,test-fn))))
       (if ,pair
           (,get-value-fn ,pair)
           ,default))))

(defun key-name-to-code (key-name &optional (default nil))
  "Return the code (an integer) from the given keyname (a keyword).

If the code does not exist, return the value of the optional parameter: 'default'."
  (access-alist key-name assoc eq cdr default))

(defun key-code-to-name (code &optional (default nil))
  "Take an integer representing the function key code, return a
keyword representing the function key name.

If a name to the given code is not in the key list, return the value
of the optional parameter: 'default'."
  (access-alist code rassoc = car default))

(defgeneric delete-function-key (object)
  (:documentation "Delete a mapping croatoan keycode <-> curses integer, if exists"))

(defmethod delete-function-key ((key-name symbol))
  "Take a key name, delete the mapping key name (keyword) <-> key code (integer), if it exists.

This operation modifies *key-alist*."
  (setf *key-alist* (remove-if (lambda (a) (eq (car a) key-name))
                               *key-alist*)))

(defmethod delete-function-key ((key-code integer))
  "Take a key code, delete the mapping key name (keyword) <-> key code (number), if it exists.

This operation modifies *key-alist*."
  (setf *key-alist* (remove-if (lambda (a) (= (cdr a) key-code))
                               *key-alist*)))

(defun add-function-key (key-name key-code)
  "Add a new function key given by the mapping of key name (keyword) <->  key code (integer).

If the alist already contains key-name or key-code this mapping will
be overwritten by the new values.

This operation modifies *key-alist*."
  ;; if either key name or code already exist, remove them from the alist first.
  (let ((alist (remove-if (lambda (a)
                            (or (eq (car a) key-name)
                                (=  (cdr a) key-code)))
                          *key-alist*)))
    (setf *key-alist* (acons key-name key-code alist))))

;; TODO 200319 we cant add new code numbers if these numbers are already taken by ncurses
;; whats the highest number pre-registered by ncurses?
(defun gen-unused-key-code ()
  "Generate and return an unused key code.

Used by define-function-key when a new escape sequence is added.

To avoid conflicts with existing ncurses code numbers, new numbers start at 1024."
  (let ((new-code (1+ (reduce #'max *key-alist* :key #'cdr))))
    (if (> new-code 1023)
        new-code
        1024)))

(defun function-key-p (number)
  "Take a single-byte key code returned by get-char, return t if the
number is a known function key, or nil if it is either a char or an
unknown key.

Used in get-event to check the return value of get-char.

get-wide-char/event has a different way to check for function keys."
  (and (> number 255)
       (key-code-to-name number nil)))

;; http://rosettacode.org/wiki/Keyboard_input/Keypress_check
;; Returns t if a key has been pressed and a char can be read by get-char.
;; Requires input-blocking for window to be set to nil.
(defun key-pressed-p (window)
  (let ((ch (get-char window)))
    ;; ncurses get-char returns -1 when no key was pressed.
    (unless (= ch -1)
      ;; if a key was pressed, put it back into the input buffer so it can be rad by the next call to get-char.
      (unget-char ch)
      ;; Return t.
      t)))

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
      ;; 0-255 are regular chars, whch can be converted to lisp chars with code-char.
      ((and (>= ch 0) (<= ch 255))
       (make-instance 'event :key (code-char ch) :code ch))
      ;; if the code belongs to a registered function key, return a keyword symbol.
      ((function-key-p ch)
       (let ((ev (key-code-to-name ch ch)))
         (if (eq ev :mouse)
             ;; a mouse event returns 3 values, see mouse.lisp
             (multiple-value-bind (mev y x mods) (get-mouse-event)
               (make-instance 'mouse-event :key mev :code ch :y y :x x
                                           :modifiers mods))
             ;; for normal function keys, return a keyword and the code.
             (make-instance 'event :key ev :code ch))))
      ;; todo: unknown codes, like mouse, resize and unknown function keys.
      (t
       ;; if we have a code without a corresponding key, return the code as key
       (make-instance 'event :key ch :code ch)))))

#|

#define KEY_CODE_YES    0400            /* A wchar_t contains a key code */
#define KEY_MIN         0401            /* Minimum curses key */

#define KEY_BREAK       0401            /* Break key (unreliable) */
#define KEY_DOWN        0402            /* down-arrow key */
#define KEY_UP          0403            /* up-arrow key */
#define KEY_LEFT        0404            /* left-arrow key */
#define KEY_RIGHT       0405            /* right-arrow key */
#define KEY_HOME        0406            /* home key */
#define KEY_BACKSPACE   0407            /* backspace key */
#define KEY_F0          0410            /* Function keys.  Space for 64 */
#define KEY_F(n)        (KEY_F0+(n))    /* Value of function key n */
#define KEY_DL          0510            /* delete-line key */
#define KEY_IL          0511            /* insert-line key */
#define KEY_DC          0512            /* delete-character key */
#define KEY_IC          0513            /* insert-character key */
#define KEY_EIC         0514            /* sent by rmir or smir in insert mode */
#define KEY_CLEAR       0515            /* clear-screen or erase key */
#define KEY_EOS         0516            /* clear-to-end-of-screen key */
#define KEY_EOL         0517            /* clear-to-end-of-line key */
#define KEY_SF          0520            /* scroll-forward key */
#define KEY_SR          0521            /* scroll-backward key */
#define KEY_NPAGE       0522            /* next-page key */
#define KEY_PPAGE       0523            /* previous-page key */
#define KEY_STAB        0524            /* set-tab key */
#define KEY_CTAB        0525            /* clear-tab key */
#define KEY_CATAB       0526            /* clear-all-tabs key */
#define KEY_ENTER       0527            /* enter/send key */
#define KEY_SRESET      0530            /* Soft (partial) reset (unreliable) */
#define KEY_RESET       0531            /* Reset or hard reset (unreliable) */
#define KEY_PRINT       0532            /* print key */
#define KEY_LL          0533            /* lower-left key (home down) */
#define KEY_A1          0534            /* upper left of keypad */
#define KEY_A3          0535            /* upper right of keypad */
#define KEY_B2          0536            /* center of keypad */
#define KEY_C1          0537            /* lower left of keypad */
#define KEY_C3          0540            /* lower right of keypad */
#define KEY_BTAB        0541            /* back-tab key */
#define KEY_BEG         0542            /* begin key */
#define KEY_CANCEL      0543            /* cancel key */
#define KEY_CLOSE       0544            /* close key */
#define KEY_COMMAND     0545            /* command key */
#define KEY_COPY        0546            /* copy key */
#define KEY_CREATE      0547            /* create key */
#define KEY_END         0550            /* end key */
#define KEY_EXIT        0551            /* exit key */
#define KEY_FIND        0552            /* find key */
#define KEY_HELP        0553            /* help key */
#define KEY_MARK        0554            /* mark key */
#define KEY_MESSAGE     0555            /* message key */
#define KEY_MOVE        0556            /* move key */
#define KEY_NEXT        0557            /* next key */
#define KEY_OPEN        0560            /* open key */
#define KEY_OPTIONS     0561            /* options key */
#define KEY_PREVIOUS    0562            /* previous key */
#define KEY_REDO        0563            /* redo key */
#define KEY_REFERENCE   0564            /* reference key */
#define KEY_REFRESH     0565            /* refresh key */
#define KEY_REPLACE     0566            /* replace key */
#define KEY_RESTART     0567            /* restart key */
#define KEY_RESUME      0570            /* resume key */
#define KEY_SAVE        0571            /* save key */
#define KEY_SBEG        0572            /* shifted begin key */
#define KEY_SCANCEL     0573            /* shifted cancel key */
#define KEY_SCOMMAND    0574            /* shifted command key */
#define KEY_SCOPY       0575            /* shifted copy key */
#define KEY_SCREATE     0576            /* shifted create key */
#define KEY_SDC         0577            /* shifted delete-character key */
#define KEY_SDL         0600            /* shifted delete-line key */
#define KEY_SELECT      0601            /* select key */
#define KEY_SEND        0602            /* shifted end key */
#define KEY_SEOL        0603            /* shifted clear-to-end-of-line key */
#define KEY_SEXIT       0604            /* shifted exit key */
#define KEY_SFIND       0605            /* shifted find key */
#define KEY_SHELP       0606            /* shifted help key */
#define KEY_SHOME       0607            /* shifted home key */
#define KEY_SIC         0610            /* shifted insert-character key */
#define KEY_SLEFT       0611            /* shifted left-arrow key */
#define KEY_SMESSAGE    0612            /* shifted message key */
#define KEY_SMOVE       0613            /* shifted move key */
#define KEY_SNEXT       0614            /* shifted next key */
#define KEY_SOPTIONS    0615            /* shifted options key */
#define KEY_SPREVIOUS   0616            /* shifted previous key */
#define KEY_SPRINT      0617            /* shifted print key */
#define KEY_SREDO       0620            /* shifted redo key */
#define KEY_SREPLACE    0621            /* shifted replace key */
#define KEY_SRIGHT      0622            /* shifted right-arrow key */
#define KEY_SRSUME      0623            /* shifted resume key */
#define KEY_SSAVE       0624            /* shifted save key */
#define KEY_SSUSPEND    0625            /* shifted suspend key */
#define KEY_SUNDO       0626            /* shifted undo key */
#define KEY_SUSPEND     0627            /* suspend key */
#define KEY_UNDO        0630            /* undo key */
#define KEY_MOUSE       0631            /* Mouse event has occurred */
#define KEY_RESIZE      0632            /* Terminal resize event */
#define KEY_EVENT       0633            /* We were interrupted by an event */

#define KEY_MAX         0777            /* Maximum key value is 0633 */

|#
