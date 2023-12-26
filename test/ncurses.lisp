(in-package :de.anvi.ncurses.test)

;; here the ncurses primitive bindings should be tested.

(defun nctest ()
  "Minimal example: init, output, refresh, end."
  (initscr)
  (mvaddstr 0 0 "hello there")
  (mvaddstr 7 7 "hello there")
  (mvaddstr 15 15 "hello there")
  (refresh)
  (getch)
  (endwin))

(defun nctest2 ()
  "Save and use the screen pointer, set attributes by their bitmask."
  (let ((scr (initscr)))
    (mvaddstr 0 0 "hello there")

    (wattron scr #x00020000)
    (mvaddstr 7 7 "hello there")
    (wattroff scr #x00020000)

    (wattron scr #x80000000)
    (mvaddstr 15 15 "hello there")
    (wattroff scr #x80000000)

    (wrefresh scr)
    (wgetch scr)
    (endwin)))

(defun nctest3 ()
  "Start color, init a color pair, examine color and pair contents."
  (initscr)
  (start-color)
  (init-pair 1 1 3) ; red(1) on yellow(3)

  ;; extract and display the foreground and background color numbers from the pair number
  (cffi:with-foreign-objects ((ptr-f :short)
                              (ptr-b :short))
    (pair-content 1 ptr-f ptr-b)
    (mvaddstr 0 0 (format nil "1 ~A, 3 ~A" (cffi:mem-aref ptr-f :short) (cffi:mem-aref ptr-b :short))))

  ;; extract and display the RGB contents of predefined color no. 3 (yellow).
  (cffi:with-foreign-objects ((ptr-r :short)
                              (ptr-g :short)
                              (ptr-b :short))
    (color-content 3 ptr-r ptr-g ptr-b)
    (mvaddstr 1 0 (format nil "~3A ~3A ~3A"
                           (cffi:mem-aref ptr-r :short)
                           (cffi:mem-aref ptr-g :short)
                           (cffi:mem-aref ptr-b :short))))

  ;; set red on yellow as the window color
  (color-set 1 (cffi:null-pointer))
  (mvaddstr 5 0 "hello")

  ;; problem: even though color 3 (yellow) is a predefined color, it is NOT predefined with the
  ;; predefined ncurses rgb value, 680, but with some other rgb, probably the terminal default palette
  ;; since the colors xterm shows are different than what gnome-terminal shows.

  ;; also, initializing the color redefines the color everywhere, not just for subsequent uses of that color.

  ;; so we can _NOT_ print the rgb values of the 256 colors, because they are terminal specific and cant
  ;; be queried, we just get nonsensical values from ncurses.

  ;; but we can find the rgb values for the xterm colors on the web, and then define these in ncurses.

  ;; before we do init-color, we should do can-change-color
  ;; can-change-color checks whether we can change what color is displayed by a color number.
  ;; some terminals have a hard coded palette.

  ;; BOLD simply makes the color brighter. if red is 680, then red bold is 1000.
  ;; so what happens when we apply bold to 1000? nothing?

  (getch)

  (init-color 3 1000 680 0)
  (mvaddstr 6 0 "hello")

  ;; extract and display the RGB contents of predefined color no. 3 (yellow).
  (cffi:with-foreign-objects ((ptr-r :short)
                              (ptr-g :short)
                              (ptr-b :short))
    (color-content 3 ptr-r ptr-g ptr-b)
    (mvaddstr 8 0 (format nil "~3A ~3A ~3A"
                          (cffi:mem-aref ptr-r :short)
                          (cffi:mem-aref ptr-g :short)
                          (cffi:mem-aref ptr-b :short))))
  (refresh)
  (getch)
  (endwin))

(defun nctest4 ()
  "Test low-level cchar_t reading and writing.

The output is:

a          rendered cchar_t
97         code of character #\a
1          color pair 1
00020100   attribute underline #x00020000 OR-ed with bit-shifted color pair 1

We see that the attr_t slot contains _both_ the attribute _and_ the
bit-shifted color pair, as if it were a chtype in ABI5.

When ABI6 is used, the separate color-pair slot contains the same color
pair number.

The goal is obviously to make the cchar_t usable under both ABI5 and ABI6."
  (let ((scr (initscr)))
    (start-color)
    (init-pair 1 1 3) ; red(1) on yellow(3)

    (cffi:with-foreign-objects ((ptr '(:struct cchar_t))
                                (wch 'wchar_t 5))
      (dotimes (i 5) (setf (cffi:mem-aref wch 'wchar_t i) 0))
      (setf (cffi:mem-aref wch 'wchar_t) (char-code #\a))
      ;;(setcchar ptr wch attr_t color-pair-number (null-pointer))
      (setcchar ptr wch #x00020000 1 (cffi:null-pointer))
      (wadd-wch scr ptr))

    ;; access the struct slots directly using slot pointers
    (cffi:with-foreign-object (ptr '(:struct cchar_t))
      (mvwin-wch scr 0 0 ptr)
      (let* ((char (cffi:mem-aref (cffi:foreign-slot-pointer ptr '(:struct cchar_t) 'cchar-chars) 'wchar_t 0))
             (col (cffi:foreign-slot-value ptr '(:struct cchar_t) 'cchar-colors))
             (attr (cffi:foreign-slot-value ptr '(:struct cchar_t) 'cchar-attr)))
        ;; char code
        (mvaddstr 1 0 (format nil "~A" char))
        ;; color pair number
        (mvaddstr 2 0 (format nil "~A" col))
        ;; attr_t in hex.
        (mvaddstr 3 0 (format nil "~8,'0x" attr))))

    ;; deconstruct cchar_t using getcchar
    (cffi:with-foreign-objects ((wcval '(:struct cchar_t))
                                (wch 'wchar_t 5)
                                (attrs 'attr_t)
                                (color-pair :short))
      (dotimes (i 5) (setf (cffi:mem-aref wch 'wchar_t i) 0))
      (mvwin-wch scr 0 0 wcval)
      (getcchar wcval wch attrs color-pair (cffi:null-pointer))

      (mvaddstr 5 0 (format nil "~A" (cffi:mem-aref wch 'wchar_t 0)))
      (mvaddstr 6 0 (format nil "~A" (cffi:mem-aref color-pair :short)))
      (mvaddstr 7 0 (format nil "~8,'0x" (cffi:mem-aref attrs 'attr_t))))

    (refresh)
    (getch)
    (endwin)))

;; 190302
(defun nctest5 ()
  "Test setting background characters."
  (let ((scr (initscr)))
    (addstr (format nil "~A~%" "no background "))
    (wgetch scr)

    (wbkgdset scr (char-code #\-))
    (addstr (format nil "~A~%" "background minus "))
    (wgetch scr)

    (wbkgd scr (char-code #\*))
    (addstr (format nil "~A~%" "background star "))
    (wgetch scr)

    (wbkgd scr (char-code #\-))
    (addstr (format nil "~A~%" "background minus "))
    (wgetch scr)

    (wbkgd scr (char-code #\+))
    (addstr (format nil "~A~%" "background plus "))
    (wgetch scr)

    (wrefresh scr)
    (wgetch scr)
    (endwin)))

;; 190826
(defun nctest6 ()
  "Test the interaction of the window foreground color and background char."
  (let ((scr (initscr)))
    (start-color)
    (init-pair 1 1 3) ; red(1) on yellow(3)
    (init-pair 2 2 7) ; green(2) on white(7)

    ;; set the window foreground color
    (color-set 1 (cffi:null-pointer))
    (addstr (format nil "color-set: red on yellow~%"))
    (getch)

    ;; a background color overwrites the window color.
    (bkgdset (logior (char-code #\.) (color-pair 2)))
    (addstr (format nil "bkgd: green on white~%"))
    (getch)

    (addch (char-code #\a))
    (addch (char-code #\b))
    ;; trying to write space will actually write the background char #\.
    (addch (char-code #\space))
    (addch (char-code #\space))
    (addch (char-code #\c))
    (getch)

    ;; the next call to color-set again overwrites the color set by bkgd
    (color-set 1 (cffi:null-pointer))
    (addstr (format nil "color-set: red on yellow~%"))
    (getch)

    ;; (erase) and (clear) use the last bkgd char to overwrite the window.
    (clear)
    (getch)

    ;; here we only set a color as the bg char, the default bg char is space
    (bkgdset (color-pair 2))
    (addstr (format nil "bkgdset: green on white~%"))
    (getch)

    ;; the default color pair is black on white
    (bkgd (color-pair 0))
    (addstr (format nil "bkgd: default colors~%"))
    (getch)

    (clear)
    (getch)

    ;; color-set (foreground) and bgkd write to the same global window variable.
    ;; here we se both at the same time
    (bkgdset (logior (char-code #\.) (color-pair 2)))
    (color-set 1 (cffi:null-pointer))
    (addstr (format nil "bkgd green on white, then color-set: red on yellow~%"))
    (refresh)

    (getch)

    (endwin)))

(defun nctest7 ()
  "Interaction between the default color pair 0 and new color pairs."
  (let ((scr (initscr)))
    (start-color)

    ;; init-pair 0 7 0 ; white(7) on black (0) - default color pair
    (init-pair 1 1 3) ; red(1) on yellow(3)
    (init-pair 2 7 0) ; white(7) on black(0)

    (addch (char-code #\a))

    (color-set 1 (cffi:null-pointer))

    (addch (char-code #\b))

    ;; when we set a color pair to a window, ncurses wont let use use the default color pair any more
    ;; the default color pair is ignored and overwritten by the window color pair
    (addch (logior (char-code #\c) (color-pair 0)))

    ;; we can actually use the default color pair, but have to give it another color pair number
    (addch (logior (char-code #\d) (color-pair 2)))

    ;; set the color back to default
    (color-set 0 (cffi:null-pointer))
    (addch (char-code #\e))

    (refresh)
    (getch)
    (endwin)))

;; 190901
(defun nctest8 ()
  "Usage of terminal default colors, when different from black on white."
  (let ((scr (initscr)))
    (start-color)

    ;;(use-default-colors)
    ;; is the same as
    ;;(assume-default-colors -1 -1)

    ;; we can not combine any other color with -1
    ;; as soon as one color (fg or g) is -1, the other automatically is set to -1.
    ;; this is the case for unrendered characters added without color attributes.
    (assume-default-colors 5 -1)

    ;; init-pair 0 7 0 ; white(7) on black (0) - default color pair
    (init-pair 1 1 3) ; red(1) on yellow(3)
    (init-pair 2 7 0) ; white(7) on black(0)

    ;; -1 are not the terminal colors, but the (assumed) default colors.
    (init-pair 3 -1 -1)

    ;; as soon as one of the assumed default colors is -1,
    ;; the other is also set to -1 for unrendered characters.
    (addch (char-code #\a))

    ;; when we reference 5 -1 in a color pair, then the mixed pair works.
    (addch (logior (char-code #\d) (color-pair 3)))

    (addch (logior (char-code #\b) (color-pair 1)))
    (addch (logior (char-code #\c) (color-pair 2)))

    (refresh)
    (getch)
    (endwin)))

;; 200517
(defun nctest9 ()
  "Test precedence of the background char. Compare with t02d."
  (let ((scr (initscr)))
    (noecho)
    (start-color)
    (init-pair 1 1 3) ; red(1) on yellow(3)
    (init-pair 2 2 7) ; green(2) on white(7)

    (bkgd (logior (char-code #\.) (color-pair 1)))
    (refresh)
    (getch)

    ;; trying to write space will actually write the background char #\.
    (addch (char-code #\space))
    (refresh)
    (getch)

    ;; but when the char has a color, the space is written instead of the background char.
    (addch (logior (char-code #\space) (color-pair 2)))
    (refresh)
    (getch)

    ;; the screen is cleared using the background char.
    (clear)
    (refresh)
    (getch)
    (endwin)))

;; 210902
(defun nctest10 ()
  "Use new pair functions: find-pair, alloc-pair, free-pair and reset-color-pairs."
  (initscr)
  (start-color)
  (let ((pair (if (= -1 (find-pair 1 3)) ; red (1) on yellow (3)
                  ;; alloc-pair replaces init-pair to define a new pair
                  (alloc-pair 1 3)
                  ;; if the pair already exists, return its number
                  (find-pair 1 3))))
    (mvaddstr 0 0 "default white on black")
    (refresh)
    (getch)
    (color-set pair (cffi:null-pointer))
    (mvaddstr 1 0 "red on yellow")
    (refresh)
    (getch)
    ;; mark the pair as unused, so alloc can use it to define a new pair
    ;; but the color can still be used until it is redefined.
    (free-pair pair)
    (mvaddstr 2 0 "red on yellow after free-pair")
    (refresh)
    (getch)
    ;; erases defined color pairs everywhere theyre used (back to black on black)
    (reset-color-pairs)
    (color-set 0 (cffi:null-pointer))
    (mvaddstr 3 0 "default white on black")
    (refresh)
    (getch)
    (endwin)))

;; 210906
(defun nctest11 ()
  "Use wresize to resize a window."
  (initscr)
  (let ((win (newwin 5 10 2 10)))
    (wbkgd win (char-code #\-))
    (wrefresh win)
    (wgetch win)
    ;; the added area is filled with the background character.
    (wresize win 10 20)
    (wbkgd win (char-code #\.))
    (wrefresh win)
    (wgetch win)

    (wresize win 5 10)
    (wbkgd win (char-code #\+))
    (refresh)
    (wrefresh win)
    (wgetch win)

    (delwin win)
    (endwin)))

;; 211212
(defun nctest12 ()
  "Basic use of the ncurses mouse functions."
  (let ((scr (initscr)))
    (noecho)
    (cbreak)
    ;; we have to call keypad so mouse events are returned as a single
    ;; event code instead of a whole escape sequence
    (keypad scr t)
    ;; 32bit bitmask of mouse events to return
    (mousemask #b00000111111111111111111111111111 (cffi:null-pointer))
    (getch)
    ;; after a general mouse event has been returned by getch,
    ;; getmouse will return a struct with specific mouse event details.
    (cffi:with-foreign-object (me '(:struct mevent))
      (format t "~A" (getmouse me))
      ;; return the mouse event struct as a plist and print it
      (addstr (format nil "~A" (cffi:mem-ref me '(:struct mevent)))))
    (refresh)
    (getch)
    (endwin)))

;; 230822
(defun nctest13 ()
  "Test of soft-label key functions (SLK)."
  (slk-init 3)
  (initscr)
  (cbreak)
  (loop for i from 1 to 8 do
    (slk-set i (format nil "t~A" i) 0))
  (slk-refresh)
  (getch)
  (endwin))

(defun nctest14 ()
  "Query the terminfo database for function key capabilities."
  (let ((scr (initscr)))
    (keypad scr t)
    (use-extended-names t)
    (let* ((arr (cffi:foreign-symbol-pointer "strnames")))
      (loop for i from 0
            for cap = (cffi:mem-aref arr :string i)
            do (if cap
                   ;; only return capabilities starting with k (function keys)
                   (when (char= (char cap 0) #\k)
                     (format t "~A " cap))
                   ;; mem-aref returns nil for a null-terminated array.
                   (loop-finish))))
    (endwin)))

(defun make-invalid-pointer (size)
  "Take a pointer size 64 bit or 32 bit, return invalid #XFFF.. pointer.

The use of this pointer is to match the error response of some functions,
for example tigetstr, which return the error code (char *) -1, which is
an invalid pointer pointing to a negative address or to the largest
possible address (void *) -1 == (size_t) -1."
  ;; 64 bit, default
  ;; #.(SB-SYS:INT-SAP #XFFFFFFFFFFFFFFFF)
  ;; 32 bit
  ;; #.(SB-SYS:INT-SAP #XFFFFFFFF)
  (cffi:make-pointer (ldb (byte size 0) -1)))

(defun nctest15 ()
  "Get and print the key code of a non-standard terminal capability."
  (let ((scr (initscr)))
    ;; make ncurses recognize function keys
    (keypad scr t)

    (let* ((seq (tigetstr "kEND3")))
      (cond
        ;; invalid negative pointer
        ((or (cffi:pointer-eq seq (make-invalid-pointer 64))
             (cffi:pointer-eq seq (make-invalid-pointer 32)))
         (endwin)
         (print "Capability is not string-valued."))

        ;; null pointer
        ((cffi:pointer-eq seq (cffi:null-pointer))
         (endwin)
         (print "Capability absent from terminal description."))

        ;; all other (valid) string pointers
        (t
         (let ((code (key-defined seq)))
           (print (coerce (cffi:foreign-string-to-lisp seq) 'list))
           (endwin)
           (print code)))))))
