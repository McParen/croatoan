(in-package :de.anvi.croatoan.tests)

;; here the ncurses primitive bindings should be tested.

(defun nctest ()
  (%initscr)
  (%mvaddstr 0 0 "hello there")
  (%mvaddstr 7 7 "hello there")
  (%mvaddstr 15 15 "hello there")
  (%refresh)
  (%getch)
  (%endwin))

(defun nctest2 ()
  (let ((scr (%initscr)))
    (%mvaddstr 0 0 "hello there")
    (%mvaddstr 7 7 "hello there")
    (%mvaddstr 15 15 "hello there")
    (%wrefresh scr)
    (%wgetch scr)
    (%endwin)))
