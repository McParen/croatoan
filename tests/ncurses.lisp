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

(defun nctest3 ()
  (%initscr)
  (%start-color)
  (%init-pair 1 1 3) ; red(1) on yellow(3)

  ;; extract and display the foreground and background color numbers from the pair number
  (with-foreign-objects ((ptr-f :short)
                         (ptr-b :short))
    (%pair-content 1 ptr-f ptr-b)
    (%mvaddstr 0 0 (format nil "1 ~A, 3 ~A" (mem-aref ptr-f :short) (mem-aref ptr-b :short))))

  ;; extract and display the RGB contents of predefined color no. 3 (yellow).
  (with-foreign-objects ((ptr-r :short)
                         (ptr-g :short)
                         (ptr-b :short))
    (%color-content 3 ptr-r ptr-g ptr-b)
    (%mvaddstr 1 0 (format nil "~3A ~3A ~3A"
                           (mem-aref ptr-r :short)
                           (mem-aref ptr-g :short)
                           (mem-aref ptr-b :short))))
  (%refresh)
  (%getch)
  (%endwin))
