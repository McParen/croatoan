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
    
    (%wattron scr #x00020000)
    (%mvaddstr 7 7 "hello there")
    (%wattroff scr #x00020000)

    (%wattron scr #x80000000)
    (%mvaddstr 15 15 "hello there")
    (%wattroff scr #x80000000)

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
  (let ((scr (%initscr)))
    (%start-color)
    (%init-pair 1 1 3) ; red(1) on yellow(3)

    (with-foreign-objects ((ptr '(:struct cchar_t))
                           (wch 'wchar_t 5))
      (dotimes (i 5) (setf (mem-aref wch 'wchar_t i) 0))
      (setf (mem-aref wch 'wchar_t) (char-code #\a))
      ;;(%setcchar ptr wch attr_t color-pair-number (null-pointer))
      (%setcchar ptr wch #x00020000 1 (null-pointer))
      (%wadd-wch scr ptr))

    ;; access the struct slots directly using slot pointers
    (with-foreign-object (ptr '(:struct cchar_t))
      (%mvwin-wch scr 0 0 ptr)
      (let* ((char (mem-aref (foreign-slot-pointer ptr '(:struct cchar_t) 'cchar-chars) 'wchar_t 0))
             (col (foreign-slot-value ptr '(:struct cchar_t) 'cchar-colors))
             (attr (foreign-slot-value ptr '(:struct cchar_t) 'cchar-attr)))
        ;; char code
        (%mvaddstr 1 0 (format nil "~A" char))
        ;; color pair number
        (%mvaddstr 2 0 (format nil "~A" col))
        ;; attr_t in hex.
        (%mvaddstr 3 0 (format nil "~8,'0x" attr))))

    ;; deconstruct cchar_t using getcchar
    (with-foreign-objects ((wcval '(:struct cchar_t))
                           (wch 'wchar_t 5)
                           (attrs 'attr_t)
                           (color-pair :short))
      (dotimes (i 5) (setf (mem-aref wch 'wchar_t i) 0))
      (%mvwin-wch scr 0 0 wcval)
      (%getcchar wcval wch attrs color-pair (null-pointer))
      
      (%mvaddstr 5 0 (format nil "~A" (mem-aref wch        'wchar_t 0)))
      (%mvaddstr 6 0 (format nil "~A" (mem-aref color-pair :short)))
      (%mvaddstr 7 0 (format nil "~8,'0x" (mem-aref attrs      'attr_t))))

    (%refresh)
    (%getch)
    (%endwin)))

;; 190302
(defun nctest5 ()
  (let ((scr (%initscr)))
    (%addstr (format nil "~A~%" "no background "))
    (%wgetch scr)

    (%wbkgd scr (char-code #\-))
    (%addstr (format nil "~A~%" "background minus "))
    (%wgetch scr)

    (%wbkgd scr (char-code #\*))
    (%addstr (format nil "~A~%" "background star "))
    (%wgetch scr)

    (%wbkgd scr (char-code #\-))
    (%addstr (format nil "~A~%" "background minus "))
    (%wgetch scr)

    (%wbkgd scr (char-code #\+))
    (%addstr (format nil "~A~%" "background plus "))
    (%wgetch scr)
    
    (%wrefresh scr)
    (%wgetch scr)
    (%endwin)))
