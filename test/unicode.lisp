(in-package :de.anvi.ncurses.test)

;; Tested in xterm and Gnome Terminal.
;; Doesn't work in the Linux console and aterm.

;; Unicode strings are supported by the default non-unicode ncurses API (addstr) as long as libncursesw is used.
;; Special wide-char string functions (addwstr, add_wchstr) do not have to be used.
(defun ut01 ()
  (initscr)

  (mvaddstr 2 2 "ččććššđđžž")
  (mvaddstr 4 6 "öäüüüßß")
  (mvaddstr 6 8 "Без муки нет науки - no pain, no gain")
  (mvaddstr 8 10 "指鹿為馬 - point deer, make horse")
  (mvaddstr 10 12 "μολὼν λαβέ / ΜΟΛΩΝ ΛΑΒΕ - come and get it")
  
  (refresh)
  (getch)
  (endwin))

;; For displaying single unicode chars, addch functions do not work, and we have to use add_wch explicitly.
;; The data type also changes, from the integral chtype for addch, to the cchar_t struct for add_wch.
;; Use the cchar type for convert-to-foreign via a plist.
;; cchar-chars isnt a pointer to an integer array here, but an integer.
(defun ut02 ()
  (let ((scr (initscr)))

    ;; %add-wch
    ;; Add #\CYRILLIC_SMALL_LETTER_SHA = #\ш to the stdscr.
    (cffi:with-foreign-object (ptr '(:struct cchar))
      (setf ptr (cffi:convert-to-foreign (list 'cchar-attr 0 'cchar-chars (char-code #\ш))
                                    '(:struct cchar)))
      (add-wch ptr))

    ;; %wadd-wch
    ;; #\CYRILLIC_CAPITAL_LETTER_LJE = #\Љ
    (cffi:with-foreign-object (ptr '(:struct cchar))
      (setf ptr (cffi:convert-to-foreign (list 'cchar-attr #x00020000 'cchar-chars (char-code #\Љ))
                                    '(:struct cchar)))
      (wadd-wch scr ptr))

    (wrefresh scr)
    (wgetch scr)
    (endwin)))

(defun ut02b ()
  (let ((scr (initscr)))
    (start-color)
    (init-pair 1 1 3) ; red(1) on yellow(3)

    ;; %wadd-wch
    ;; #\CYRILLIC_CAPITAL_LETTER_LJE = #\Љ
    (cffi:with-foreign-object (ptr '(:struct cchar_t))
      ;; 2 is the attribute, 1 is the color pair, the color doesnt work with convert-to-foreign
      ;;(setf ptr (convert-to-foreign (list 'cchar-attr #x00020100 'cchar-chars (char-code #\Љ) 'cchar-colors 1)
      ;;                              '(:struct cchar_t)))

      ;; we have to set each slot manually in order to make the extended colors work.
      (setf (cffi:foreign-slot-value ptr '(:struct cchar_t) 'cchar-attr) #x00020100)
      (setf (cffi:foreign-slot-value ptr '(:struct cchar_t) 'cchar-colors) 1)
      ;; also with cchar_t, we can not use convert to foreign at all, but have to manually add the char
      ;; at the first place in the char array.
      (setf (cffi:mem-aref
             (cffi:foreign-slot-pointer ptr '(:struct cchar_t) 'cchar-chars)
             'wchar_t
             0)
            (char-code #\Љ))
      (wadd-wch scr ptr))

    (wrefresh scr)
    (wgetch scr)
    (endwin)))

;; We do not need special "wide character" functions for displaying single UTF-8 characters.
;; We can just use the string output function %waddstr.
;; Here, the underlying %waddstr powers the gray stream interface displaying UTF-8.
(defun ut03 ()
  (crt:with-screen (scr)
    ;; Even though we have a control STRING, format still writes the char
    ;; arguments char by char with write-char.
    (format scr "~C ~A" #\Љ #\ш)
    (terpri scr)
    (format scr "Без муки нет науки")
    (terpri scr)
    (format scr "指鹿為馬")
    (terpri scr)
    (format scr "μολὼν λαβέ / ΜΟΛΩΝ ΛΑΒΕ")
    (terpri scr)
    (crt:refresh scr)
    (crt:get-char scr)))

;; Use the cchar_t type for setcchar.
;; cchar-chars is a pointer to an integer array here, like the C prototype requires.
(defun ut04 ()
  (let ((scr (initscr)))
    (start-color)
    ;; Initialize color pair 1, yellow 3 on red 1.
    (init-pair 1 3 1)

    (cffi:with-foreign-objects ((ptr '(:struct cchar_t))
                                (wch 'wchar_t 5))
      ;; Reset the wch array to zero.
      (dotimes (i 5) (setf (cffi:mem-aref wch 'wchar_t i) 0))
      (setf (cffi:mem-aref wch 'wchar_t) (char-code #\ш))
      ;; Create a cchar_t containing #\ш, attribute underline, color pair yellow on red.
      (setcchar ptr wch #x00020000 1 (cffi:null-pointer))
      (wadd-wch scr ptr)
      (waddch scr (char-code #\newline))
      ;; Take a look at the plist convert-from-foreign returns from cchar_t.
      ;; Sadly that plist cant be read back because it contains a pointer.
      ;; As of now, the convert-to-foreign function requires foreign values, not pointers.
      (waddstr scr (princ-to-string (cffi:convert-from-foreign ptr '(:struct cchar)))))

    (wrefresh scr)
    (wgetch scr)
    (endwin)))

;; 126 works 7bit
;; 161 doesn't work 8bit
;; #x2592 doesn't work wide char
(defun ut05 (&optional (n #x2592))
  "Minimal example of creating a cchar and setting it as a window background."

  ;; call setlocale explicitly because it is not called any more by SBCL
  ;; sbcl-2.0.3/src/runtime/runtime.c 
  (setlocale +LC-ALL+ "")

  (let ((scr (initscr)))
    (cffi:with-foreign-objects ((ptr '(:struct cchar_t))
                                (wch 'wchar_t 5))
      (dotimes (i 5)
        (setf (cffi:mem-aref wch 'wchar_t i) 0))
      (setf (cffi:mem-aref wch 'wchar_t 0) n)
      (setcchar ptr wch 0 0 (cffi:null-pointer))

      ;; TODO 200404 works with ncurses 6.1 but not with 6.2
      ;; manually replacing line 209: SetChar2(*cp, CharOf(new_char));
      ;; with: win->_line[y].text[x] = win->_nc_bkgd;
      ;; from ncurses 6.1 is an ugly, but working solution for wbkgrnd
      ;; in ncurses-6.2-20200404/ncurses/base/lib_bkgd.c.
      (wbkgrnd scr ptr)
      
      (wrefresh scr)
      (wgetch scr)
      (endwin))))
