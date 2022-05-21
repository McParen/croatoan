(in-package :de.anvi.croatoan)

(defun draw-wide-border (window &key left right top bottom
                                top-left top-right bottom-left bottom-right)
  "Draw a border around the window using (wide) unicode line-drawing characters.

If no border chars are given, the default ncurses WACS chars will be used."
  (cffi:with-foreign-objects ((ls '(:struct ncurses:cchar_t))
                              (rs '(:struct ncurses:cchar_t))
                              (ts '(:struct ncurses:cchar_t))
                              (bs '(:struct ncurses:cchar_t))
                              (tl '(:struct ncurses:cchar_t))
                              (tr '(:struct ncurses:cchar_t))
                              (bl '(:struct ncurses:cchar_t))
                              (br '(:struct ncurses:cchar_t))
                              (wch 'ncurses:wchar_t 5))
    (apply #'ncurses:wborder-set
           (winptr window)

           ;; take a list of (wide) character codes and empty cchar_t pointers, return a list of cchar_t pointers or null pointers.
           (mapcar #'(lambda (char ptr)
                       (if char
                           ;; if nil, then null-pointer, then the default wacs will be used
                           ;; if not nil, pointer to cchar_t
                           (progn
                             ;; blank the wch array in the struct
                             (dotimes (i 5)
                               (setf (cffi:mem-aref wch 'ncurses:wchar_t i) 0))
                             ;; copy the char code to the wch array
                             (setf (cffi:mem-aref wch 'ncurses:wchar_t) char)
                             ;; assemble the cchar_t using %setcchar
                             (ncurses:setcchar ptr wch 0 0 (cffi:null-pointer))
                             ;; return the pointer to the cchar_t
                             ptr)
                           ;; if the char is not passed, return a null-pointer.
                           (cffi:null-pointer)))
                   ;; list of passed character codes
                   (list left right top bottom top-left top-right bottom-left bottom-right)
                   ;; list of pointers to allocated cchar_t structs
                   (list ls rs ts bs tl tr bl br)))))
