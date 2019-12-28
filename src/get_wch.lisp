(in-package :de.anvi.croatoan)

(defun get-wide-char (window &key y x)
  "Read in a wide C wchar_t (multi-byte) from the keyboard and return it.

If the destination coordinates y (row) and x (column) are given, move
the cursor to the destination first and then read a multi-byte char.

The window from which the char is read is automatically refreshed."
  (when (and y x) (move window y x))

  (cffi:with-foreign-object (ptr 'wint_t)
    ;; #define KEY_CODE_YES    0400            /* A wchar_t contains a key code */
    ;; if the char is a function key, return t as a second value, otherwise nil.
    (if (= 256 (%wget-wch (winptr window) ptr))
        (values (cffi:mem-ref ptr 'wint_t) t)
        (values (cffi:mem-ref ptr 'wint_t) nil))))

(defun get-wide-event (window)
  "Return a single user input event.

An event can be a lisp character or a keyword representing a function or mouse key.

If input-blocking is nil for the window, return nil if no key was typed."
  (multiple-value-bind (ch function-key-p) (get-wide-char window)
    (cond
      ;; for wide chars, if no input is waiting in non-blocking mode, ERR=0 is returned.
      ;; for normal chars, ERR=-1.
      ((= ch 0) nil)
      (function-key-p
       (let ((ev (function-key ch)))
         (if (eq ev :mouse)
             (multiple-value-bind (mev y x) (get-mouse-event)
               (values mev y x)) ; returns 3 values, see mouse.lisp
             ev)))
      (t (code-char ch)))))
