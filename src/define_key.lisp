(in-package :de.anvi.croatoan)

;; define_key
;; define a keycode
;; http://invisible-island.net/ncurses/man/define_key.3x.html

(defun define-function-key (key-name definition &key (key-code nil))
  "Add or replace a new function key defined by the given character sequence.

key-name is the symbol used to represent the key (e.g. :f1).

The definition of the function key can be passed as a list of
characters or as a string.

The definition is the raw (escape) sequence of characters returned by
the terminal to identify an event (key pressed, mouse event, etc.).

If key-code is an integer, a new mapping (name . code) is added
to the database overwriting any already existing mapping which
contains either key-name or key-code.

If key-code is nil (default) the mapping is added with an unique,
unused, generated keycode."
  (assert (or (null key-code)
              (integerp key-code)))
  (let ((control-string (typecase definition
                          (string definition)
                          (list (coerce definition 'string))))
        (code (if key-code
                  ;; if code is given and name exists
                  ;;   use the new code instead of the existing code
                  ;; if code is given and name does not exist
                  ;;   use the new name and the new code
                  key-code

                  ;; if the new code is NOT given ...
                  (if (assoc key-name *key-alist*)
                      ;; ... and name exists
                      ;; add the new sequence to the code already associated
                      ;; with the name instead of adding a new code
                      ;; we do not want one name associated with more than one code
                      (cdr (assoc key-name *key-alist*))
                      ;; if code is not given and name doesnt exist
                      ;; add the new name and generate a new code
                      (gen-unused-key-code)))))

    ;; add name and code to the croatoan database
    (add-function-key key-name code)
    ;; add control sequence and code to the underlying ncurses database.
    (ncurses:define-key control-string code)))

(defun function-key-code (definition)
  "Take a key definition sequence, return the keycode if the key is defined.

The definition can be passed as string or as a list of characters."
  (let ((retval (ncurses:key-defined (typecase definition
                                       (string definition)
                                       (list (coerce definition 'string))))))
    (cond ((= retval 0) nil)
          ((= retval -1) nil)
          (t retval))))

(defun function-key-definition (code)
  "Take a function key code, return its definition as a string."
  (let* ((ptr (cffi:foreign-funcall "keybound" :int code :int 0 :pointer))
         (str (cffi:foreign-string-to-lisp ptr)))
    ;; keybound returns a C string which must be freed.
    (cffi:foreign-free ptr)
    str))
