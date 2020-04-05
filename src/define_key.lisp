(in-package :de.anvi.croatoan)

;; define_key
;; define a keycode
;; http://invisible-island.net/ncurses/man/define_key.3x.html

(defun define-function-key (key-name char-list &key (key-code nil))
  "Add or replace a new function key defined by the given character sequence.

key-name is the symbol used to represent the key (e.g. :f1).

char-list is the raw sequence of characters returned by the terminal
to identify an event (key pressed, mouse event, etc.).

If key-code is an integer a new mapping (name . code) is added
to the database overwriting any already existing mapping which
contains either key-name or key-code.

If key-code is nil (default) the mapping is added with an unique,
unused, generated keycode."
  (assert (or (null key-code)
              (integerp key-code)))
  (let ((control-string (coerce char-list 'string))
        (code (if key-code
                  ;; if code is given and name exists
                  ;;   use the new code insteaad of the existing code
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
