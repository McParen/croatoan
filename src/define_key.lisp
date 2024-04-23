(in-package :de.anvi.croatoan)

;; define_key
;; define a keycode
;; http://invisible-island.net/ncurses/man/define_key.3x.html

(defun define-function-key (key definition &key (key-code nil))
  "Add or replace a function key defined by a character sequence.

The key has to be given by a key struct, which at least has to contain
the key-name slot.

The key name is a keyword symbol used to represent the key (e.g. :f1).

The definition of the function key can be passed as a list of
characters or as a string.

The definition is the raw (escape) sequence of characters returned by
the terminal to identify an event (key pressed, mouse event, etc.).

If key-code is an integer, a new mapping (code . key) is added to the
*key-alist* database overwriting any already existing mapping which
contains either key or code.

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
                  ;; ... and the key exists
                  (if (rassoc key *key-alist* :test #'equalp)
                      ;; add the new sequence to the code already associated
                      ;; with the key instead of adding a new code
                      ;; we do not want one key associated with more than one code
                      (car (rassoc key *key-alist* :test #'equalp))
                      ;; if code is not given and name doesnt exist
                      ;; add the new name and generate a new code
                      (gen-unused-key-code)))))

    ;; add name and code to *key-alist*
    (add-function-key key code)
    ;; add control sequence and code to the underlying ncurses database.
    (ncurses:define-key control-string code)))
