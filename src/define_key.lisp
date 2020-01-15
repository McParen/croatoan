(in-package :de.anvi.croatoan)

;; define_key
;; define a keycode
;; http://invisible-island.net/ncurses/man/define_key.3x.html

(defun define-function-key (key-name char-list &key (key-code nil))
  "Add or replace a new function key defined by the given character sequence.

   - keyname   is  the   symbol   used  to   represent   the  key   by
     croatoan (e.g. :f1);

   - char-list  is  the  raw  sequence of  characters  returned  by  the
     terminal to identify an event (key pressed, mouse event etc.).

   If keycode  is an integer  a new  mapping key-name <->  key-code is
   added  to the  database  overwriting any  already existing  mapping
   which contains either keyname or key-code.

   If  keycode is  nil  (the default)  the mapping  is  added with  an
   unique, unused, generated keycode.

"
  (assert (or (null     key-code)
              (integerp key-code)))
  (let ((control-string (coerce char-list 'string)))
    (if key-code
        (progn
          (add-function-key key-name  key-code)
          (%define-key control-string key-code))
        (let* ((new-key-code (gen-unused-key-code)))
          (add-function-key key-name new-key-code)
          (%define-key control-string new-key-code)))))
