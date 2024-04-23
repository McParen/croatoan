(in-package :de.anvi.croatoan)

;; key_defined
;; check if a keycode is defined
;; http://invisible-island.net/ncurses/man/key_defined.3x.html

(defun function-key-code (definition)
  "Take a function key definition, return the ncurses key code.

If the key is defined, return the key code (as returned by getch or
get_wch), or nil if the key is not defined.

A key definition is an escape sequence of characters that is generated
by the keyboard and recognized by ncurses as a single function key.

The definition can be passed as string or as a list of characters.

Also see: function-key-definition."
  (let ((retval (ncurses:key-defined (typecase definition
                                       (string definition)
                                       (list (coerce definition 'string))))))
    (cond ((= retval 0) nil)
          ((= retval -1) nil)
          (t retval))))
