(in-package :de.anvi.croatoan)

;; define_key
;; define a keycode
;; http://invisible-island.net/ncurses/man/define_key.3x.html

(defun define-function-key (key-name char-list)
  "Add a new function key defined by the given character sequence."
  (let ((max-key-code (reduce #'max *key-alist* :key #'cdr))
        (control-string (coerce char-list 'string)))
    (setf *key-alist* (acons key-name (1+ max-key-code) *key-alist*))
    (%define-key control-string (1+ max-key-code))))
