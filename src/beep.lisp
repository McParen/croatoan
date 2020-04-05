(in-package :de.anvi.croatoan)

(defun alert (&optional (type :beep))
  (case type
    (:beep (ncurses:beep))
    (:flash (ncurses:flash))
    (otherwise (error "Available alert types: :beep :flash"))))

;;; TODOs

;; [ ] Return values, errors.
