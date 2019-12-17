(in-package :de.anvi.croatoan)

(defun alert (&optional (type :beep))
  (case type
    (:beep (%beep))
    (:flash (%flash))
    (otherwise (error "Available alert types: :beep :flash"))))

;;; TODOs

;; [ ] Return values, errors.
