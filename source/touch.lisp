(in-package :de.anvi.croatoan)

(defun touch (window)
  "Make the next call to refresh rewrite whe whole window by marking the whole window as changed.

Makes it possible to raise unchanged overlapping windows by refreshing."
  (let ((winptr (.winptr window)))
    (%touchwin winptr)))

;;; TODOs

;; [ ] Return values, errors.
;; [ ] Difference to redrawwin.