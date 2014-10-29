(in-package :de.anvi.croatoan)

;; bkgd applies to every char in the window.
;; bkgdset applies only to new chars inserted after the call to bkgdset.
;; i.e. with bkgd we manipulate the existing text, with bkgdset the new text.

;; the attribute part of the background char is compined with any chars added.
  "Set the background character and attributes of a window.

The attribute part of the background character is combined with
characters in the window.

If target is :all, the background setting is applied to all positions
in the window. 

If target is :new, it is applies only to newly added characters."

(defun set-background-char (winptr char &optional (target :whole-window))
  (let ((chtype (convert-char char :chtype)))
    (case target
      (:whole-window (%wbkgd    winptr chtype))
      (:new-chars    (%wbkgdset winptr chtype)))))

(defun get-background-char (winptr)
  "Return the background character and attributes of window."
  (convert-char (%getbkgd winptr) :complex-char))

;;; TODOs

;; make get- return an xchar instead of a chtype.

;; set bg and fg colors of a window without messing around with
;; color pairs.
;;(defun set-window-colors (window &key fg bg)
;;  (let ((pair (get-color-pair fg bg)))
;;    (bkgd window (color-pair pair))))

;; given a fg color and a bg color, return the color pair number.
;;(defun get-color-pair (fg bg)
;;  (gethash (fg . bg) *color-pairs*))
;; for a reference, see lol-evolution.
