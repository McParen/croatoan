(in-package :de.anvi.croatoan)

;; bkgd applies to every char in the window.
;; bkgdset applies only to new chars inserted after the call to bkgdset.
;; i.e. with bkgd we manipulate the existing text, with bkgdset the new text.

;; the attribute part of the background char is combined with any chars added.
;; because of that, we cant use alternate chars as background chars, since
;; :altcharset is an attribute.

(defun set-background-char (winptr char &optional (apply t))
  "Set the background character of a window.

The attribute part of the background character is combined with
characters in the window.

If apply is t, the background setting is applied to all characters 
in the window.

Otherwise, it is applied only to newly added characters."
  (let ((chtype (convert-char char :chtype)))
    (if apply
        ;; the background char is applied to every cell in the window by default.
        (%wbkgd    winptr chtype)
        ;; if apply is nil, the background is combined only with new characters.
        (%wbkgdset winptr chtype))))

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
