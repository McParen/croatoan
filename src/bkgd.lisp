(in-package :de.anvi.croatoan)

;; bkgd applies to every cell in the window.
;; bkgdset applies only to new chars inserted after the call to bkgdset.
;; i.e. with bkgd we manipulate the existing text, with bkgdset the new text.

;; the attribute part of the background char is combined with any chars added.
;; because of that, we cant use alternate chars as background chars, since
;; :altcharset is an attribute.

;; TODO: add test whether xchar is nil
(defun set-background-char (winptr xchar &optional (apply t))
  "Set a complex single-byte character as the background of a window.

The attribute part of the background character is combined with
simple characters in the window.

If apply is t, the background setting is immediately applied to all cells
in the window.

Otherwise, it is applied only to newly added simple characters."
  (let ((chtype (xchar2chtype xchar)))
    (if apply
        ;; the background char is applied to every cell in the window by default.
        (ncurses:wbkgd    winptr chtype)
        ;; if apply is nil, the background is combined only with new characters.
        (ncurses:wbkgdset winptr chtype))))

(defun get-background-char (window)
  "Return the complex char that is the background character of the window."
  (let* ((winptr (winptr window))
         (chtype (ncurses:getbkgd winptr)))
    (chtype2xchar chtype)))
