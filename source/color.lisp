
;; :color-pair (car (rassoc (ash (logand ch (attribute :color)) -8) *color-pair-alist*))))

;;(defun color-attribute-in-lisp (pair-number)
;;  "Takes a color pair number (0-64), returns the appropriate ncurses color attribute.")

#|

(defun add-color-pair (pair-number fg-color-number bg-color-number)
  "Define a new color pair by giving a pair number and two color numbers.

The pair number must be an integer between 1 and (1- +color-pairs+).

The colors are integers from 0 to +colors+ representing already
defined colors.

The default pre-defined color pair 0 is black on white."
  (%init-pair pair-number fg-color-number bg-color-number))


;; this is the end-user function.
;; it relies on the c-backed function color-attribute, but eventually we will reimplement 
;; color-attribute in lisp, because it is just bit-shifting.
;; when it is in lisp only, we can better debug it in slime.
(defun color (fg bg)
  "Take two color keywords, return the color attribute integer."
  (color-attribute (pair2number (cons fg bg))))



;; returns an integer that has to be used whereever a color attribute
;; is expected, for example in set-background (bkgd).
;; this is the lisp wrapper of the ncurses COLOR_PAIR(n) macro.
(defun color-attribute (pair-number)
  "Take a pair number, return an integer representing the color attribute."
  (%color-pair pair-number))


|#