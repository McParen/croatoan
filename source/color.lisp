(in-package :de.anvi.croatoan)

;; 1000 -> 255 (ff)
(defun color-1k-to-8bit (integer)
  "Convert the ncurses color intensity 0-1000 to the 8bit range 0-255."
  (values (round (* integer 0.255))))

;; 255 (ff) -> 1000
(defun color-8bit-to-1k (integer)
  "Convert the 8bit range 0-255 to the ncurses color intensity 0-1000."
  (values (round (* integer (/ 1 0.255)))))

;; https://www.w3schools.com/colors/colors_rgb.asp
;; 255 255 255 -> ffffff
;; the hex triplet is a 6 digit, 3 byte (24 bit) hexadecimal number
;; 255 192 203 :pink
(defun rgb-to-hex (rgb-list)
  "Take a list of three 8bit (0-255) RGB values, return a 24bit integer (RGB hex triplet)."
  (let ((r (nth 0 rgb-list))
        (g (nth 1 rgb-list))
        (b (nth 2 rgb-list)))
    (logior (ash r 16)
            (ash g  8)
            (ash b  0))))
;; test: (format t "~x" (rgb-to-hex (list 255 255 0)))

;; (hex-to-rgb #xffffff) => (255 255 255)
(defun hex-to-rgb (hex)
  "Take a 24bit integer (RGB hex triplet), return a list of three 8bit (0-255) RGB values."
  (let ((r (ldb (byte 8 16) hex))
        (g (ldb (byte 8  8) hex))
        (b (ldb (byte 8  0) hex)))
    (list r g b)))

;;(defun hex-to-string (hex)
;;  (format nil "~6,'0x" hex))

;; SGR = select graphc rendition, vt100 attribute sequences

;; xterm sources:
;; https://github.com/joejulian/xterm/blob/master/256colres.pl
;; https://github.com/joejulian/xterm/blob/master/256colres.h

;; scale rgb values from 0-255 down to 0-5, corresponding to colors in the xterm 6x6x6 RGB cube.
(defun rgb-to-rgb6 (rgb-list)
  "Take a list of three RGB integers 0-255, return a list of three RGB integers 0-5."
  (mapcar (lambda (x)
            (if (< x 55)
                0
                (floor (/ (- x 55) 40))))
          rgb-list))

;; scale rgb values from 0-5 to 0-255, from the xterm 6x6x6 RGB color cube to 24bit.
(defun rgb6-to-rgb (rgb6-list)
  "Take a list of three RGB integers 0-5, return a list of three RGB integers 0-255 of the xterm color palette."
  (mapcar (lambda (x) (if (> x 0)
                     (+ 55 (* 40 x))
                     0))
          rgb6-list))

(defun rgb6-to-sgr (rgb6-list)
  "Take a list of three RGB integers 0-5, return an 8bit SGR color code 16-231."
  (let ((r (nth 0 rgb6-list))
        (g (nth 1 rgb6-list))
        (b (nth 2 rgb6-list)))
    (+ (* r 36)
       (* g  6)
       (* b  1)
       16)))

;; only returns values from the color cube, not from the grayscale ramp 232-255.
(defun sgr-to-rgb6 (sgr)
  "Take a 8bit SGR color code 16-231, return a list of three RGB integers 0-5."
  (let* ((rgb (- sgr 16))
         (r (floor rgb 36))
         (r-rem (- rgb (* 36 r)))
         (g (floor r-rem 6))
         (b (- r-rem (* 6 g))))
    (list r g b)))

;; the rgb values of the first 8 ansi colors arent defined, they only have names.
;; the first 16 colors of the 256-color palette have names ("web colors") and rgb values.
;; https://en.wikipedia.org/wiki/Web_colors

;; TODO: this list is also defined in attr.lisp, but it should only be here
;;(defparameter *xterm-color-name-list*
;;  '(:black :maroon :green :olive  :navy :purple  :teal :silver
;;    :gray  :red    :lime  :yellow :blue :magenta :cyan :white))

(defparameter *xterm-color-hex-list*
  '(#x000000
    #x800000
    #x008000
    #x808000
    #x000080
    #x800080
    #x008080
    #xc0c0c0

    #x808080
    #xff0000
    #x00ff00
    #xffff00
    #x0000ff
    #xff00ff
    #x00ffff
    #xffffff))

(defun gray-to-rgb (sgr)
  "Take a sgr gray color number 232-255, return a list of three RGB integers 0-255."
  (let ((val (+ 8 (* 10 (- sgr 232)))))
    (list val val val)))

(defun closest-gray (rgb)
  "Take an integer 0-255 denoting a gray color intensity, return the closest gray from the xterm palette."
  (let* ((allowed-gray-values (loop for i from 0 to 23 collect (+ 8 (* 10 i))))
         (delta-list (mapcar (lambda (x) (abs (- rgb x))) allowed-gray-values))
         (delta-min (apply #'min delta-list))
         (pos (cl:position delta-min delta-list)))
    (+ 232 pos)))
    ;;(nth pos allowed-gray-values)))

;; otherwise return the closest short-rgb color.
(defun hex-to-sgr (hex)
  "Takes a RGB hex triplet, returns the exact or most appropriate SGR color code 0-255."
  (let ((rgb-list (hex-to-rgb hex)))
    ;; TODO: check whether we use 8 or 256 colors, limit the hex codes to the first 8 if necessary.
    (cond
      ;; is the hex value one of the 16 basic ansi colors?
      ((member hex *xterm-color-hex-list*)
       (cl:position hex *xterm-color-hex-list*))
      ;; if all three rgb values are equal, return the closest shade of gray.
      ((apply #'= rgb-list)
       (closest-gray (car rgb-list)))
      ;; if they arent equal, return the closest value from the 6x6x6 rgb cube.
      (t (rgb6-to-sgr (rgb-to-rgb6 rgb-list))))))

;; handles all three xterm-256color color spaces
;; we need this to list the rgb values of all 256 xterm colors to compare them to the x11 color list.
(defun sgr-to-hex (sgr)
  "Take a SGR color code 0-255, return a 24bit hex triplet."
  (cond
    ;; 8 ansi colors (8 normal and 8 bright or bold) 0-15
    ((< sgr 16)
     ;;(cdr (assoc sgr *ansi-color-sgr-hex-alist*)))
     (nth sgr *xterm-color-hex-list*))
    ;; 216 colors from a 6x6x6 RGB color cube, 16-231
    ((< sgr 232)
     (rgb-to-hex (rgb6-to-rgb (sgr-to-rgb6 sgr))))
    ;; 24 gray colors without black and white, which are contained in both 1. and 2.
    ((< sgr 256)
     (rgb-to-hex (gray-to-rgb sgr)))))
