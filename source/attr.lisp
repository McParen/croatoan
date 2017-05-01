(in-package :de.anvi.croatoan)

(defparameter *ansi-color-list*
  (list :black :red :green :yellow :blue :magenta :cyan :white))

(defparameter *color-alist*
  '((:default . -1)
    (:black   . 0)
    (:red     . 1)
    (:green   . 2)
    (:yellow  . 3)
    (:blue    . 4)
    (:magenta . 5)
    (:cyan    . 6)
    (:white   . 7)))

(defun color->number (color-name)
  "Take a keyword, returns the corresponding color number.

Example: (color->number :white) => 7"
  (let ((pair (assoc color-name *color-alist*)))
    (if pair
        (cdr pair)
      (error "color doesnt exist."))))

;; Do not signal an error, because not all color numbers have names.
(defun number->color (number)
  "Take a color number, return a color keyword if the color name exists, nil otherwise."
  (car (rassoc number *color-alist*)))

;; keys are 2 element lists of the form: (:fg :bg)
;; fg and bg are keyword symbols
;; vals are integers that represent ncurses color pairs.
;; only one color pair, 0, is predefined: (:default :default),
;; which is identical to (:white :black) if use-default-colors is nil.
;; if use-default-colors is t, it is whatever color pair the terminal
;; used before the ncurses init.
(defparameter *color-pair-alist* nil)

;; called from initialize-instance :after ((scr screen)
;; test with t09a
(defun set-default-color-pair (use-default-colors)
  (if use-default-colors
      (progn
        (%use-default-colors)
        (setf *color-pair-alist* (acons '(:default :default) 0 *color-pair-alist*)))
      (setf *color-pair-alist* (acons '(:white :black) 0 *color-pair-alist*))))

(defun pair->number (pair)
  "Take a 2 element list of colors, return the pair number.

The colors can be keywords or numbers -1:255.

If it is a new pair, add them to ncurses, return the new pair number.

If the pair already exists, return the pair number.

Example: (pair->number '(:white :black)) => 0"
  (let ((result (assoc pair *color-pair-alist* :test #'equal)))
    (if result
        ;; if the entry already exists, just return the pair number.
        (cdr result)
      ;; if the pair doesnt exist, create a new pair number
      (let ((new-pair-number (list-length *color-pair-alist*)))
        ;; add it to the alist first.
        (setf *color-pair-alist* (acons pair new-pair-number *color-pair-alist*))
        ;; then add it to ncurses.
        (let ((fg (car pair))
              (bg (cadr pair)))
          (%init-pair new-pair-number
                      (if (numberp fg) fg (color->number fg))
                      (if (numberp bg) bg (color->number bg))))
        ;; return the newly added pair number.
        new-pair-number))))

;; TODO: cross check with the ncurses primitives that we get the same result.
(defun number->pair (number)
  "Take a pair number, return a color pair in a 2 element list of keywords."
  (car (rassoc number *color-pair-alist*)))

(defun add-attributes (win attributes)
  "Takes a list of keywords and turns the appropriate attributes on."
  (dolist (i attributes)
    (setf (.attributes win) (adjoin i (.attributes win)))
    (%wattron (.winptr win) (get-bitmask i))))

(defun remove-attributes (win attributes)
  "Takes a list of keywords and turns the appropriate attributes off."
  (dolist (i attributes)
    (setf (.attributes win) (remove i (.attributes win)))
    (%wattroff (.winptr win) (get-bitmask i))))

;; (set-attributes scr '(:bold :underline))
;; set-attributes overwrites color settings because it treats color as an attribute.
;; thats why we wont use it directly.
(defun set-attributes (winptr attributes)
  "Takes a list of keywords and sets the appropriate attributes.

Overwrites any previous attribute settings including the color."
  (%wattrset winptr
             (apply #'logior (loop for i in attributes collect (get-bitmask i)))))

;; (%wchgat (.winptr win) 9 #x00040000 0 (null-pointer))
(defun change-attributes (win n attributes &key color-pair y x)
  "Change the attributes of n chars starting at the current cursor position.

If n is -1, as many chars will be added as will fit on the line.

If the destination coordinates y and x are given, the attributes are changed
from the given point without moving the cursor position."
  (let ((attrs (attrs2chtype attributes))
        (pairno (if color-pair
                    (pair->number color-pair)
                    (if (.color-pair win)
                        (.color-pair win)
                        0))))
    (if (and y x)
        (%mvwchgat (.winptr win) y x n attrs pairno (null-pointer))
        (%wchgat (.winptr win) n attrs pairno (null-pointer)))))

;; (set-color window '(:black :white))
(defun set-color-pair (winptr color-pair)
  "Sets the color attribute only."
  (%wcolor-set winptr (pair->number color-pair) (null-pointer)))

;;In general, only underline, bold and reverse work.
(defparameter *bitmask-alist*
  '((:normal     . #x00000000)
    (:attributes . #xffffff00)
    (:chartext   . #x000000ff)
    (:color      . #x0000ff00)
    (:standout   . #x00010000)
    (:underline  . #x00020000)
    (:reverse    . #x00040000)
    (:blink      . #x00080000)
    (:dim        . #x00100000)
    (:bold       . #x00200000)
    (:altcharset . #x00400000)
    (:invis      . #x00800000)
    (:protect    . #x01000000)
    (:horizontal . #x02000000)
    (:left       . #x04000000)
    (:low        . #x08000000)
    (:right      . #x10000000)
    (:top        . #x20000000)
    (:vertical   . #x40000000)))

(defun get-bitmask (attribute)
  "Returns an ncurses attr/chtype representing the attribute keyword."
  (cdr (assoc attribute *bitmask-alist*)))

(defparameter *valid-attributes*
  '(:standout
    :underline 
    :reverse 
    :blink 
    :dim 
    :bold 
    :altcharset 
    :invis 
    :protect 
    :horizontal 
    :left 
    :low 
    :right 
    :top 
    :vertical))

(defparameter *used-attributes*
  '(:underline 
    :reverse 
    :blink 
    :dim 
    :bold))

(defun chtype2attrs (ch)
  "Take a chtype, return a list of used attribute keywords."
  (loop
     for i in *used-attributes*
     if (logtest ch (get-bitmask i)) collect i))

;; used in: char2chtype, change-attributes
(defun attrs2chtype (attrs)
  "Take a list of attribute keywords, return a chtype with the attribute bits set."
  (apply #'logior (mapcar #'get-bitmask attrs)))

;; usage: c2x, extract wide char, everywhere where number->pair is used.
;; first get the color attribute bits by log-AND-ing them with the ch.
;; then right shift them by 8 to extract the color pair short int from them.
;; then get the color pair (:white :black) associated with that number.
(defun chtype2colors (ch)
  "Take a chtype or attr_t integer, return a list of two keywords denoting a color pair."
  (number->pair (ash (logand ch (get-bitmask :color)) -8)))

;;; ------------------------------------------------------------------

(defun char2chtype (char attributes color-pair)
  "Convert a char (lisp char, char code or ACS symbol) to a chtype.

attributes should be a list of attribute keywords.

color-pair should be a list of a foreground and background color keyword."
  (let ((ch (typecase char
              ;; if the char is already an integer from char-code.
              (integer char)
              ;; alternative chars are given as keywords
              (keyword (acs char))
              ;; if it is a lisp char, convert it to an integer first
              (character (char-code char))
              (t 0)))
        ;; convert the pair to an integer, then bit shift it by 8
        (col (if color-pair (ash (pair->number color-pair) 8) 0))
        ;; the attribute bitmasks already are bit-shifted to the
        ;; correct position in the chtype
        (attr (if attributes (attrs2chtype attributes) 0)))
    ;; the 3 integers are now OR-ed together to create the chtype.
    (logior ch attr col)))

;; Example: (x2c (c2x 2490466)) => 2490466

(defun x2c (ch)
  "Converts a croatoan complex char to a ncurses chtype."
  (logior (let ((char (.simple-char ch)))
            (if char
                (typecase char
                  (character (char-code (.simple-char ch)))
                  (integer char)
                  (keyword (acs char))
                  (t 0))
                0)) ; logioring something with 0 has no effect.
          
          ;; if an attribute is there, add its integer or a 0.
          ;; TODO: abstract away the c&p orgy, z.B in a local macro.
          (if (member :underline  (.attributes ch)) (get-bitmask :underline)  0)
          (if (member :reverse    (.attributes ch)) (get-bitmask :reverse)    0)
          (if (member :blink      (.attributes ch)) (get-bitmask :blink)      0)
          (if (member :dim        (.attributes ch)) (get-bitmask :dim)        0)
          (if (member :bold       (.attributes ch)) (get-bitmask :bold)       0)
          (if (member :altcharset (.attributes ch)) (get-bitmask :altcharset) 0)
          (if (member :invis      (.attributes ch)) (get-bitmask :invis)      0)
          (if (member :protect    (.attributes ch)) (get-bitmask :protect)    0)
          (if (member :horizontal (.attributes ch)) (get-bitmask :horizontal) 0)
          (if (member :left       (.attributes ch)) (get-bitmask :left)       0)
          (if (member :low        (.attributes ch)) (get-bitmask :low)        0)
          (if (member :right      (.attributes ch)) (get-bitmask :right)      0)
          (if (member :top        (.attributes ch)) (get-bitmask :top)        0)
          (if (member :vertical   (.attributes ch)) (get-bitmask :vertical)   0)

          ;; right shift by 8 to get the color bits at their proper place in a chtype.
          ;; you cannot simply logior the pair number because that would overwrite the char.
          (if (.color-pair ch)
              (ash (pair->number (.color-pair ch)) 8)
              0)))

(defun c2x (ch)
  "Converts a ncurses chtype to croatoan complex-char."
  (make-instance 'complex-char
                 :simple-char (code-char (logand ch (get-bitmask :chartext)))
                 :attributes (loop for i in *valid-attributes*
                                   if (logtest ch (get-bitmask i)) collect i)
                 ;; first get the color attribute bits by log-AND-ing them with ch.
                 ;; then right shift them by 8 to extract the color int from them.
                 ;; then get the color pair (:white :black) associated with that number.
                 :color-pair (number->pair (ash (logand ch (get-bitmask :color)) -8))))


(defgeneric convert-char (char result-type)
  (:documentation "Take a char and convert it to a char of result-type."))

;; The lisp class representing chtype is complex-char.
(defmethod convert-char ((char complex-char) result-type)
  (case result-type
    (:simple-char (.simple-char char))
    (:chtype (x2c char))))

;; Lisps character object is here called "simple-char".
(defmethod convert-char ((char character) result-type)
  (case result-type
    (:complex-char (make-instance 'complex-char :simple-char char :attributes nil))
    (:chtype (char-code char))))

;; chtype is a ncurses unsigned long, an integer.
(defmethod convert-char ((char integer) result-type)
  (case result-type
    (:simple-char (code-char (logand char (get-bitmask :chartext))))
    (:complex-char (c2x char))))


;;; TODOs

;; todo: convert-char -> convert

;; [ ] add type asserts.
;; what is an attr_t? get all ncurses types definitions.

;; make it clear which routines use xchars and which use chtypes.
;; make all user visible apis use xchars and only internally convert to chtypes.
;; functions to manipulate attributes and colors of xchars.
;; the char part of an xchar should not be changeable.
