(in-package :de.anvi.croatoan)

(defparameter *ansi-color-list*
  '(:black :red :green :yellow :blue :magenta :cyan :white))

;; this list should be in color.lisp, not here.
;; but attr.lisp is loaded before color.lisp.
(defparameter *xterm-color-name-list*
  '(:black :maroon :green :olive  :navy :purple  :teal :silver
    :gray  :red    :lime  :yellow :blue :magenta :cyan :white))

;; do not use the color alist but the two color lists, for 8 and 256 colors
;; in the 256 colors mode, only the first 16 colors are named.
(defun color-name-to-number (color-name)
  "Take a keyword denoting a color name, return the color number."
  (if (eq color-name :default)
      ;; the terminal default colors are only available when
      ;; the screen is initialized with :use-default-colors t
      ;; the :default color number is -1.
      -1
      ;; depending on whether we use 8 or 256 colors, we have different names for the same colors.
      (let* ((color-list (if (<= %colors 8) *ansi-color-list* *xterm-color-name-list*))
             (position (cl:position color-name color-list)))
        (if position
            position
            (error "color name does not exist.")))))

;; address a color by:
;; integer: color number 0-255, so we can cycle through all available colors
;; keyword: it doesnt exist for all 256 colors
;; string: "#ff00ff" hex triplet in web-notation. string-length = 7, first char = #

;; a color can also be given as a list, where the first element is a keyword
;; denoting the coding scheme.

;; (:number 255)
;; (:name :black)
;; (:hex #xff00ff)
;; TODO: (:hex "#ff00ff")
;; TODO: (:rgb 255 00 255)
;; TODO: (:hsv 10 15 245)

;; TODO: naming conflict with color->number
;; after we can convert everything to a color number, add this function to
;; pair->number, so we can generate a color pair out of every color notation.
(defun color-to-number (color)
  "Takes a color in various notations, converts that notation to the exact or most appropriate color number."
  (typecase color
    ;; hex rgb code
    ;; we cant use hex codes when in 8-color ansi mode, because the 8 ansi colors
    ;; are only defined by names, not by rgb color contents.
    (integer (hex-to-sgr color))
    ;; color name, for now ONLY the first 8 ansi colors and 16 web colors
    ;; TODO: expand to all x11 color names
    (keyword (color-name-to-number color))
    ;; a list in the form of (:type value).
    (list
     (let ((type (car color))
           (val (cadr color)))
       (case type
         ;; direct input of the color number, just return it.
         (:number val)
         ;; keyword denoting the color name
         (:name (color-name-to-number color))
         (:hex
          (typecase val
            ;; hex rgb notation, for example (:hex #x00ff00)
            (integer (hex-to-sgr color)))))))))

;; keys are 2 element lists of the form: (:fg :bg)
;; fg and bg are keyword symbols
;; vals are integers that represent ncurses color pairs.
;; only one color pair, 0, is predefined: (:default :default),
;; which is identical to (:white :black) if use-default-colors is nil.
;; if use-default-colors is t, it is whatever color pair the terminal
;; used before the ncurses init.
;; TODO: this also could be a hashmap
;; TODO: move this to be a screen variable, so it gets reset on every screen init
;; for now it is reset to nil on evey screen init by set-default-color-pair
(defparameter *color-pair-alist* nil)

;; called from initialize-instance :after ((scr screen)
;; test with t09a
(defun set-default-color-pair (use-default-colors)
  ;; reset the color pair alist on every screen init
  (setf *color-pair-alist* nil)
  (if use-default-colors
      (progn
        (%use-default-colors)
        (setf *color-pair-alist* (acons '(:default :default) 0 *color-pair-alist*)))
      (setf *color-pair-alist* (acons '(:white :black) 0 *color-pair-alist*))))

;; TODO: later rename to pair-to-number
(defun pair->number (pair)
  "Take a two-element list of colors, return the ncurses pair number.

The colors can be keywords or numbers -1:255.

-1 is the terminal :default color when use-default-colors is t.

If it is a new color pair, add it to ncurses, then return the new pair number.

If the pair already exists, return its pair number.

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
          (%init-pair new-pair-number (color-to-number fg) (color-to-number bg)))
        ;; return the newly added pair number.
        new-pair-number))))

;; TODO: cross check with the ncurses primitives that we get the same result.
;; TODO: number-to-pair
(defun number->pair (number)
  "Take a pair number, return a color pair in a 2 element list of keywords."
  (car (rassoc number *color-pair-alist*)))

;; TODO: use %wattr_on instead of %wattron, also for get and set
(defun add-attributes (win attributes)
  "Takes a list of keywords and turns the appropriate attributes on."
  (dolist (i attributes)
    (setf (attributes win) (adjoin i (attributes win)))
    (%wattron (winptr win) (get-bitmask i))))

(defun remove-attributes (win attributes)
  "Takes a list of keywords and turns the appropriate attributes off."
  (dolist (i attributes)
    (setf (attributes win) (remove i (attributes win)))
    (%wattroff (winptr win) (get-bitmask i))))

;; (set-attributes scr '(:bold :underline))
;; set-attributes overwrites color settings because it treats color as an attribute.
;; thats why we wont use it directly.
(defun set-attributes (winptr attributes)
  "Takes a list of keywords and sets the appropriate attributes.

Overwrites any previous attribute settings including the color."
  (%wattrset winptr
             (apply #'logior (loop for i in attributes collect (get-bitmask i)))))

;; (%wchgat (winptr win) 9 #x00040000 0 (null-pointer))
(defun change-attributes (win n attributes &key color-pair y x position)
  "Change the attributes of n chars starting at the current cursor position.

If n is -1, as many chars will be added as will fit on the line.

If the destination coordinates y and x are given, the attributes are changed
from the given point without moving the cursor position."
  (when (and y x) (move win y x))
  (when position (apply #'move win position))
  (let ((attrs (attrs2chtype attributes))
        (pairno (if color-pair
                    (pair->number color-pair)
                    (if (color-pair win)
                        (pair->number (color-pair win))
                        0))))
    (%wchgat (winptr win) n attrs pairno (null-pointer))))

;; (set-color window '(:black :white))
(defun set-color-pair (winptr color-pair)
  "Sets the color attribute only."
  (if color-pair
      (%wcolor-set winptr (pair->number color-pair) (null-pointer))
      ;; if color-pair is nil, set the color pair 0 (white black) or (default default).
      (%wcolor-set winptr 0 (null-pointer))))

(defparameter *bitmask-alist*
  ;; the first four are not attributes, but bitmasks used to extract parts of the chtype.
  '((:normal     . #x00000000)
    (:attributes . #xffffff00)
    (:chartext   . #x000000ff)
    (:color      . #x0000ff00)
    ;; we have 16 attributes that can be set.
    ;; In general, only underline, bold and reverse are widely supported by terminals.
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
    (:vertical   . #x40000000)
    (:italic     . #x80000000)))

;; TODO: signal an error if passed an invalid attribute.
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
    :vertical
    :italic))

(defun chtype2attrs (ch)
  "Take a chtype, return a list of used attribute keywords."
  (loop
     for i in *valid-attributes*
     if (logtest ch (get-bitmask i)) collect i))

;; used in: make-chtype, change-attributes
(defun attrs2chtype (attrs)
  "Take a list of attribute keywords, return a chtype with the attribute bits set."
  (if attrs
      ;; the attribute bitmasks already are bit-shifted to the correct position in the chtype
      ;; we just need to OR them all together
      (apply #'logior (mapcar #'get-bitmask attrs))
      ;; if the attribute list is nil, logior returns 0.
      ;; but to emphasize intent, we explicitely return 0 if the attribute list is nil.
      0))

(defun colors2chtype (color-pair)
  "Take a list of a color pair, return a chtype with the color attribute set."
  (if color-pair
      ;; convert the pair to an integer, then bit shift it by 8
      ;; right shift by 8 to get the color bits at their proper place in a chtype.
      ;; you cannot simply logior the pair number because that would overwrite the char.
      (ash (pair->number color-pair) 8)
      0))

;; usage: c2x, extract wide char, everywhere where number->pair is used.
;; first get the color attribute bits by log-AND-ing them with the ch.
;; then right shift them by 8 to extract the color pair short int from them.
;; then get the color pair (:white :black) associated with that number.
(defun chtype2colors (ch)
  "Take a chtype or attr_t integer, return a list of two keywords denoting a color pair."
  (number->pair (ash (logand ch (get-bitmask :color)) -8)))

(defun char2chtype (char)
  "Take a character in different forms, return a chtype containing that character."
  (if char
      (typecase char
        ;; if the char is already an integer from char-code.
        (integer char)
        ;; alternative chars are given as keywords
        ;; we use acs only when we produce chtypes, for cchar_t, we need wacs.
        (keyword (acs char))
        ;; if it is a lisp char, convert it to an integer first
        (character (char-code char))
        ;; if char is any other type, we dont handle it for now.
        (otherwise (error "char2chtype: char is not integer, keyword or character.")))
      0))

;;; ------------------------------------------------------------------

(defun make-chtype (char attributes color-pair)
  "Assemble a chtype out of a char, attributes and a color-pair.

char can be a lisp character, an ACS keyword, or an integer code point.

attributes should be a list of valid attribute keywords.

color-pair should be a list of a foreground and background color keyword.

attributes and color-pair can be nil.

If char is a complex char, and the attributes and color-pair are passed,
they override the attributes and the color-of the complex char."
  (typecase char
    ;; x2c itself calls make-chtype 
    (complex-char (xchar2chtype char))
    ;; we first convert all three parameters to separate integers,
    ;; then OR them together to create the chtype.
    (otherwise    (logior (char2chtype char)
                          (attrs2chtype attributes)
                          (colors2chtype color-pair)))))

;; we do not really need this function, but it is provided to parallel funcall-with-cchar_t.
;; TODO: it has to accept complex-chars because funcall-make-cchar_t also does.
(defun funcall-make-chtype (fn window char attributes color-pair count)
  "Make a chtype and apply low-level ncurses function fn count times to window and chtype.

chtype is a 32-bit integer representing a non-wide complex-char in ncurses."
  (let ((chtype (make-chtype char attributes color-pair))
        (winptr (winptr window)))
    (if (= count 1)
        (funcall fn winptr chtype)
        (dotimes (i count) (funcall fn winptr chtype)))))

;; Example: (xchar2chtype (chtype2xchar 2490466)) => 2490466

(defun xchar2chtype (ch)
  "Convert a croatoan complex char to an integral ncurses chtype."
  (make-chtype (simple-char ch)
               (attributes ch)
               (color-pair ch)))

(defun chtype2xchar (ch)
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
    (:simple-char (simple-char char))
    (:chtype (xchar2chtype char))))

;; Lisps character object is here called "simple-char".
(defmethod convert-char ((char character) result-type)
  (case result-type
    (:complex-char (make-instance 'complex-char :simple-char char :attributes nil))
    (:chtype (char-code char))))

;; chtype is a ncurses unsigned long, an integer.
(defmethod convert-char ((char integer) result-type)
  (case result-type
    (:simple-char (code-char (logand char (get-bitmask :chartext))))
    (:complex-char (chtype2xchar char))))


;;; TODOs

;; todo: convert-char -> convert

;; [ ] add type asserts.
;; what is an attr_t? get all ncurses types definitions.

;; make it clear which routines use xchars and which use chtypes.
;; make all user visible apis use xchars and only internally convert to chtypes.
;; functions to manipulate attributes and colors of xchars.
;; the char part of an xchar should not be changeable.
