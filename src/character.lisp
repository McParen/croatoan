(in-package :de.anvi.croatoan)

(defclass complex-char ()
  ((simple-char
    :initarg       :simple-char
    :initform      nil
    :type          (or null integer character keyword)
    :accessor      simple-char
    :documentation "Lisp primitive character type, like #\a.")

   (attributes
    :initarg       :attributes
    :initform      '()
    :type          (or null cons)
    :accessor      attributes
    :documentation "List of keywords denoting attributes.")

   (fgcolor
    :initarg       :fgcolor
    :initform      nil
    :type          (or null keyword integer list)
    :accessor      fgcolor
    :documentation "A keyword denoting the foreground color of the complex character.")

   (bgcolor
    :initarg       :bgcolor
    :initform      nil
    :type          (or null keyword integer list)
    :accessor      bgcolor
    :documentation "A keyword denoting the background color of the complex character."))

  (:documentation
   "A complex char consists of a simple char, a list of attribute keywords and a pair of color keywords."))

(defmethod initialize-instance :after ((xch complex-char) &key color-pair)
  "If color-pair is passed as a keyword, set fgcolor and bgcolor."
  (when color-pair
    (with-slots (fgcolor bgcolor) xch
      (setf fgcolor (car color-pair)
            bgcolor (cadr color-pair)))))

(defun set-equal (list-a list-b &key (test #'eq) (key #'identity))
  "Compare if every element of list-a is contained in list-b.

The order  of elements in lists does not matter, so the lists are actually treated as sets.

The items of the set are extracted using function 'key' and compared using predicate 'test'."
  (and (= (length list-a) (length list-b))
       (null (set-exclusive-or list-a list-b :test test :key key))))

(defun complex-char= (a b)
  "Return t if the simple-char, the color pair and the attribute list of a complex-char are equal.

The initial purpose of this function is to be used as the equality test for alexandria:define-constant."
  (with-accessors ((simple-char-a simple-char)
                   (bgcolor-a     bgcolor)
                   (fgcolor-a     fgcolor)
                   (attributes-a  attributes)) a
    (with-accessors ((simple-char-b simple-char)
                     (bgcolor-b     bgcolor)
                     (fgcolor-b     fgcolor)
                     (attributes-b  attributes)) b
      (let* ((simple-char-equals-p (cond
                                     ((null simple-char-a)
                                      (null simple-char-b))
                                     ((and (characterp simple-char-a)
                                           (characterp simple-char-b))
                                      (char= simple-char-a
                                             simple-char-b))
                                     ((and (integerp simple-char-a)
                                           (integerp simple-char-b))
                                      (= simple-char-a
                                         simple-char-b))
                                     (t ; mixed type or keyword
                                      (equalp simple-char-a
                                              simple-char-b)))))
        (and simple-char-equals-p
             (equalp fgcolor-a fgcolor-b)
             (equalp bgcolor-a bgcolor-b)
             (set-equal attributes-a attributes-b))))))

(defmethod make-load-form ((object complex-char) &optional environment)
  "Describe how complex-char objects can be serialized and loaded by the compiler."
  (make-load-form-saving-slots object
                               :slot-names '(simple-char attributes color-pair)
                               :environment environment))

;; TODO: do not use an array, use a simple list.
;; rename complex-char-array to chars, which is a list.
(defclass complex-string ()
  ((complex-char-array
    :initarg       :complex-char-array
    :initform      (make-array 0 :element-type 'complex-char :fill-pointer 0 :adjustable t)
    :type          vector
    :accessor      complex-char-array
    :documentation "Lisp primitive string type."))

  (:documentation
   "A complex string consists of an array of complex characters."))

;; TODO: what to do when there is no init string, when we start empty and build the string char by char,
;; for example when extracting a complex string.
;; we only need to use arrays when we realize that using a simple list is too slow.
(defmethod initialize-instance :after ((cstr complex-string) &key string attributes fgcolor bgcolor color-pair)
  (with-slots (complex-char-array) cstr
    (when string
      (loop for char across string
         do (vector-push-extend
             (make-instance 'complex-char :simple-char char :attributes attributes
                            :fgcolor fgcolor :bgcolor bgcolor :color-pair color-pair)
             complex-char-array)))))
