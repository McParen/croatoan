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
    :initform      (make-array 0
                               :element-type 'complex-char
                               :fill-pointer 0
                               :adjustable   t)
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

;;;; Some handy functions to manipulate complex-strings

(defun make-background (color-bg &key (color-fg nil) (char #\Space))
  "Makes an object suitable as background for a window using `color-bg' as background color,
`color-fg' as  foreground color (default to  `color-bg') and character
`char'."
  (make-instance 'complex-char
                 :simple-char char
                 :color-pair  (if color-fg
                                  (list color-fg color-bg)
                                  (list color-bg color-bg))))

(defmacro complex-string-format ((control-string &rest args)
                      &key
                        (attributes nil)
                        (fgcolor    nil)
                        (bgcolor    nil))
  "Use `control-string' to  build a `complex-string' with  the help of
`format' function"
  `(make-instance 'complex-string
                  :string      (apply #'format nil ,control-string ,@args)
                  :attributes  ,attributes
                  :fgcolor     ,fgcolor
                  :bgcolor     ,bgcolor))

(defun complex-string-length (complex-string)
  "Returns the length (in characters  units) of a complex string passed
as argument `complex-string'."
  (length (complex-char-array complex-string)))

(defgeneric text-width (object)
  (:documentation "Returns the length (in characters  units) of a complex string passed
as argument `complex-string'."))

(defgeneric text-slice (object start &optional end)
  (:documentation  "Returns a sub sequence of `object' starting from
  `start` and terminating at `end'. If end in nil the the sub sequence ends alt the last element of the sequence"))

(defmethod text-width ((object string))
  (length object))

(defmethod text-width ((object complex-string))
  (complex-string-length object))

(defmethod text-slice ((object string) start &optional (end nil))
  (subseq object start end))

(defun array-slice (array start &optional (end nil))
  (let* ((new-size         (if end
                               (- end start)
                               (length array)))
         (new-fill-pointer (cond
                             ((array-has-fill-pointer-p array)
                              (if end
                                  new-size
                                  (fill-pointer array)))
                             (t
                              nil)))
         (new-array        (make-array new-size
                                       :element-type    (array-element-type array)
                                       :fill-pointer    new-fill-pointer
                                       :initial-element (elt array 0)
                                       :adjustable      (adjustable-array-p array)))
         (end-iteration    (or end
                               (length array))))
    (loop
       for index-from from start below end-iteration
       for index-to   from 0
           do
         (setf (elt new-array index-to)
               (elt array     index-from)))
    new-array))

(defmethod text-slice ((object complex-string) start &optional (end nil))
  (let ((res (copy-complex-string object)))
    (setf (complex-char-array res)
          (array-slice (complex-char-array object) start end))
    res))

(defun copy-complex-char (complex-char)
  (let ((res (make-instance 'complex-char)))
    (flet ((%copy (thing)
             (if (listp thing)
                 (copy-list thing)
                 thing)))
      (setf (simple-char res) (simple-char complex-char)
            (attributes  res) (copy-list (attributes complex-char))
            (fgcolor     res) (%copy (fgcolor complex-char))
            (bgcolor     res) (%copy (bgcolor complex-char)))
      res)))

(defmethod copy-complex-string (complex-string)
  (let ((res (make-instance 'complex-string)))
    (with-accessors ((char-array-to complex-char-array)) res
      (setf char-array-to
            (make-array (length (complex-char-array complex-string))
                        :initial-element (make-instance 'complex-char)
                        :element-type 'complex-char
                        :fill-pointer (length (complex-char-array complex-string))
                        :adjustable   t))
      (loop
         for xch across (complex-char-array complex-string)
         for i from 0
         do
           (setf (elt char-array-to i)
                 (copy-complex-char xch)))
      res)))

(defun nconcat-complex-string (a b)
  "Destructively concatenate the `complex-string' `a' and `b'"
  (with-accessors ((inner-array-a complex-char-array)) a
    (with-accessors ((inner-array-b complex-char-array)) b
      (setf inner-array-a
            (concatenate 'vector inner-array-a inner-array-b)))))

(defgeneric concat-complex-string (a b &key color-attributes-contagion)
  (:documentation "Return  a new `complex-string' that  is the results
  of concatenating `a' and 'b'. If `color-attributes-contagion' is non
  nil `b' will inherit all the attributes and color of the last element of `a'."))

(defun copy-complex-char-array (a)
  "Make a (non deep) copy of `complex-char' array `a'"
  (let ((res (make-array (length a)
                         :element-type    'complex-char
                         :fill-pointer    (length a)
                         :adjustable      t
                         :initial-element (make-instance 'complex-char))))
    (loop
       for i from 0 below (length a)
       for char across a
       do
         (setf (elt res i) char))
    res))

(defun concat-complex-string-no-contagion (a b)
  "Concatenate two  `complex-strings': the args `b' does not inherit
the color and attributes of `a'."
  (with-accessors ((inner-array-a complex-char-array)) a
    (let* ((res (make-instance 'complex-string
                               :complex-char-array (copy-complex-char-array inner-array-a))))
      (with-accessors ((inner-array-res complex-char-array)) res
        (map nil
             (lambda (a)
               (vector-push-extend (make-instance 'complex-char
                                                  :simple-char a)
                                   inner-array-res))
             b))
      res)))

(defun vector-not-empty-p (v)
  (/= (length v) 0))

(defun complex-string-last-char-attributes (complex-string)
  "Returns the attributes of the last character of `complex-string' as three values:
attributes, background color and foreground color"
  (flet ((last-element (a)
           (elt a (1- (length a)))))
    (with-accessors ((inner-array-a complex-char-array)) complex-string
      (let* ((last-complex-char    (and (vector-not-empty-p inner-array-a)
                                        (last-element inner-array-a)))
             (last-char-attributes (and last-complex-char
                                        (attributes last-complex-char)))
             (last-char-fg         (and last-complex-char
                                        (fgcolor last-complex-char)))
             (last-char-bg         (and last-complex-char
                                        (bgcolor last-complex-char))))
        (values last-char-attributes
                last-char-bg
                last-char-fg)))))

(defun complex-string-first-char-attributes (complex-string)
  "Returns the attributes of the first character of `complex-string' as three values:
attributes, background color and foreground color."
  (flet ((first-element (a)
           (elt a 0)))
    (with-accessors ((inner-array-a complex-char-array)) complex-string
      (let* ((first-complex-char    (and (vector-not-empty-p inner-array-a)
                                         (first-element inner-array-a)))
             (first-char-attributes (and first-complex-char
                                         (attributes first-complex-char)))
             (first-char-fg         (and first-complex-char
                                         (fgcolor first-complex-char)))
             (first-char-bg         (and first-complex-char
                                         (bgcolor first-complex-char))))
        (values first-char-attributes
                first-char-bg
                first-char-fg)))))

(defun concat-complex-string-with-contagion (string-1 string-2)
  "Concatenate two  `complex-strings': the args `string-2' does not inherit
the color and attributes of `string-1'."
  (with-accessors ((inner-array-a complex-char-array)) string-1
    (let* ((res (make-instance 'complex-string
                               :complex-char-array (copy-complex-char-array inner-array-a))))
      (with-accessors ((inner-array-res complex-char-array)) res
        (multiple-value-bind (new-attributes new-bg new-fg)
            (complex-string-last-char-attributes string-1)
          (map nil
               (lambda (a)
                 (let ((new-char (make-instance 'complex-char
                                                :simple-char (simple-char a)
                                                :attributes  new-attributes
                                                :fgcolor     new-fg
                                                :bgcolor     new-bg)))
                   (vector-push-extend new-char (complex-char-array res))))
               (complex-char-array string-2))))
      res)))

(defmethod concat-complex-string ((a complex-string) (b sequence)
                                  &key (color-attributes-contagion t))
  "Return a  complex string  that is the  results of  concatenating of
`a'    (a `complex-string') and `b' (a string). If
`color-attributes-contagion' is non nil `b' will inherit all the attributes and
color of the last element of `a'."
  (if (not color-attributes-contagion)
      (concat-complex-string-no-contagion a b)
      (with-accessors ((inner-array-a complex-char-array)) a
        (multiple-value-bind (new-attributes new-bg new-fg)
            (complex-string-last-char-attributes a)
          (let* ((res (make-instance 'complex-string
                                     :complex-char-array
                                     (copy-complex-char-array inner-array-a))))
            (with-accessors ((inner-array-res complex-char-array)) res
              (map nil
                   (lambda (a)
                     (vector-push-extend (make-instance 'complex-char
                                                        :bgcolor     new-bg
                                                        :fgcolor     new-fg
                                                        :attributes  new-attributes
                                                        :simple-char a)
                                         inner-array-res))
                   b))
            res)))))

(defmethod concat-complex-string ((a sequence) (b complex-string)
                                  &key (color-attributes-contagion t))
  "Return a  complex string  that is the  results of  concatenating of
`a'    (a `complex-string') and `b' (a string). If
`color-attributes-contagion' is non nil `b' will inherit all the attributes and
color of the last element of `a'."
  (with-accessors ((inner-array-b complex-char-array)) b
    (multiple-value-bind (new-attributes new-bg new-fg)
        (complex-string-first-char-attributes b)
      (let* ((res-vector-length (+ (length a)
                                   (length inner-array-b)))
             (res               (make-instance 'complex-string
                                               :complex-char-array
                                               (make-array res-vector-length
                                                           :element-type 'complex-char
                                                           :fill-pointer res-vector-length
                                                           :adjustable      t
                                                           :initial-element
                                                           (make-instance 'complex-char)))))
        (with-accessors ((inner-array-res complex-char-array)) res
          (loop for i from 0 below (length a) do
               (let ((new-complex-char (if (not color-attributes-contagion)
                                           (make-instance 'complex-char
                                                          :simple-char (elt a i))
                                           (make-instance 'complex-char
                                                          :bgcolor     new-bg
                                                          :fgcolor     new-fg
                                                          :attributes  new-attributes
                                                          :simple-char (elt a i)))))
                 (setf (elt inner-array-res i) new-complex-char)))
          (loop for i from 0 below (length inner-array-b) do
               (setf (elt inner-array-res (+ i (length a))) (elt inner-array-b i)))
          res)))))

(defmethod concat-complex-string ((a complex-string) (b complex-string)
                                  &key (color-attributes-contagion nil))
  "Return a complex string that is the results of concatenating of `a'
  and  `b': two  `complex-string'. If  `color-attributes-contagion' is
  non nil `b' will inherit all the attributes and color of the last element of `a'."
  (if color-attributes-contagion
      (concat-complex-string-with-contagion a b)
      (with-accessors ((inner-array-a complex-char-array)) a
        (with-accessors ((inner-array-b complex-char-array)) b
          (let ((res (make-instance 'complex-string
                                    :complex-char-array (copy-complex-char-array inner-array-a))))
            (with-accessors ((inner-array-res complex-char-array)) res
              (loop for i across  inner-array-b do
                   (vector-push-extend i inner-array-res))
              res))))))

(defun complex-string->chars-string (complex-string)
  "Convert a `complex-string' to a `string'."
  (with-accessors ((complex-char-array complex-char-array)) complex-string
    (let ((res (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t)))
      (with-output-to-string (stream res)
        (loop for i across complex-char-array do
             (format stream "~a" (simple-char i)))
        res))))

(defgeneric text-ellipsize (object len &key truncate-string)
  (:documentation "If `object''s length is bigger  than `len', cut the last characters
  out.  Also replaces the last n  characters (where n is the length of
  `truncate-string')     of     the      shortened     string     with
  `truncate-string'. It  defaults to  \"...\", but can  be nil  or the
  empty string."))

(defmethod text-ellipsize ((object complex-string) len &key (truncate-string "..."))
  (let ((string-len (text-width object)))
    (cond
      ((<= string-len len)
       object)
      ((< len
          (text-width truncate-string))
       (text-slice object 0 len))
      (t
       (concat-complex-string (text-slice object 0 (- len (text-width truncate-string)))
                              truncate-string)))))

(defgeneric text-right-pad (object total-size &key padding-char)
  (:documentation "Prepend a number of copies of `padding-char' to `object' so that the
latter has a length equals to `total-size'"))

(defun right-padding (str total-size &key (padding-char #\Space))
  (concatenate 'string
               str
               (make-string (max 0 (- total-size (length str)))
                            :initial-element padding-char)))

(defmethod text-right-pad ((object string) (total-size number) &key (padding-char #\Space))
  (assert (> total-size 0))
  (right-padding object total-size :padding-char padding-char))

(defmethod text-right-pad ((object complex-string) (total-size number)
                               &key (padding-char #\Space))
  (assert (> total-size 0))
  (let ((suffix (make-string (max 0 (- total-size (text-width object)))
                             :initial-element padding-char)))
    (concat-complex-string object suffix)))
