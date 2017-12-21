(in-package :de.anvi.croatoan)

#|

classes and methods for the gray stream interface.

http://www.nhplace.com/kent/CL/Issues/stream-definition-by-user.html
http://www.gnu.org/software/clisp/impnotes/gray-gf-char-out.html

before including this, add trivial-gray-streams to asd or use the sb-gray stream package.

Up to now, window and screen had no superclasses, thus they were subclasses of standard-object.
Now, they will become bi-directional character streams.

That means that we dont need separate classes for defining streams, and that windows will have
a stream as a feature, windows now will _be_ specialized streams.

fundamental-character-output-stream
fundamental-character-input-stream
  window
    screen
    subwin

For the existing code, nothing will change. we will still be able to use all ncurses and croatoan
functions.

Actually, we still _need_ those functions to define the gray stream functions. But once defined,
we will not need add-char and add-string any more, we will simply use Lisp's format, read, print, etc.

|#

;;;; libncursesw, wide IO

;;; Character Output stream

;;; Mandatory methods

;; write-char, format ~C
(defmethod stream-write-char ((stream window) (ch character))
  (if (.insert-enabled stream)
      (progn
        (insert-wide-char stream ch)
        ;; move the cursor after the inserted character.
        (move-to stream :right))
      (add-wide-char stream ch)))

;; 170830: #sbcl, according to stassats we can not specialize on the second argument,
;; so no complex chars or complex strings with the ~C directive.
;; use ~/xyz/ instead of ~C.
;; ~A uses print-object underneath, not gray streams, so it probably can be used.
#|
; SLIME 2.19
CL-USER> (defun xxx (&rest args)
             (print (second args)))
XXX
CL-USER> xxx
; Evaluation aborted on #<UNBOUND-VARIABLE XXX {1002E0A433}>.
CL-USER> (xxx 1 2 3 4)

2 
2
CL-USER> (format t "~/xxx/")
; Evaluation aborted on #<SB-FORMAT:FORMAT-ERROR {1003469493}>.
CL-USER> (format t "~/xxx/" 1)

1 
NIL
CL-USER> (format t "~/xxx/" 11)

11 
NIL
CL-USER> 
|#
(defmethod stream-write-char ((stream window) (ch complex-char))
  (add-wide-char stream ch))

;; Returns the column number where the next character would be written, i.e. the current x position
(defmethod stream-line-column ((stream window))
  (%getcurx (.winptr stream)))

;;; Non-mandatory methods

;; Default method uses repeated calls to stream-write-char
;; We can not specialize stream-write-string on complex-strings.

#|
(defmethod stream-write-string ((stream window) (str-orig string) &optional (start 0) (end nil))
  ;; TODO: either do something with start and end, or (declare (ignore start end))
  (let ((str (subseq str-orig start end)))
    ;; TODO: we can not combine %wadd-wch and %waddstr
    ;; TODO: writing a normal string waddstr on a wide cchar background causes an SB-KERNEL::CONTROL-STACK-EXHAUSTED-ERROR
    (%waddstr (.winptr stream) str)))
|#

;;; Character Input Stream

;;; Mandatory methods: stream-read-char, stream-unread-char

(defmethod stream-read-char ((stream window))
  (code-char (get-wide-char stream)))

(defmethod stream-unread-char ((stream window) (ch character))
  (%unget-wch (char-code ch)))

;;;; libncurses, non-wide IO 

;;; Character Output stream

;;; Mandatory methods: stream-write-char, stream-line-column

;; write-char, format ~C
;;(defmethod stream-write-char ((stream window) (ch character))
;;  (let ((code (char-code ch))
;;        (winptr (.winptr stream)))
;;    (if (.insert-enabled stream)
;;        (progn
;;          (%winsch winptr code)
;;          ;; move the cursor after the inserted character.
;;          (move-to stream :right))
;;        (%waddch winptr code))))

;; write-char, format ~C
;;(defmethod stream-write-char ((stream window) (ch complex-char))
;;  (%waddch (.winptr stream) (x2c ch)))

;; print, prin1, princ, format ~A, ~S
;;(defmethod print-object ((ch complex-char) (stream window))
;;  (%waddch (.winptr stream) (x2c ch)))
;;
;;(defmethod print-object ((cstr complex-string) stream)
;;  (loop for ch across (.complex-char-array cstr)
;;     do (princ (.simple-char ch))))
;;
;;(defmethod print-object ((cstr complex-string) (stream window))
;;  (loop for ch across (.complex-char-array cstr)
;;     do (add-char stream ch)))

;; Returns the column number where the next character would be written, i.e. the current y position
;;(defmethod stream-line-column ((stream window))
;;  (%getcurx (.winptr stream)))

;;; Non-mandatory methods

;; Default method uses repeated calls to stream-write-char
;;(defmethod stream-write-string ((stream window) (str string) &optional (start 0) (end nil))
;;  ;; TODO: either do something with start and end, or (declare (ignore start end))
;;  (%waddstr (.winptr stream) str))

;;; Character Input Stream

;;(defmethod stream-read-char ((stream window))
;;  (code-char (%wgetch (.winptr stream))))

;;(defmethod stream-read-char-no-hang ((stream window))
;; %wgetch wie bei read-char, nur muss input-blocking nil sein.

;;(defmethod stream-unread-char ((stream window) (ch character))
;;  (%ungetch (char-code ch)))

;; listen = read-char-no-hang + unread
;;(defmethod stream-listen ((stream window))

