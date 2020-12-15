(in-package :de.anvi.croatoan)

;;; thread-safe queue

;; support for cross-thread evaluation
;; authors: d4ryus <d4ryus@teknik.io>,
;;          Anton Vidovic <anton.vidovic@gmx.de>

(defclass queue ()
  ((head
    :documentation "Pointer to the first cons of the elements list.")
   (tail
    :documentation "Pointer to the last cons of the elements list.")
   (lock
    :documentation "A mutex that ensures thread-safe access to the queue from multiple threads."))
  (:documentation  "A thread-safe FIFO queue."))

(defmethod initialize-instance :after ((queue queue) &key)
  (with-slots (head tail lock) queue
    (setf head (cons nil nil)
          tail (last head)
          lock (bt:make-lock))))

(define-condition job-error (error)
  ((form
    :initarg       :form
    :initform      (error "Form required")
    :documentation "The form that failed to execute")
   
   (error
    :initarg       :error
    :initform      (error "Error required")
    :documentation "The error that was signaled when form was executed")))

(defmethod print-object ((obj queue) stream)
  (with-slots (head lock) obj
    (bt:with-lock-held (lock)
      (print-unreadable-object (obj stream :type t)
        (format stream "~:[<empty>~;~:*~a~]" (cdr head))))))

(defmethod print-object ((obj job-error) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (with-slots (form error) obj
      (format stream "form: ~a ~_error: ~a" form error))))

(defgeneric enqueue (item queue))

(defgeneric dequeue (queue))

(defmethod enqueue (item (queue queue))
  "Push a new item onto the tail of the queue, return the new item."
  (with-slots (tail lock) queue
    (bt:with-lock-held (lock)
      (car (setf tail (cdr (rplacd tail (list item))))))))

(defmethod dequeue ((queue queue))
  "Pop of the first element of queue and return it, returns NIL when queue is empty"
  (with-slots (head lock) queue
    (bt:with-lock-held (lock)
      (when (cdr head)
        (car (setf head (cdr head)))))))

(defparameter *job-queue* (make-instance 'queue)
  "A queue of functions (consed to their form for debugging purposes)
to be processed by the main thread to interface with ncurses.")

(defmacro submit (&body body)
  "Submit BODY from a producer thread to a job queue to be processed by the main thread.

The main thread should be the only one interfacing ncurses directly,
and should be running in a terminal.

SUBMIT uses a thread-safe FIFO queue to queue up jobs which should be
evaluated inside the terminal thread.

For this to work PROCESS has to be called from inside the terminal
thread to pop requests from the FIFO queue and evaluate them.

When a condition is signaled while the body of SUBMIT is evaluated, it
is handled by PROCESS and put inside a JOB-ERROR which also contains
the failed form.

The condition is then signaled again from within a restart which
allows skipping the failed form and continue evaluating requests.

In practice, this allows for calls to ncurses forms from SLIME to be
performed without IO glitches that tend to occur when ncurses code is
called from the SLIME repl thread directly."
  `(enqueue
    (cons (lambda () ,@body)
          ;; the failed form is included for debugging.
          ',body)
    *job-queue*))

(defun process ()
  "Process the contents of the job queue in the current thread, then exit.

Process should be called from the main thread, which should be the only thread 
interfacing ncurses directly, and should be running in a terminal."
  (loop :for (fn . form) := (dequeue *job-queue*)
        :while fn
        ;; We want to be able to see what form failed, for this we
        ;; need to wrap the signaled condition in a job-error
        ;; which contains the failed form and the signaled condition.
        :do (handler-case (funcall fn)
              (error (e)
                (restart-case (error 'job-error
                                     :form form
                                     :error e)
                  (skip-job-form ()
                    :report (lambda (stream)
                              (format stream "Skip job form"))))))))

;;; Simple, non-thread-safe queue

(defclass simple-queue ()
  ((head
    :documentation "Pointer to the first cons of the elements list.")
   (tail
    :documentation "Pointer to the last cons of the elements list."))
  (:documentation  "A simple FIFO queue (not thread-safe)."))

(defmethod initialize-instance :after ((queue simple-queue) &key)
  (with-slots (head tail) queue
    (setf head (cons nil nil)
          tail (last head))))

(defmethod enqueue (item (queue simple-queue))
  "Push a new item onto the tail of the queue, return the new item."
  (with-slots (tail) queue
    (car (setf tail (cdr (rplacd tail (list item)))))))

(defmethod dequeue ((queue simple-queue))
  "Pop of the first element of queue and return it, returns NIL when queue is empty."
  (with-slots (head) queue
    (when (cdr head)
      (car (setf head (cdr head))))))
