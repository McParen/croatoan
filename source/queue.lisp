(in-package :de.anvi.croatoan)

;; queue
;; support for cross-thread evaluation
;; author: d4ryus <d4ryus@teknik.io>

;; Thread-safe FIFO queue, see MAKE-QUEUE, QUEUE-PUSH and QUEUE-POP
(defstruct (queue (:constructor %make-queue (init-head &aux (init-tail (last init-head)))))
  (lock (bt:make-lock) :read-only t)
  (head init-head :read-only t)
  (tail init-tail :read-only t))

(defun make-queue (&key initial-contents)
  (%make-queue (cons nil initial-contents)))

(define-condition job-error (error)
  ((form :initarg :form
         :initform (error "Form required")
         :documentation "The form that failed to execute")
   (error :initarg :error
         :initform (error "Error required")
         :documentation "The error that was signaled when form was executed")))

(defmethod print-object ((obj queue) stream)
  (with-slots (head lock) obj
    (bt:with-lock-held (lock)
      (print-unreadable-object (obj stream :type t)
        (format stream "~:[<empty>~;~:*~a~]" (rest head))))))

(defmethod print-object ((obj job-error) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (with-slots (form error) obj
      (format stream "form: ~a ~_error: ~a"
              form error))))

(defmethod queue-push ((obj queue) new)
  "Push an element onto the back of the queue and return it"
  (with-slots (tail lock) obj
    (bt:with-lock-held (lock)
      (first (setf tail (cdr (rplacd tail (list new))))))))

(defmethod queue-pop ((obj queue))
  "Pop of the first element of queue and return it, returns NIL when queue is empty"
  (with-slots (head lock) obj
    (bt:with-lock-held (lock)
      (when (rest head)
        (first (setf head (rest head)))))))

;; queue of functions consed to their form which are queued to be
;; executed inside the ncurses thread
(defparameter *job-queue* (make-queue))

(defmacro submit (&body body)
  "Submit BODY from a producer thread to a job queue to be processed by the main thread.

The main thread should be the only one interfacing ncurses directly, and should
be running in a terminal."
  `(queue-push *job-queue*
               (cons (lambda () ,@body)
                     ',body)))

(defun process ()
  "Process the contents of the job queue in the current thread, then exit.

Process should be called from the main thread, which should be the only thread 
interfacing ncurses directly, and should be running in a terminal."
  (loop :for (fn . form) := (queue-pop *job-queue*)
        :while fn
        ;; We want to be able to see what form failed, for this we
        ;; need to wrap the signald condition in a job-error
        ;; which contains the failed form and the signaled condition.
        :do (handler-case (funcall fn)
              (error (e)
                (restart-case (error 'job-error
                                     :form form
                                     :error e)
                  (skip-job-form ()
                    :report (lambda (stream)
                              (format stream "Skip job form"))))))))
