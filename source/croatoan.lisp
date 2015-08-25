(in-package :de.anvi.croatoan)

;;; Define all macros here centrally.

(defmacro with-screen ((screen &key 
                               (input-reading  :unbuffered) 
                               (input-blocking t)
                               (input-echoing  t)
                               (enable-fkeys   t) 
                               (enable-colors  t)
                               (cursor-visibility t))
                       &body body)
  `(unwind-protect
        (let ((,screen (make-instance 'screen
                                      :input-reading  ,input-reading
                                      :input-blocking ,input-blocking
                                      :input-echoing  ,input-echoing
                                      :enable-fkeys   ,enable-fkeys
                                      :enable-colors  ,enable-colors
                                      :cursor-visibility ,cursor-visibility))

              ;; when an error is signaled and not handled, cleanly end ncurses, print the condition text
              ;; into the repl and get out of the debugger into the repl.
              ;; the debugger is annoying with ncurses apps.
              ;; add (abort) to automatically get out of the debugger.
              (*debugger-hook* #'(lambda (c h) (declare (ignore h)) (end-screen) (print c) )))

          ;; clear the display when starting up.
          (clear ,screen)

          ,@body)

     ;; cleanly exit ncurses whatever happens.
     (end-screen)))

;; window event loop, behaves like case. at the moment, it is limited to a single window.
;; for this to work, input-reading has to be unbuffered and input-blocking has to be nil.
;; the main window event loop name is hard coded to "event-case-loop" to be used with return-from.
(defmacro with-event-case-loop ((window event) &body body)
  `(loop named event-case-loop do
        (let ((,event (get-event ,window)))
              (case ,event
                ,@body))))
