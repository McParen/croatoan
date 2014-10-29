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
              (*debugger-hook* #'(lambda (c h) (declare (ignore h)) (end-screen) (print c) (abort))))

          ;; clear the display when starting up.
          (clear ,screen)

          ,@body)

     ;; cleanly exit ncurses whatever happens.
     (end-screen)))

