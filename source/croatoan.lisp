(in-package :de.anvi.croatoan)

;;; Define all macros here centrally.

(defmacro with-screen ((screen &key 
                               (input-reading  :unbuffered) 
                               (input-blocking t)
                               (input-echoing  t)
                               (enable-fkeys   t) 
                               (enable-colors  t)
                               (use-default-colors nil)
                               (cursor-visibility t))
                       &body body)
  "Create a screen, evaluate the forms in the body, then cleanly close the screen.

Pass any arguments to the initialisation of the screen object. The screen 
is cleared immediately after initialisation.

This macro is the main entry point for writing ncurses programs with the croatoan 
library. Do not run more than one screen at the same time."
  `(unwind-protect
        (let ((,screen (make-instance 'screen
                                      :input-reading  ,input-reading
                                      :input-blocking ,input-blocking
                                      :input-echoing  ,input-echoing
                                      :enable-fkeys   ,enable-fkeys
                                      :enable-colors  ,enable-colors
                                      :use-default-colors ,use-default-colors
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

(defmacro event-case ((window event &optional mouse-y mouse-x) &body body)
  "Window event loop, events are handled by an implicit case form.

For now, it is limited to events generated in a single window. So events
from multiple windows have to be handled separately.

In order for event-handling to work, input-reading has to be unbuffered.

If input-blocking is nil, we can handle the (nil) event, i.e. what
happens between key presses.

If input-blocking is t, the (nil) event is never returned.

The main window event loop name is hard coded to event-case-loop to be
used with return-from.

Instead of ((nil) nil), which eats 100% CPU, use input-blocking t."
  (if (and mouse-y mouse-x)
      `(loop named event-case do
            (multiple-value-bind (,event ,mouse-y ,mouse-x) (get-event ,window)
              ;;(print (list ,event mouse-y mouse-x) ,window)
              (case ,event
                ,@body)))
      `(loop named event-case do
            (let ((,event (get-event ,window)))
              (case ,event
                ,@body)))))

(defmacro save-excursion (window &body body)
  "After executing body, return the cursor in window to its initial position."
  `(let ((pos (.cursor-position ,window)))
     ,@body
     (move ,window (car pos) (cadr pos))))
