(in-package :de.anvi.croatoan)

;;; Define all macros here centrally.

(defmacro with-screen ((screen &key
                               (input-buffering nil)
                               (process-control-chars t)
                               (input-blocking t)
                               (input-echoing  t)
                               (enable-fkeys   t) 
                               (enable-colors  t)
                               (use-default-colors nil)
                               (cursor-visibility t)
                               (stacked nil)
                               (color-pair nil)
                               (background nil))
                       &body body)
  "Create a screen, evaluate the forms in the body, then cleanly close the screen.

Pass any arguments to the initialisation of the screen object. The screen 
is cleared immediately after initialisation.

This macro is the main entry point for writing ncurses programs with the croatoan 
library. Do not run more than one screen at the same time."
  `(unwind-protect
        (let ((,screen (make-instance 'screen
                                      :input-buffering ,input-buffering
                                      :process-control-chars ,process-control-chars
                                      :input-blocking ,input-blocking
                                      :input-echoing  ,input-echoing
                                      :enable-fkeys   ,enable-fkeys
                                      :enable-colors  ,enable-colors
                                      :use-default-colors ,use-default-colors
                                      :cursor-visibility ,cursor-visibility
                                      :stacked ,stacked
                                      :color-pair ,color-pair
                                      :background ,background))

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

(defmacro with-window ((win &rest options) &body body)
  "Create a window, evaluate the forms in the body, then cleanly close the window.

Pass any arguments to the initialisation of the window object.

Example:

(with-window (win :input-echoing t
  body)"
  `(let ((,win (make-instance 'window ,@options)))
     (unwind-protect
          (progn
            ,@body)
       (close ,win))))

;; see similar macro cffi:with-foreign-objects.
(defmacro with-windows (bindings &body body)
  "Create one or more windows, evaluate the forms in the body, then cleanly close the windows.

Pass any arguments to the initialisation of the window objects.

Example:

(with-windows ((win1 :input-echoing t)
               (win2 :input-echoing t))
  body)"
  (if bindings
      ;; execute the bindings recursively
      `(with-window ,(car bindings)
         ;; the cdr is the body
         (with-windows ,(cdr bindings)
           ,@body))
      ;; finally, execute the body.
      `(progn
         ,@body)))

(defmacro event-case ((window event &optional mouse-y mouse-x) &body body)
  "Window event loop, events are handled by an implicit case form.

For now, it is limited to events generated in a single window. So events
from multiple windows have to be handled separately.

In order for event-handling to work, input-buffering has to be nil.
Several control character events can only be handled when 
process-control-chars is also nil.

If input-blocking is nil, we can handle the (nil) event, i.e. what
happens between key presses.

If input-blocking is t, the (nil) event is never returned.

The main window event loop name is hard coded to event-case to be
used with return-from.

Instead of ((nil) nil), which eats 100% CPU, use input-blocking t."
  (if (and mouse-y mouse-x)
      `(loop :named event-case do
          (multiple-value-bind (,event ,mouse-y ,mouse-x)
              ;; depending on which version of ncurses is loaded, decide which event reader to use.
              #+(or sb-unicode unicode openmcl-unicode-strings) (get-wide-event ,window)
              #-(or sb-unicode unicode openmcl-unicode-strings) (get-event ,window)
              ;;(print (list ,event mouse-y mouse-x) ,window)
              (case ,event
                ,@body)))
      `(loop :named event-case do
          ;; depending on which version of ncurses is loaded, decide which event reader to use.
          (let ((,event #+(or sb-unicode unicode openmcl-unicode-strings) (get-wide-event ,window)
                        #-(or sb-unicode unicode openmcl-unicode-strings) (get-event ,window)))
            (case ,event
              ,@body)))))

(defmacro add-event-handler ((window event) &body handler-function)
  "Add the event and its handler-function to the window's event handler alist.

The handlers will be called by the run-event-loop when keyboard or mouse events occur.

The handler functions have two mandatory arguments, window and event.

For every event-loop, at least an event to exit the event loop should be assigned,
by associating it with the predefined function exit-event-loop.

If a handler for the event :default is defined, it will handle all events for which
no specific event handler has been defined.

If input-blocking of the window is set to nil, a handler for the nil event
can be defined, which will be called at a specified frame-rate between keypresses.
Here the main application state can be updated.

Alternatively, to achieve the same effect, input-blocking can be set to a specific
delay in miliseconds."
  `(setf (slot-value ,window 'event-handlers)
         ;; we need to make handler-function a &body so it is indented properly by slime.
         (acons ,event ,@handler-function (slot-value ,window 'event-handlers))))

(defmacro remove-event-handler (window event)
  "Remove the event and the handler function from a windows event-handlers collection."
  `(setf (slot-value ,window 'event-handlers)
         (remove ,event (slot-value ,window 'event-handlers) :key #'car)))

(defparameter *keymaps* nil
  "An alist of available keymaps that can be read and written by get-keymap and add-keymap.")

(defun make-keymap (&rest args)
  "Take a list of keys and values, return an event handler keymap.

Currently the keymap is implemented as an alist, but will be converted
to a hash table in the future."
  (loop for (i j) on args by #'cddr
    collect (cons i j)))

(defun get-keymap (keymap-name)
  "Take a keyword denoting a keymap name, return a keymap object from the global keymap collection."
  (cdr (assoc keymap-name *keymaps*)))

(defun add-keymap (keymap-name keymap)
  "Add a keymap by its name to the global keymap collection."
  (setf *keymaps* (acons keymap-name keymap *keymaps*)))

(defun run-event-loop (win &optional args)
  "Read events from the window, then call predefined event handler functions on the events.

The handlers can be added by the macro add-event-handler, or by directly setting
a predefined keymap to the window's event-handlers property.

Args is either a single additional argument passed to the handlers, or a list of arguments."
  ;; provide a non-local exit point so we can exit the loop from an event handler.
  ;; one of the events MUST provide a way to exit the event loop by throwing 'event-loop
  ;; example use:
  ;; (add-event-handler (scr #\q)
  ;;   (lambda (win event)
  ;;     (throw 'event-loop :quit)))
  ;; the function exit-event-loop is pre-defined to perform this non-local exit.
  (catch 'event-loop
    (loop
       (let ((event (get-wide-event win)))
         ;; when event is not nil (it is nil only when input-blocking is nil)
         (if event
             ;; event-pair = (event . #'handler)
             (let ((event-pair (assoc event (slot-value win 'event-handlers))))
               ;; if there is no registered event handler, assoc will return nil
               (if event-pair
                   ;; if the event handler is defined, call it with two arguments, win and event.
                   ;;(funcall (cdr event-pair) win event)
                   ;; pass args as a LIST to the handler, not as separate parameters as apply would do.

                   ;; we only want to have to deal with args if an additional argument is passed to run-event-loop
                   ;; if none are passed, the handlers should only have to take window and event as params.
                   (if args
                       ;; funcall will call both 'functions by their quoted symbols, and #'function objects.
                       (funcall (cdr event-pair) win event args)
                       (funcall (cdr event-pair) win event))

                   ;; if no handler is defined for the event, call the default handler
                   (let ((event-pair (assoc :default (slot-value win 'event-handlers))))
                     (if event-pair
                         ;; if a handler for :default is defined
                         (if args
                             (funcall (cdr event-pair) win event args)
                             (funcall (cdr event-pair) win event))
                         
                         ;; if a handler for :default is not defined, the event is ignored.
                         nil)) ))
             ;; what to do between key presses (nil event)
             ;; the user has to define a handler for the nil event
             ;; TODO: ensure that the nil event is only processed when :input-blocking is nil (or a delay)
             (let ((event-pair (assoc nil (slot-value win 'event-handlers))))
               (if event-pair
                   ;; call the handler associated with the nil event
                   (if args
                       (funcall (cdr event-pair) win event args)
                       (funcall (cdr event-pair) win event))
                   
                   ;; default action for the nil event when input-blocking is nil
                   nil)
               ;; sleep between the nil events to reduce the CPU load
               (when (.frame-rate win)
                 (sleep (/ 1.0 (.frame-rate win))))) )))))

(defun exit-event-loop (win event &optional args)
  "Associate this function with an event to exit the event loop."
  (declare (ignore win event args))
  (throw 'event-loop :exit-event-loop))

(defmacro save-excursion (window &body body)
  "After executing body, return the cursor in window to its initial position."
  `(let ((pos (.cursor-position ,window)))
     ,@body
     (move ,window (car pos) (cadr pos))))
