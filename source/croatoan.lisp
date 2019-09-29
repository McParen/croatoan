(in-package :de.anvi.croatoan)

;;; Define all macros here centrally.

(defmacro with-screen ((screen &key
                               (bind-debugger-hook-p t)
                               (input-buffering nil)
                               (process-control-chars t)
                               (enable-newline-translation t)
                               (input-blocking t)
                               (input-echoing t)
                               (enable-function-keys t)
                               (enable-scrolling nil)
                               (insert-mode nil)
                               (enable-colors t)
                               (use-terminal-colors nil)
                               (cursor-visible t)
                               (stacked nil)
                               (color-pair nil)
                               (background nil))
                       &body body)
  "Create a screen, evaluate the forms in the body, then cleanly close the screen.

Pass any arguments besides BIND-DEBUGGER-HOOK-P to the initialisation of the
screen object. The screen is cleared immediately after initialisation.

This macro will bind *DEBUGGER-HOOK* so that END-SCREEN gets called before the
condition is printed. This will interfere with SWANK as it also binds *DEBUGGER-HOOK*.
To prevent WITH-SCREEN from binding *DEBUGGER-HOOK*, set BIND-DEBUGGER-HOOK-P to NIL.

This macro is the main entry point for writing ncurses programs with the croatoan
library. Do not run more than one screen at the same time."
  `(unwind-protect
        (let ((,screen (make-instance 'screen
                                      :input-buffering ,input-buffering
                                      :process-control-chars ,process-control-chars
                                      :enable-newline-translation ,enable-newline-translation
                                      :input-blocking ,input-blocking
                                      :input-echoing ,input-echoing
                                      :enable-function-keys ,enable-function-keys
                                      :enable-scrolling ,enable-scrolling
                                      :insert-mode ,insert-mode
                                      :enable-colors ,enable-colors
                                      :use-terminal-colors ,use-terminal-colors
                                      :cursor-visible ,cursor-visible
                                      :stacked ,stacked
                                      :color-pair ,color-pair
                                      :background ,background))

              ;; when an error is signaled and not handled, cleanly end ncurses, print the condition text
              ;; into the repl and get out of the debugger into the repl.
              ;; the debugger is annoying with ncurses apps.
              ;; add (abort) to automatically get out of the debugger.
              ;; this binding is added by default. call with-screen with :bind-debugger-hook-p nil to remove.
              ,@(if bind-debugger-hook-p
                  '((*debugger-hook* #'(lambda (c h)
                                         (declare (ignore h))
                                         (end-screen)
                                         (print c))))
                  nil))

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

In order for event handling to work, input-buffering has to be nil.
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
              (when (null ,event)
                ;; process the contents of the job queue (ncurses access from other threads)
                (process))          
              (case ,event
                ,@body)))
      `(loop :named event-case do
          ;; depending on which version of ncurses is loaded, decide which event reader to use.
          (let ((,event #+(or sb-unicode unicode openmcl-unicode-strings) (get-wide-event ,window)
                        #-(or sb-unicode unicode openmcl-unicode-strings) (get-event ,window)))
            (when (null ,event)
              ;; process the contents of the job queue (ncurses access from other threads)
              (process))
            (case ,event
              ,@body)))))

(defun bind (object event handler)
  "Bind the handler function to the event in the bindings alist of the object.

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
delay in miliseconds.

Example use: (bind scr #\q  (lambda (win event) (throw 'event-loop :quit)))"
  (setf (bindings object)
        (acons event handler (bindings object))))

(defun unbind (object event)
  "Remove the event and the handler function from object's bindings alist."
  (setf (slot-value object 'bindings)
        (remove event (slot-value object 'bindings) :key #'car)))

(defparameter *keymaps* nil "An alist of available keymaps.")

(defun define-keymap (name plist)
  "Register a keymap given by a name and a plist of keys and functions."
  (let ((keymap (make-instance 'keymap :bindings-plist plist)))
    (setf *keymaps* (acons name keymap *keymaps*))))

(defun find-keymap (keymap-name)
  (cdr (assoc keymap-name *keymaps*)))

;; source: alexandria
(defun plist2alist (plist)
  "Take a plist in the form (k1 v1 k2 v2 ...), return an alist ((k1 . v1) (k2 . v2) ...)"
  (let (alist)
    (do ((lst plist (cddr lst)))
        ((endp lst) (nreverse alist))
      (push (cons (car lst) (cadr lst)) alist))))

(defun get-event-handler (object event)
  "Take an object and an event, return the handler for that event.

The key bindings alist is stored in the bindings slot of the object.

If no handler is defined for the event, the default event handler t is tried.
If not even a default handler is defined, the event is ignored.

If input-blocking is nil, we receive nil events in case no real events occur.
In that case, the handler for the nil event is returned, if defined.

The event pairs are added by the bind function as conses: (event . #'handler).

An event should be bound to the pre-defined function exit-event-loop."
  (flet ((ev (event)
           (let ((keymap (typecase (keymap object)
                           (keymap (keymap object))
                           (symbol (find-keymap (keymap object))))))
             ;; object-local bindings override the external keymap
             ;; an event is checked in the bindings first, then in the external keymap.
             (if (bindings object)
                 (if (assoc event (bindings object))
                     (assoc event (bindings object))
                     (if (and keymap (bindings keymap))
                         (assoc event (bindings keymap))
                         nil))
                 (if (and keymap (bindings keymap))
                     (assoc event (bindings keymap))
                     nil)))))
    (cond
      ;; Event occured and event handler is defined.
      ((and event (ev event)) (cdr (ev event)))
      ;; Event occured and a default event handler is defined.
      ;; If not even the default handler is defined, the event is ignored.
      ((and event (ev t)) (cdr (ev t)))
      ;; If no event occured and the idle handler is defined.
      ;; The event is only nil when input input-blocking is nil.
      ((and (null event) (ev nil)) (cdr (ev nil)))
      ;; If no event occured and the idle handler is not defined.
      (t nil))))

(defun run-event-loop (object &rest args)
  "Read events from the window, then call predefined event handler functions on the events.

The handlers can be added by the bind function, or by directly setting a predefined keymap
to the window's bindings slot.

Args is one or more additional argument passed to the handlers.

Provide a non-local exit point so we can exit the loop from an event handler. 

One of the events must provide a way to exit the event loop by throwing 'event-loop.

The function exit-event-loop is pre-defined to perform this non-local exit."
  (catch 'event-loop
    (loop
       (let* ((window (typecase object
                        (form-window (sub-window object))
                        (window object)
                        (otherwise (window object))))
              (event (get-wide-event window)))
         (handle-event object event args)
         ;; process the contents of the job queue (ncurses access from other threads)
         (process)
         ;; should a frame rate be a property of the window or of the object?
         (when (and (null event) (frame-rate window))
           (sleep (/ 1.0 (frame-rate window)))) ))))

(defgeneric handle-event (object event args)
  ;; the default method applies to window, field, button, menu.
  (:method (object event args)
    "Default method for all objects without a specialized method."
    (let ((handler (get-event-handler object event)))
      (when handler
        (apply handler object event args)))))

(defmethod handle-event ((form form) event args)
  "If a form cant handle an event, let the current form element try to handle it."
  (let ((handler (get-event-handler form event)))
    (if handler
        (apply handler form event args)
        (handle-event (current-element form) event args))))

(defun exit-event-loop (&optional win event args)
  "Associate this function with an event to exit the event loop."
  (declare (ignore win event args))
  (throw 'event-loop :exit-event-loop))

(defmacro save-excursion (window &body body)
  "After executing body, return the cursor in window to its initial position."
  `(let ((pos (cursor-position ,window)))
     ,@body
     (move ,window (car pos) (cadr pos))))
