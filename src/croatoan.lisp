(in-package :de.anvi.croatoan)

;;; Define all macros here centrally.

(defmacro with-screen ((screen &key
                               (bind-debugger-hook t)
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
                               (fgcolor nil)
                               (bgcolor nil)
                               (color-pair nil)
                               (background nil))
                       &body body)
  "Create a screen, evaluate the forms in the body, then cleanly close the screen.

Pass any arguments besides BIND-DEBUGGER-HOOK to the initialisation of the
screen object. The screen is cleared immediately after initialisation.

This macro will bind *DEBUGGER-HOOK* so that END-SCREEN gets called before the
condition is printed. This will interfere with SWANK as it also binds *DEBUGGER-HOOK*.
To prevent WITH-SCREEN from binding *DEBUGGER-HOOK*, set BIND-DEBUGGER-HOOK to NIL.

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
                                      :fgcolor ,fgcolor
                                      :bgcolor ,bgcolor
                                      :color-pair ,color-pair
                                      :background ,background))

              ;; when an error is signaled and not handled, cleanly end ncurses, print the condition text
              ;; into the repl and get out of the debugger into the repl.
              ;; the debugger is annoying with ncurses apps.
              ;; add (abort) to automatically get out of the debugger.
              ;; this binding is added by default. call with-screen with :bind-debugger-hook nil to remove.
              ,@(if bind-debugger-hook
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
  ;; depending on which version of ncurses is loaded, decide which event reader to use.
  (let ((get-event-function
         #+(or sb-unicode unicode openmcl-unicode-strings) ''get-wide-event
         #-(or sb-unicode unicode openmcl-unicode-strings) ''get-event))
    (if (and mouse-y mouse-x)
        ;; when the variables y and x are passed, bind them to the mouse coordinates
        `(loop :named event-case do
            (multiple-value-bind (,event ,mouse-y ,mouse-x) (funcall ,get-event-function ,window)
              ;;(print (list ,event mouse-y mouse-x) ,window)
              (when (null ,event)
                ;; process the contents of the job queue (ncurses access from other threads)
                (process))
              (case ,event
                ,@body)))
        ;; default case, no mouse used
        `(loop :named event-case do
            (let ((,event (funcall ,get-event-function ,window)))
              (when (null ,event)
                ;; process the contents of the job queue (ncurses access from other threads)
                (process))
              (case ,event
                ,@body))))))

(defun bind (object event handler)
  "Bind the handler to the event in the bindings alist of the object.

The object can be a croatoan object (like window or form) or a keymap.

If event is a list of events, bind the handler to each event separately.

The handler can be a function object, a fbound symbol or a keymap.

The handler function will be called by the run-event-loop when
keyboard or mouse events occur. The functions have two mandatory
arguments, object and event.

If a keymap is bound to a key, this allows keys to be defined as
prefix keys, so event sequences like '^X a' can be chained together
and handled like a single event.

For every event-loop, at least an event to exit the event loop should
be assigned, by associating it with the predefined function
exit-event-loop.

If a handler for the default event t is defined, it will handle all
events for which no specific event handler has been defined.

If input-blocking of the window is set to nil, a handler for the nil
event can be defined, which will be called at a specified frame-rate
between keypresses. Here the main application state can be updated.

Alternatively, to achieve the same effect, input-blocking can be set
to a specific delay in miliseconds.

Example use: (bind scr #\q  (lambda (win event) (throw scr :quit)))"
  (with-accessors ((bindings bindings)) object
    (cond ((or (null event)
               (atom event))
           (if (stringp event)
               ;; when event is a control char in caret notation, i.e. "^A"
               (setf bindings (acons (string-to-char event) handler bindings))
               (setf bindings (acons event handler bindings))))
          ((listp event)
           (dolist (e event)
             (if (stringp e)
                 (setf bindings (acons (string-to-char e) handler bindings))
                 (setf bindings (acons e handler bindings))))))))

(defun unbind (object event)
  "Remove the event and the handler function from object's bindings alist.

If event is a list of events, remove each event separately from the alist."
  (with-accessors ((bindings bindings)) object
    (cond ((or (null event)
               (atom event))
           (if (stringp event)
               ;; when event is a control char in caret notation, i.e. "^A"
               (setf bindings (remove (string-to-char event) bindings :key #'car))
               (setf bindings (remove event bindings :key #'car))))
          ((listp event)
           (dolist (e event)
             (if (stringp e)
                 (setf bindings (remove (string-to-char e) bindings :key #'car))
                 (setf bindings (remove e bindings :key #'car))))))))

(defparameter *keymaps* nil "An alist of available keymaps.")

(defmacro define-keymap (name &body body)
  "Register a keymap given its name and (key function) pairs.

As with bind, the keys can be characters, two-char strings in caret notation for
control chars and keywords for function keys."
  `(progn
     (setf *keymaps* (acons ',name (make-instance 'keymap) *keymaps*))
     (%defcdr (bindings (cdr (assoc ',name *keymaps*))) ,@body)))

;; CL-USER> (%defcdr a (:a 'cdr) (:b #'car) (:c (lambda () 1)))
;; ((:C . #<FUNCTION (LAMBDA ()) {52C8868B}>)
;;  (:B . #<FUNCTION CAR>)
;;  (:A . #<FUNCTION CDR>))
(defmacro %defcdr (alist &body body)
  "Take an alist and populate it with key-value pairs given in the body."
  (when (car body)
    `(progn
       ;; add the first key-value pair to the alist
       (%defcar ,alist ,(car body))
       ;; recursively add the rest of the body
       (%defcdr ,alist ,@(cdr body)))))

;; CL-USER> (defparameter a ())
;; CL-USER> (%defcar a (:a 'car))
;; ((:A . #<FUNCTION CAR>))
(defmacro %defcar (alist (k v))
  "Push a single key-value list to the alist.

If value v is a symbol, first convert it to a function object.

The key can be a lisp character, a two-char string in caret notation for
control chars and a keyword for function keys.

If the key is given as a caret notation string, first convert it to
the corresponding control char."
  `(cond ((and (symbolp ,v) (fboundp ,v))
          ;; if the function is given as a symbol and is fbound
          (if (stringp ,k)
              ;; when the event is given as a 2-char string: "^A"
              (push (cons (string-to-char ,k) (fdefinition ,v)) ,alist)
              (push (cons ,k (fdefinition ,v)) ,alist)))
         ((functionp ,v)
          ;; if the function is given as a function object
          (if (stringp ,k)
              (push (cons (string-to-char ,k) ,v) ,alist)
              (push (cons ,k ,v) ,alist)))
         ((typep ,v 'keymap)
          (if (stringp ,k)
              (push (cons (string-to-char ,k) ,v) ,alist)
              (push (cons ,k ,v) ,alist)))
         (t
          (error "DEFINE-KEYMAP: Invalid binding type. Supported types: symbol, function, keymap."))))

(defun find-keymap (keymap-name)
  "Return a keymap given by its name from the global keymap alist."
  (cdr (assoc keymap-name *keymaps*)))

;; source: alexandria
(defun plist2alist (plist)
  "Take a plist in the form (k1 v1 k2 v2 ...), return an alist ((k1 . v1) (k2 . v2) ...)"
  (let (alist)
    (do ((lst plist (cddr lst)))
        ((endp lst) (nreverse alist))
      (push (cons (car lst) (cadr lst)) alist))))

(defun check-string-char (char)
  "If char is a string convert it to a character."
  (if (stringp char)
      (string-to-char char)
      char))

(defun convert-strings (bindings-plist)
  "Loop over a bindings plist, convert strings to characters."
  (mapcar #'check-string-char bindings-plist))

(defun assoc-unique (alist)
  "Return a copy of alist with duplicate entries removed."
  (let ((result nil)
        (rest alist))
    (loop while rest do
      (let* ((pair (car rest))
             (key (car pair)))
        (unless (assoc key result)
          (push pair result)))
      (setq rest (cdr rest)))
    (nreverse result)))

(defun assoc-merge (&rest alists)
  "Merge alists then return an alist with duplicate entries removed."
  (assoc-unique (apply #'append alists)))

(defun get-event-handler (object event)
  "Take an object and an event, return the object's handler for that event.

The key bindings alist is stored in the bindings slot of the object.

An external keymap can be defined so several objects can share the same
set of bindings.

Object-local bindings override the external keymap. The local bindings
are checked first for a handler, then the external keymap.

If no handler is defined for the event, the default event handler t is tried.
If not even a default handler is defined, the event is ignored.

If input-blocking is nil, we receive nil events in case no real events occur.
In that case, the handler for the nil event is returned, if defined.

The event pairs are added by the bind function as conses: (event . #'handler).

An event should be bound to the pre-defined function exit-event-loop."
  (flet ((ev (event)
           "Take an event, return the cons (event . handler)."
           (let* ((keymap (typecase object
                            (keymap object)
                            (t (typecase (keymap object)
                                 ;; the keymap can be a keymap object
                                 (keymap (keymap object))
                                 ;; or a symbol as the name of the keymap
                                 (symbol (find-keymap (keymap object)))))))
                  ;; object-local bindings override the external keymap
                  ;; an event is checked in the bindings first, then in the external keymap.
                  (bindings (assoc-merge (bindings object)
                                         (when (and keymap (bindings keymap)) (bindings keymap)))))
             (assoc event bindings))))
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
  "Read events from the object, then call predefined event handler functions on the events.

The object has to be a ncurses window or have an associated ncurses window.

The handlers can be added by the bind function, or by directly setting a predefined keymap
to the object's bindings slot.

Args is one or more additional arguments that can be passed to the handlers.

Provide a non-local exit point so we can exit the loop from an event handler. 

One of the events must provide a way to exit the event loop by 'throwing' the object.

The function exit-event-loop is pre-defined to perform this non-local exit."
  ;; throw object value specified the return value of the run-event-loop.
  (catch object
    (loop
      (handle-events object args)
      (process))))

(defun handle-events (object args)
  "Read a single event from the user, lookup a handler and apply it.

Set the frame rate for the idle (nil) event generated when input-blocking is nil."
  (let* ((event (get-wide-event object)))
    (if event
        ;; t
        (handle-event object event args)
        ;; The nil (idle) event should be defined directly in the object's bindings/keymap.
        ;; It has to be handled separately to allow handling of chained events.
        (let ((handler (get-event-handler object event)))
          (when handler
            (apply handler object event args))
          (when (frame-rate object)
            (sleep (/ 1.0 (frame-rate object))))))))

(defgeneric handle-event (object event args)
  (:documentation
   "Get the event handler from the object, then apply it to the object.

If the handler for an event is a keymap object, the handler for the
next event will be looked up from that keymap. This allows for several
events to be chained together."))

(defmethod handle-event (object event args)
  "Default method. If the current keymap is not nil, lookup from that keymap."
  (with-slots ((map current-keymap)) object
    (let ((handler (get-event-handler (if map map object) event)))
      (when handler
        (cond ((typep handler 'keymap)
               ;; when the handler is another keymap, lookup the next event from that keymap.
               (setf map handler))
              ((or (functionp handler) (symbolp handler))
               ;; when the handler is a function, lookup the next event from the object's bindings/keymap.
               (setf map nil)
               ;; if args is nil, apply will call the handler with just object and event
               ;; this means that if we dont need args, we can define most handlers as two-argument functions.
               (apply handler object event args)))))))

(defmethod handle-event ((object form) event args)
  "If a form can't handle an event, let the current element try to handle it."
  (with-slots ((map current-keymap)) object
    (let ((handler (get-event-handler (if map map object) event)))
      (if handler
        (cond ((typep handler 'keymap)
               ;; when the handler is another keymap, lookup the next event from that keymap.
               (setf map handler))
              ((or (functionp handler) (symbolp handler))
               ;; when the handler is a function, lookup the next event from the object's bindings/keymap.
               (setf map nil)
               ;; if args is nil, apply will call the handler with just object and event
               ;; this means that if we dont need args, we can define most handlers as two-argument functions.
               (apply handler object event args)))
        ;; if there is no handler in the form keymap, pass the event to the current element.
        (handle-event (current-element object) event args)))))

(defun exit-event-loop (object event &rest args)
  "Associate this function with an event to exit the event loop."
  (declare (ignore event args))
  (throw object :exit-event-loop))

(defmacro save-excursion (window &body body)
  "After executing body, return the cursor in window to its initial position."
  (let ((pos (gensym)))
    `(let ((,pos (cursor-position ,window)))
       ,@body
       (move ,window (car ,pos) (cadr ,pos)))))
