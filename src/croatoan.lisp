(in-package :de.anvi.croatoan)

;;; Define all macros here centrally.

(defmacro with-screen ((screen &key
                               (bind-debugger-hook t)
                               (input-buffering nil)
                               (process-control-chars t)
                               (enable-newline-translation t)
                               (input-blocking t)
                               (frame-rate nil)
                               (input-echoing t)
                               (enable-function-keys t)
                               (enable-soft-labels nil)
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
                                      :frame-rate ,frame-rate
                                      :input-echoing ,input-echoing
                                      :enable-function-keys ,enable-function-keys
                                      :enable-soft-labels ,enable-soft-labels
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

(defmacro event-case ((window event) &body body)
  "Window event loop, events are handled by an implicit case form.

For now, it is limited to events generated in a single window.
So events from multiple windows have to be handled separately.

In order for event handling to work, input-buffering has to be nil.
Several control character events can only be handled when
process-control-chars is also nil.

Allowed values for keys are lisp characters, function key names as
keywords, t for all events or (nil) for the idle event.

Key structs or emacs-style key specs are not supported, because the
underlying lisp eql can not test for structs and strings.

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
    `(loop :named event-case do
       (let ((,event (funcall ,get-event-function ,window)))
         (when (null (event-key ,event))
           ;; when idle, process the contents of the job queue (ncurses access from other threads)
           (process))
         ;; run the hook only for non-nil events.
         (when (event-key ,event)
           (run-hook ,window 'before-event-hook))
         ;; handle the actual events.
         ;; event-key can be lisp characters, integers or key structs.
         ;; if the key is a struct, pass the key-name (lisp keyword) to case.
         (case (if (typep (event-key ,event) 'key)
                   (key-name (event-key ,event))
                   (event-key ,event))
           ,@body)
         (when (event-key ,event)
           (run-hook ,window 'after-event-hook))))))

(defun tokenize-key-spec (str)
  "Take a single key spec, return a list with key and modifiers.

Character keys are returned as lisp characters, function keys are
returned as keywords.

The modifiers ctrl, alt (meta) and shift, are returned as single
character strings, C, M and S.

Examples:

C-M-S-return => (#\Return S M C)

C-<backspace> => (:BACKSPACE C)

The list is then further handled by parse-key-spec, which returns
a list of chars and assembled key structs."
  (flet ((function-key-p (str)
           ;; Return t if spec represents a function key.
           ;; It is a function key if the char is enclosed in <>.
           (let ((beg (position #\< str))
                 (end (position #\> str)))
             ;; is > the last char, do we have < before that
             (if (and end
                      (= end (1- (length str)))
                      beg)
                 ;; if a function key, return its position,
                 ;; so we can easily extract the value
                 beg
                 nil)))
         ;; check if the char is a minus/hyphen char
         (char-hyphen-p (str)
           ;; if len=1, check if the last char of str is -.
           ;; if len>1, check if both last chars are --
           (cond ((= (length str) 0)
                  nil)
                 ((= (length str) 1)
                  (char= #\- (char str (1- (length str)))))
                 ((> (length str) 1)
                  (and (char= #\- (char str (- (length str) 1)))
                       (char= #\- (char str (- (length str) 2))))))))
    ;; check if spec is an empty string
    (unless (= (length str) 0)
      ;; check if spec is a <function-key> or a character
      (let ((fkeyp (function-key-p str)))
        (if fkeyp
            ;; function key
            (let (mods)
              (if (= fkeyp 0) ; pos 0 means there are no mods.
                  (setq mods nil)
                  ;; if there are mods, split them on -
                  (setq mods (split-string (subseq str 0 fkeyp) #\-)))
              ;; return (:key-name "C" "M" "S")
              (cons (intern (string-upcase (subseq str (1+ fkeyp) (1- (length str))))
                            "KEYWORD")
                    mods))
            ;; character
            ;;  check if the (last) char is a hyphen -
            (if (char-hyphen-p str)
                ;; char is a hyphen -
                (let ((mods (reverse (split-string str #\-)))) ; after split we only have mods left
                  (cons #\- mods))
                ;; char is not a hyphen -
                ;;  check if there is a char at all, or we only have mods.
                (if (char= #\- (char str (- (length str) 1)))
                    ;; we only have mods "C-M-", a char is not given, return nil as an error
                    nil
                    ;; a char is given and it is not -, so we can safely split the spec on -
                    (let* ((lst (reverse (split-string str #\-)))  ; move char to car, mods to cdr
                           (char (car lst))
                           (mods (cdr lst)))
                      (cond ((= 1 (length char))
                             ;; coerce "a" t0 #\a
                             (cons (coerce char 'character) mods))
                            (t
                             ;; convert "return" to #\Return
                             (cons (name-char char) mods)))))))))))

(defun parse-key-spec (spec)
  (let* ((tokens (tokenize-key-spec spec))
         (event (car tokens))
         (mods (when (cdr tokens)
                 (mapcar (lambda (i)
                           (char-upcase (coerce i 'character)))
                         (cdr tokens))))
         (ctrl (if (member #\C mods) t nil))
         (alt (if (member #\M mods) t nil))
         (shift (if (member #\S mods) t nil)))
    (typecase event
      (null event)
      (character
       ;; the first 32 chars are control chars, so ignore ctrl and shift
       (when (and (>= (char-code event) 0)
                  (<= (char-code event) 31))
         (setq ctrl nil
               shift nil))
       ;; if an alphabetic char is already upcased, ignore shift
       (when (and (alpha-char-p event)
                  (char= event (char-upcase event))
                  shift)
         (setq shift nil))
       ;; if an alphabetic char is downcase, upcase it and delete shift
       (when (and (alpha-char-p event)
                  (char= event (char-downcase event))
                  shift)
         (setq shift nil
               event (char-upcase event)))
       ;; if ctrl is t, convert chars 64-95 to their corresponding control chars
       (when (and (>= (char-code event) 64)
                  (<= (char-code event) 95)
                  ctrl)
         (setq ctrl nil
               event (code-char (- (char-code event) 64))))
       ;; do the same for the lower-case chars 97-122
       (when (and (>= (char-code event) 97)
                  (<= (char-code event) 122)
                  ctrl)
         (setq ctrl nil
               event (code-char (- (char-code event) 96))))
       ;; edge case:
       ;; Return ^? or C-? as <backspace> key or #\rubout char
       ;; but only if Alt is not pressed at the same time.
       ;; on my keyboard,  <-- sends ^? 127 <backspace>
       ;;                C-<-- sends ^H   8 #\backspace
       (if (and ctrl
                (not alt)
                (char= #\? event))
           (list (make-key :name :backspace))

           ;; all other return values:
           ;; if alt is t, return a list of #\esc and the event char.
           (if alt
               (list #\esc
                     event)
               (list event))))
      (keyword
       (list (make-key :name event
                       :ctrl ctrl
                       :alt  alt
                       :shift shift))))))

(defun parse-key (spec)
  "Take a string with zero or more key specs, return a list of events."
  (if (position #\space spec)
      ;; If the spec contains spaces, it contains more than one key
      (let ((spec (split-string spec)))
        ;; nconc the keys to one big list
        (mapcan #'parse-key-spec spec))
      (parse-key-spec spec)))

(defun bind (object event handler)
  "Bind the handler to the event in the bindings alist of the object.

The object can be a croatoan object (like window or form) or a keymap.

If event argument is a list, bind the handler to the whole sequence
of events (key chain).

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

If a default event handler (for the event t) is defined, it will
handle all events for which no specific event handler has been
defined.

If the handler for an event is the symbol t, the event is ignored.
If the event was part of an event sequence, the handler t cancels the
input sequence.

Bind adds t as the default handler to all implicitly created
sub-keymaps, which cancels the input sequence of a key chain in case
an unknown key is typed.

If input-blocking of the window is set to nil, a handler for the nil
event can be defined, which will be called at a specified frame-rate
between keypresses. Here the main application state can be updated.

Alternatively, to achieve the same effect, input-blocking can be set
to a specific delay in miliseconds.

Example use: (bind scr #\q (lambda (win event) (throw scr :quit)))"
  ;; use slot instead of accessor
  (with-accessors ((bindings bindings)) object
    (cond ((or (null event)
               (atom event))
           (typecase event
             ;; if event is a string, treat it as a key spec.
             (string
              (let ((res (parse-key event)))
                ;; "" = nil, but we wont use empty strings for nil events.
                (when res
                  (if (= 1 (length res))
                      ;; if we only have one single key in the parsed list
                      (setf bindings (acons (car res) handler bindings))
                      ;; if the string contains more than one key, pass the parsed list to bind again.
                      (bind object res handler)))))
             ;; if event is a keyword, treat it as a key name without modifiers.
             (keyword
              (setf bindings (acons (make-key :name event) handler bindings)))
             ;; other event types:
             ;; t      default event handler for all undefined events.
             ;; nil    idle handler for input-blocking nil.
             ;; character events, function key structs, integer key codes.
             (t
              (setf bindings (acons event handler bindings)))))
          ;; instead of a single event, bind a chain of events.
          ;; the first event is a prefix key and is bound to a sub-keymap instead of a handler.
          ;; the first event that is bound to a handler completes the chain.
          ((listp event)
           (labels ((bind-keys (keys value node)
                      ;; if any of the list elements is still "^X" in caret notation, parse it to a char.
                      (let ((ch (if (and (stringp (elt keys 0))
                                         (= (length (elt keys 0)) 2)
                                         (char= (char (elt keys 0) 0) #\^))
                                    (string-to-char (elt keys 0))
                                    ;; if a list element is a keyword, treat it as key name without modifiers.
                                    (if (keywordp (elt keys 0))
                                        (make-key :name (elt keys 0))
                                        (elt keys 0)))))
                        (if (> (length keys) 1)
                            ;; recursively bind prefix keys to sub-keymaps
                            ;; first: get an existing keymap or make a new one
                            (let ((map (if (and (assoc ch (bindings node))
                                                (typep (cdr (assoc ch (bindings node))) 'keymap))
                                           ;; if a keymap already exists, take it.
                                           (cdr (assoc ch (bindings node)))
                                           ;; if it doesnt exist, make a new one only if value is non-nil.
                                           ;; if value is nil, map will be nil, and no new sub-binding will be made.
                                           (when value
                                             (make-instance 'keymap
                                                            ;; for sub-maps, the default handler is t, which means
                                                            ;; the reading of a key sequence will be aborted if an
                                                            ;; unknown key is hit, and will not block waiting for
                                                            ;; a known binding.
                                                            :bindings (list t t))))))
                              ;; then proceed with the recursion but only if a map is available
                              (when map
                                ;; first bind the sub-keys to the map
                                (bind-keys (cdr keys) value map)
                                ;; if there already is an existing binding and the value is a keymap
                                (if (and (assoc ch (bindings node))
                                         (typep (cdr (assoc ch (bindings node))) 'keymap))
                                    ;; delete the binding (and thus the keymap) if the keymap is empty.
                                    ;; this ensures that the keymaps are deleted when their last binding is deleted.
                                    (unless (bindings (cdr (assoc ch (bindings node))))
                                      (setf (bindings node) (delete ch (bindings node) :key #'car)))
                                    (progn
                                      ;; if there already is a binding, and it is not a keymap
                                      ;; overwrite it with the new keymap binding.
                                      (when (and (assoc ch (bindings node))
                                                 (not (typep (cdr (assoc ch (bindings node))) 'keymap)))
                                        ;; delete the old binding before pushing a new one
                                        (setf (bindings node) (delete ch (bindings node) :key #'car)))
                                      ;; then push a subkeymap
                                      (push (cons ch map) (bindings node))))))
                            ;; if length = 1 we're at the last event which is bound to the actual value
                            (progn
                              ;; again, delete the old binding before pushing a new one
                              (when (assoc ch (bindings node))
                                (setf (bindings node) (delete ch (bindings node) :key #'car)))
                              ;; but push the new one only if value is non-nil,
                              ;; which means value nil deletes the binding.
                              (when value
                                (push (cons ch value) (bindings node))))))))
             (bind-keys event handler object))))))

(defun unbind (object event)
  "Remove the event and the handler function from object's bindings alist.

If event argument is a list, remove the whole sequence of events (key chain)."
  ;; use slot instead of accessor
  (with-accessors ((bindings bindings)) object
    (cond ((or (null event)
               (atom event))
           (if (and (stringp event)
                    (= (length event) 2)
                    (char= (char event 0) #\^))
               ;; when event is a control char in caret notation, i.e. "^A"
               (setf bindings (remove (string-to-char event) bindings :key #'car))
               (setf bindings (remove event bindings :key #'car))))
          ;; to unbind a key chain, bind it to nil
          ((listp event)
           (bind object event nil)))))

(defparameter *keymaps* nil "An alist of available keymaps.")

;;(define-keymap t28b-map (parent-map)
;;  (#\q  'exit-event-loop)
;;  (#\a  't28-hello)
;;  ("^D" 't28-clear))
(defmacro define-keymap (name (&rest parent) &body body)
  "Register a keymap given its name and (key function) pairs.

As a second argument, optionally a parent keymap can be given.

Keys can be characters, key structs for function keys with modifiers
or a keyword for a function key without modifiers, which will
implicitely converted to a struct.

In addition to single chars or keys, emacs-style specs like 'C-x 2'
or 'C-M-S-<home>' can be given as strings, which will be parsed to
char and key lists and then bound to the given event.

If a keymap with the same name already exists, it will be deleted
before the new one is added."
  `(progn
     (setf *keymaps*
           (acons ',name
                  ,(if parent
                       `(make-instance 'keymap :parent (find-keymap ',(car parent)))
                       '(make-instance 'keymap))
                  ;; before adding a new keymap, delete the previous with
                  ;; the same name a new definition replaces the previous
                  (if (assoc ',name *keymaps*)
                      (delete ',name *keymaps* :key #'car)
                      *keymaps*)))
     (%defcdr (cdr (assoc ',name *keymaps*))
       ,@body)))

;; CL-USER> (%defcdr a (:a 'cdr) (:b #'car) (:c (lambda () 1)))
;; ((:C . #<FUNCTION (LAMBDA ()) {52C8868B}>)
;;  (:B . #<FUNCTION CAR>)
;;  (:A . #<FUNCTION CDR>))
(defmacro %defcdr (keymap &body body)
  "Take a keymap and populate it with key-value pairs given in the body."
  (when (car body)
    `(progn
       ;; add the first key-value pair to the alist
       (bind ,keymap ,@(car body))
       ;; recursively add the rest of the body
       (%defcdr ,keymap ,@(cdr body)))))

(defun find-keymap (keymap-name)
  "Return a keymap given by its name from the global keymap alist."
  (cdr (assoc keymap-name *keymaps*)))

;; source: alexandria
(defun plist2alist (plist)
  "Take a plist in the form (k1 v1 k2 v2), return an alist ((k1 . v1) (k2 . v2))"
  (let (alist)
    (do ((lst plist (cddr lst)))
        ((endp lst) (nreverse alist))
      (push (cons (car lst) (cadr lst)) alist))))

;; (loop for (k . v) in alist nconcing (list k v))
(defun alist2plist (alist)
  "Take an alist ((k1 . v1) (k2 . v2)), return a plist (k1 v1 kv v2)."
  (let (plist)
    (do ((lst alist (cdr lst)))
        ((null lst))
      (push (caar lst) plist)
      (push (cdar lst) plist))
    (nreverse plist)))

(defun check-string-char (char)
  "If char is a string convert it to a character."
  (if (stringp char)
      (string-to-char char)
      char))

(defun convert-strings (bindings-plist)
  "Loop over a bindings plist, convert strings to characters."
  (mapcar #'check-string-char bindings-plist))

;; CL-USER> (assoc-unique '((a . 1) (b. 2) (a . 3)))
;; ((A . 1) (B. 2))
(defun assoc-unique (alist)
  "Return a copy of alist with duplicate entries (keys) removed.

Only the first (newest) unique key is kept, others are discarded."
  (let ((result nil)
        (rest alist))
    (loop while rest do
      (let* ((pair (car rest))
             (key (car pair)))
        ;; keep the key only if it is not already present.
        ;; this means only the first unique key is kept.
        (unless (assoc key result)
          (push pair result)))
      (setq rest (cdr rest)))
    (nreverse result)))

;; https://github.com/troyp/asoc.el/blob/master/asoc.el
;; https://stackoverflow.com/questions/36497825/i-want-to-merge-and-sort-two-sorted-lists-with-common-lisp
;; CL-USER> (assoc-merge '((a . 1) (b . 2)) '((a . 3) (b . 4) (c . 5)))
;; ((A . 1) (B . 2) (C . 5))
(defun assoc-merge (&rest alists)
  "Merge alists then return an alist with duplicate entries removed."
  (assoc-unique (apply #'append alists)))

(defun get-event-handler (object event)
  "Take an object and an event key, return the object's handler for that event.

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
  (labels ((get-handler-from-keymap (event keymap)
             ;; return a binding pair from a keymap, if it exists.
             ;; if it doesnt exist, and the keymap has a parent,
             ;; recursively check the parent.
             (let ((pair (assoc event (bindings keymap) :test #'equalp)))
               (if pair
                   pair
                   (if (parent keymap)
                       (get-handler-from-keymap event (parent keymap))
                       nil))))
           (ev (event)
             "Take an event, return the cons (event . handler)."
             ;; take an event key, return a binding pair, if it exists.
             ;; the bindings are checked in the following order:
             ;; - local bindings of the object are checked first
             ;; - if a binding is not found, the associated keymap is checked.
             ;; - if a binding there is not found, the parent of the keymap is checked.
             (let* ((keymap (typecase object
                              ;; if object is a keymap
                              (keymap object)
                              ;; if object has a keymap slot
                              (t (when (keymap object)
                                   (typecase (keymap object)
                                     ;; the keymap can be a keymap object
                                     (keymap (keymap object))
                                     ;; or a symbol as the name of the keymap
                                     (symbol (find-keymap (keymap object)))))))))
               ;; object-local bindings override the external keymap
               ;; an event is checked in the bindings first, then in the external keymap.
               (let ((pair (assoc event (bindings object) :test #'equalp)))
                 (if pair
                     pair
                     (if keymap
                         (get-handler-from-keymap event keymap)
                         nil))))))
    (cond
      ;; Event occured and event handler is defined.
      ((and event
            (ev event))
       ;; return the handler
       (cdr (ev event)))
      ;; Event occured and a default event handler is defined.
      ;; If not even the default handler is defined, the event is ignored.
      ((and event
            (ev t))
       (cdr (ev t)))
      ;; If no event occured and the idle handler is defined.
      ;; The event is only nil when input input-blocking is nil.
      ((and (null event)
            (ev nil))
       (cdr (ev nil)))
      ;; If no event occured and the idle handler is not defined.
      (t
       nil))))

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

(defun function-lambda-list (fn)
  "Return the lambda list of the function object fn."
  #+abcl      (sys::arglist fn)
  #+allegro   (excl:arglist fn)
  #+ccl       (ccl:arglist fn)
  #+clisp     (ext:arglist fn)
  #+ecl       (ext:function-lambda-list fn)
  #+lispworks (lw:function-lambda-list fn)
  #+sbcl      (sb-introspect:function-lambda-list fn)
  #-(or abcl allegro ccl clisp ecl lispworks sbcl)
  (cadr (function-lambda-expression fn)))

(defun function-arity (fn)
  "Return the number of required arguments of the function fn."
  (let ((args (function-lambda-list fn)))
    (length (subseq args 0 (position-if (lambda (i)
                                          (member i lambda-list-keywords))
                                        args)))))

(defun apply-handler (handler object event args)
  "Determine and pass the correct arguments to each event handler.

The number of the arguments passed to the handler the arity (number of
required arguments) of the handler function.

This allows handlers with the following lambda lists to be defined:

(lambda () ...)
(lambda (object) ...)
(lambda (object event) ...)"
  (case (function-arity handler)
    ;; a thunk still has to handle args passed to run-event-loop, if there are any.
    (0 (apply handler args))
    (1 (apply handler object args))
    (2 (apply handler object event args))))

(defun handle-events (object args)
  "Read a single event from the user, lookup a handler and apply it."
  (let* ((event (get-wide-event object)))
    (if (event-key event)
        ;; t
        (progn
          (run-hook object 'before-event-hook)
          (handle-event object event args)
          (run-hook object 'after-event-hook))
        ;; The nil (idle) event should be defined directly in the object's bindings/keymap.
        ;; It has to be handled separately to allow handling of chained events.
        (let ((handler (get-event-handler object nil)))
          (when handler
            (apply-handler handler object event args))))))

(defgeneric handle-event (object event args)
  (:documentation
   "Get the event handler from the object, then apply it to the object.

If the handler for an event is a keymap object, the handler for the
next event will be looked up from that keymap. This allows for several
events to be chained together."))

(defmethod handle-event (object event args)
  "Default method. If the current keymap is not nil, lookup from that keymap."
  (with-slots ((map current-keymap)) object
    (let ((handler (get-event-handler (if map map object) (event-key event))))
      (when handler
        (cond
          ((eq handler t)
           ;; when the handler is just the boolean t, set the current keymap back to nil and ignore the event.
           ;; setting the default handler to t allows to exit a sub-keymap when a non-existing key is hit,
           ;; insted of blocking the event loop and waiting for an existing key to be hit.
           (setf map nil))
          ((typep handler 'keymap)
           ;; when the handler is another keymap, lookup the next event from that keymap.
           ;; For example: "C-x 3", ^X first returns a keymap, then we lookup 3 from that keymap
           (setf map handler))
          ((or (functionp handler)
               (symbolp handler))
           ;; when the handler is a function, the lookup has been successful,
           ;; set the map back to nil, to lookup the next event from the object's bindings or keymap.
           (setf map nil)
           ;; if args is nil, apply will call the handler with just object and event
           ;; this means that if we dont need args, we can define most handlers as two-argument functions.
           (apply-handler handler object event args)))))))

(defmethod handle-event ((object form) event args)
  "If a form can't handle an event, let the current element try to handle it."
  (with-slots ((map current-keymap)) object
    (let ((handler (get-event-handler (if map map object) (event-key event))))
      (if handler
          (cond
            ((eq handler t)
             (setf map nil))
            ((typep handler 'keymap)
             ;; when the handler is another keymap, lookup the next event from that keymap.
             (setf map handler))
            ((or (functionp handler)
                 (symbolp handler))
             ;; when the handler is a function, lookup the next event from the object's bindings/keymap.
             (setf map nil)
             ;; if args is nil, apply will call the handler with just object and event
             ;; this means that if we dont need args, we can define most handlers as two-argument functions.
             (apply-handler handler object event args)))
          ;; if there is no handler in the form keymap, pass the event to the current form element.
          (handle-event (current-item object) event args)))))

(defun exit-event-loop (object &optional args)
  "Associate this function with an event to exit the event loop."
  (declare (ignore args))
  (throw object :exit-event-loop))

(defmacro save-excursion (window &body body)
  "After executing body, return the cursor in window to its initial position."
  (let ((pos (gensym)))
    `(let ((,pos (cursor-position ,window)))
       ,@body
       (move ,window (car ,pos) (cadr ,pos)))))

(defmacro dogrid (((i y0 h)
                   (j x0 w)) &body body)
  "Loop over a 2D grid in row major order.

The grid is specified by position coordinates y0 x0 and dimensions height width.

Example: ((i 10 2) (j 10 3)) produces:

10 10, 10 11, 10 12
11 10, 11 11, 11 12"
  `(loop for ,i from ,y0 below (+ ,y0 ,h) do
     (loop for ,j from ,x0 below (+ ,x0 ,w) do
       ,@body)))
