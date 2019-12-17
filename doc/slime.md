# Working with swank/slime

Since ncurses is single threaded, it is only ever allowed to call
ncurses functions from the thread that initialized the screen. As this
thread must be the one controlling the terminal, ncurses must only
be called from the REPL prompt directly. With a recent enough croatoan
version, slime/swank does work though, but it requires a bit of extra
work. The following is an example of how one would work with croatoan
using slime/swank. First, a bit of boiler plate:

```
;; load croatoan and swank
(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload '(:croatoan :swank)))

;; Defining a package is always a good idea
(defpackage #:scratch
  (:use #:cl)
  (:export #:main))

(in-package #:scratch)

;; The main screen, we need a global so that we can access it from
;; slime
(defparameter *scr* nil)

;; Main entry point, to be called from the terminal thread, it will
;; initialize the screen and enter the event loop
(defun main ()
  (croatoan:with-screen (scr
                         ;; Set input blocking to 100 ms. This _must_
                         ;; be set for swank to work, otherwise
                         ;; get-event will block and croatoan only
                         ;; polls the job queue when a key is pressed.
                         :input-blocking 100
                         ;; Do not override the swank debugger hook,
                         ;; as we want to enter the slime debugger in
                         ;; emacs when a error occurs.
                         :bind-debugger-hook-p nil)
    ;; Set *scr* to the initilized scr so that we can access it form
    ;; the swank thread and then enter the event-loop.
    (croatoan:run-event-loop (setf *scr* scr))))

;; Initialize swank, setting dont-close will prevent the server from
;; shutting down in case slime disconnects. The default port is 4005,
;; one can specify a different one with :port.
(swank:create-server :dont-close t)

;; Initialize screen and enter the event loop
(main)
```

This should be saved in a file and then loaded directly from sbcl (or
any other CL Implementation known to work with croatoan). For sbcl
this would be (when saved to `scratch.lisp`):

```
sbcl --load scratch.lisp
```

The user should now see a blank terminal window with the cursor placed
at the top left corner. Time to connect to it from slime, form within
emacs run:

```
M-x slime-connect
```

Once started, we need to change into the right package, do this by
entering (inside the SLIME repl):

```
CL-USER> (in-package :scratch)
```

To now run something inside the terminal thread one must use
`CROATOAN:SUBMIT`:

```
SCRATCH> (croatoan:submit (croatoan:add-string *scr* "Hey!"))
```

And voila, thats the basics of working with croatoan from slime!
The next thing one might want to do is set some keybindings, for
example, bind `c` to clear the screen:

```
SCRATCH> (croatoan:submit
           (croatoan:bind *scr* #\c (lambda (win event)
                                      (croatoan:clear *scr*))))
```

And `q` to quit the event loop:

```
SCRATCH> (croatoan:submit
           (croatoan:bind *scr* #\q 'croatoan:exit-event-loop))
```

There is one caveat though, `*STANDARD-OUTPUT*` differs between the
swank and the terminal thread. It is set to the swank output stream
(which prints everything to the slime repl) inside the swank thread,
and bound to the terminal stdin/stdout inside the terminal
thread. Hence doing the following, would mess up the terminal screen
instead of printing to the slime repl:

```
SCRATCH> (croatoan:submit (format t "Hellou!~%"))
```

What we need is a way to refer to the swank stream, which can be easily
achieved by setting another global to whatever stream
`*STANDARD-OUTPUT*` points to inside the slime repl:

```
SCRATCH> (defparameter *swank-output* *standard-output*)
```

If we now run:

```
SCRATCH> (croatoan:submit (format *swank-output* "Hellou!~%"))
```

We should see `Hellou!` printed in the slime repl.

This is quite helpful, especially when debugging something within the
terminal thread:

```
SCRATCH> (croatoan:submit
           (croatoan:bind *scr* :resize
                          (lambda (win event)
                            (format *swank-output* "Terminal resized to width: ~a, height: ~a~%"
                                    (croatoan:width *scr*)
                                    (croatoan:height *scr*)))))
```
