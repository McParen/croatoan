(in-package :de.anvi.croatoan)

(defun hook (object hook function)
  "Add the function to the object's hook.

A hook is a list of functions to call on different pre-defined
occasions in order to customize the library's behavior.

The function should take the object as its only argument.

The functions are called to perform side effects, their return
values are discarded."
  (with-slots (hooks) object
    (if (assoc hook hooks)
        ;; if we already have a hook, append the function
        (push function (cdr (last (assoc hook hooks))))
        ;; if there is no such hook, add it to the alist
        (push (list hook function) hooks))))

(defun run-hook (object hook)
  "Run functions contained in the hook in the order they were added.

The following hooks are available:

before-event-hook: run before a non-nil event is handled by event-case
and run-event-loop.

after-event-hook: run after a non-nil event is handled by event-case
and run-event-loop."
  (with-slots (hooks) object
    (when (and hooks (assoc hook hooks))
      (dolist (fn (cdr (assoc hook hooks)))
        (funcall fn object)))))
