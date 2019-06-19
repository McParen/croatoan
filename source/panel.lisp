(in-package :de.anvi.croatoan)

;; panel
;; panel stack extension for curses
;; http://invisible-island.net/ncurses/man/panel.3x.html

;; defined in source/classes.lisp
;; (defparameter *window-stack* nil)

(defun raise (win)
  "Raise window one position in the stack."
  (when (not (eq win (car *window-stack*)))
    (let ((pos (position win *window-stack*)))
      (rotatef (nth (1- pos) *window-stack*)
               (nth     pos  *window-stack*)))))

(defun raise-to-top (win)
  "Raise window to the top of the window stack."
  (setf *window-stack* (cons win (remove win *window-stack*))))

(defun lower (win)
  "Lower window one position in the stack."
  (when (not (eq win (car (last *window-stack*))))
    (let ((pos (position win *window-stack*)))
      (rotatef (nth (1+ pos) *window-stack*)
               (nth     pos  *window-stack*)))))

(defun lower-to-bottom (win)
  "Lower window to the bottom of the window stack."
  (when (not (eq win (car (last *window-stack*))))
    (setf *window-stack* (append (remove win *window-stack*) (list win)))))

(defun empty-stack ()
  "Remove all windows from the stack."
  (setf *window-stack* nil))

(defun refresh-stack ()
  "Touch and refresh visible windows in the window stack."
  (if *window-stack*
      (progn
        (mapc #'(lambda (w)
                  (when (visible w)
                    (touch w)
                    (mark-for-refresh w)))
              (reverse *window-stack*))
        (refresh-marked))
      (error "refresh stack: stack empty")))

;; https://www.informatimago.com/develop/lisp/l99/p19.lisp
;; todo: what if count is longer than a list? use mod
(defun rotate (list count)
  (if (minusp count)
      (rotate list (+ (length list) count))
      (nconc (subseq list count) (subseq list 0 count))))
