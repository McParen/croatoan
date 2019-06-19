(in-package :de.anvi.croatoan)

;; takes integers and returns a winptr.
(defun new-window (&key height width begin-y begin-x)
  (%newwin height width begin-y begin-x))
;; Example: (new-window :height 18 :width 17 :begin-y 4 :begin-x 19)

;; TODO: one single command to delete a window and to end screen, or better a method that specializes on window type.
;; takes a window object.
;; TODO: look up what delwin returns.
(defun delete-window (window)
  (%delwin (winptr window)))

;; takes a winptr.
(defun move-window (window y x)
  (%mvwin window y x))

;; takes a winptr, returns a winptr.
(defun new-subwindow (parent-window &key height width begin-y begin-x relative)
  (if relative
      (%derwin parent-window height width begin-y begin-x)
      (%subwin parent-window height width begin-y begin-x)))

;; This is NOT a move function, despite the name. You still move a subwindow with mvwin. 
;; "Moving subwindows is allowed, but should be avoided."
;; mvderwin changes the source area of a subwin, but not the output area.
;; "This routine is used to display different parts of the parent window at the same physical position on the screen."
(defun move-subwindow (window parent-y parent-x)
  (%mvderwin window parent-y parent-x))

(defun duplicate-window (window)
  (%dupwin window))

;;; TODOs

;; Do something with wsyncup, syncok, wcursyncup, wsyncdown.
;; For now, I wont wrap them, because I have no idea how to test them.
;; I doubt anybody will ever use them.

