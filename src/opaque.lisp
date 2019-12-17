(in-package :croatoan)

;;; opaque
;;; curses window properties
;;; http://invisible-island.net/ncurses/man/curs_opaque.3x.html

;;; C prototypes

;; bool is_cleared(const WINDOW *win);
;; bool is_idcok(const WINDOW *win);
;; bool is_idlok(const WINDOW *win);
;; bool is_immedok(const WINDOW *win);
;; bool is_keypad(const WINDOW *win);
;; bool is_leaveok(const WINDOW *win);
;; bool is_nodelay(const WINDOW *win);
;; bool is_notimeout(const WINDOW *win);
;; bool is_pad(const WINDOW *win);
;; bool is_scrollok(const WINDOW *win);
;; bool is_subwin(const WINDOW *win);
;; bool is_syncok(const WINDOW *win);
;; WINDOW *wgetparent(const WINDOW *win);
;; int wgetscrreg(const WINDOW *win, int *top, int *bottom);

;;; Low-level C functions

(defcfun ("is_cleared"   %is_cleared)   :boolean (win window))
(defcfun ("is_idcok"     %is_idcok)     :boolean (win window))
(defcfun ("is_idlok"     %is_idlok)     :boolean (win window))
(defcfun ("is_immedok"   %is_immedok)   :boolean (win window))
(defcfun ("is_keypad"    %is_keypad)    :boolean (win window))
(defcfun ("is_leaveok"   %is_leaveok)   :boolean (win window))
(defcfun ("is_nodelay"   %is_nodelay)   :boolean (win window))
(defcfun ("is_notimeout" %is_notimeout) :boolean (win window))
(defcfun ("is_pad"       %is_pad)       :boolean (win window))
(defcfun ("is_scrollok"  %is_scrollok)  :boolean (win window))
(defcfun ("is_subwin"    %is_subwin)    :boolean (win window))
(defcfun ("is_syncok"    %is_syncok)    :boolean (win window))

(defcfun ("wgetparent" %wgetparent) window (win window))
(defcfun ("wgetscrreg" %wgetscrreg) :int (win window) (top (:pointer :int)) (bottom (:pointer :int)))

;;; High-level Lisp wrappers

(defun redraw-on-clear-p (window)
  "If t, the next refresh will redraw the screen from scratch."
  (%is_cleared window))

(defun insert-delete-char-p (window)
  "If t, the hardware insert/delete char feature of a terminal will be used, if the terminal supports it."
  (%is_idcok window))

(defun insert-delete-line-p (window)
  "If t, the hardware insert/delete line feature of a terminal will be used, if the terminal supports it."
  (is_idlok window))

(defun immediately-refresh-p (window)
  "If t, any change to a window will automatically call refresh."
  (%is_immedok window))

(defun function-keys-p (window)
  "If t, function keys will be recognized when returned by get-char."
  (%is_keypad window))

(defun leave-cursor-on-refresh-p (window)
  "If t, don't move the cursor back to the position before refresh."
  (%is_leaveok window))

;; Vorsicht: we don't use %nodelay for blocking settings. But maybe it still works somehow.
(defun input-blocking-p (window)
  "If t, reading is blocking."
  (not (%is_nodelay window)))

;; Hint: doesnt seem to work.
(defun escape-sequence-delay (window)
  "If t, do not set a delay after the escape key."
  (%is_notimeout window))

(defun pad-p (window)
  "If t, the window is a pad."
  (%is_pad window))

(defun enable-scrolling-p (window)
  "If t, scrolling is enabled."
  (%is_scrollok window))

(defun subwindow-p (window)
  "If t, the window is a subwindow."
  (%is_subwin window))

(defun touch-parent-windows-p (window)
  "If t, areas in parent windows will be touched when window is changed."
  (%is_syncok window))

(defun get-parent-window (window)
  "If window is a subwindow, return its parent window."
  (%wgetparent window))

(defun get-scrolling-region (window)
  "Return a cons pair with the top and bottom margin of the scrolling region."
  (let ((t-ptr (foreign-alloc :int))
        (b-ptr (foreign-alloc :int)))
    ;; populate the pointers with values.
    (%wgetscrreg window t-ptr b-ptr)
    ;; dereference the int pointers.
    (let ((top    (mem-ref t-ptr :int))
          (bottom (mem-ref t-ptr :int)))
      ;; free the allocated memory.
      (foreign-free t-ptr)
      (foreign-free b-ptr)
      ;; return two color integers as a cons pair.
      (cons top bottom))))

;;; TODOs

;;; NOTES

;; These functions which return properties set in the WINDOW
;; structure, allowing it to be compiled as opaque.