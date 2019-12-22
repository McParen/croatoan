(in-package :croatoan)

;;; High-level Lisp wrappers

;; old bindings not yet replaced, deprecated, not exported, not loaded, do not use.

;; all defaults are FALSE

(defun redraw-on-clear (window flag)
  "If flag is t, when refresh is called after clear, it will redraw the screen from scratch."
  (%clearok window flag))
;; for now, to use clearok with curscr, use %clearok directly.

(defun insert-delete-line (window flag)
  "If flag is t, use the hardware insert/delete line feature of a terminal, if the terminal supports it.

It is disabled by default."
  (%idlok window flag))

(defun insert-delete-char (window flag)
  "If flag is t, use the hardware insert/delete char feature of a terminal, if the terminal supports it.

It is enabled by default."
  (%idcok window flag))

(defun immediately-refresh (window flag)
  "If flag is t, any change to a window will automatically call refresh.

It is disabled by default, since it can degrade performance."
  (%immedok window flag))

(defun leave-cursor-on-refresh (window flag)
  "If flag is t, don't move the cursor back to the position before refresh.

It is disabled by default."
  (%leaveok window flag))

(defun enable-scrolling (window flag)
  "Enables and disables window scrolling.

If flag is t, when the curses moves below the bottom line of a window
or scrolling region, the window/region is scrolled.

If flag is nil, the cursor is left on the bottom line."
  (%scrollok window flag))

(defun set-scrolling-region (window top-margin bottom-margin)
  "Set the margins of a scrolling region for a window if scrolling is enabled for that window."
  (%wsetscrreg window top-margin bottom-margin))

(defun newline-translation (flag)
  "If status is t, enable translation of RET to NL on input, and NL to RET and LF on output.

It is enabled by default."
  (if flag
      (%nl)
      (%nonl)))
