(in-package :de.anvi.ansi-escape.test)
  
(defun t01 ()
  (erase)
  (cursor-position 0 0)
  (princ "0")
  (cursor-position 2 2)
  (princ "1")
  (cursor-position 5 15)
  (princ "test")
  (cursor-position 10 15)
  (force-output)
  (let ((a (read-line)))
    (cursor-position 12 15)
    (princ a)
    (force-output)))

(defun t02 ()
  (print "normal")
  (sgr 1)
  (print "bold")
  (sgr 4)
  (print "bold underline")
  (sgr 7)
  (print "bold underline reverse")
  (sgr 22)
  (print "underline reverse")
  (sgr 24)
  (print "reverse")
  (sgr 27)
  (print "normal")
  (sgr 1 4 7)
  (print "bold underline reverse")
  (sgr 0)
  (print "normal")
  (force-output))

(defun t03 ()
  "Display the 256 color palette."
  (clear)
  (loop for i from 0 to 255 do
    (sgr 48 5 i)
    (princ #\space))
  (terpri)
  (sgr 0)
  (loop for i from 0 to 255 do
    (sgr 38 5 i)
    (princ "X"))
  (sgr 0)
  (force-output)
  (sleep 3)
  (ris)
  (force-output))

(defun t04 ()
  "Hide and show the cursor."
  (princ "Cursor visible:")
  (force-output)
  (sleep 2)
  (terpri)
  (princ "Cursor invisible:")
  (hide-cursor)
  (force-output)
  (sleep 2)
  (terpri)
  (princ "Cursor visible:")
  (show-cursor)
  (force-output)
  (sleep 2))

(defun t05 ()
  "Switch to and back from the alternate screen buffer."
  (princ "Normal screen buffer. ")
  (force-output)
  (sleep 2)

  (save-cursor-position)
  (use-alternate-screen-buffer)
  (clear)
  (princ "Alternate screen buffer.")
  (force-output)
  (sleep 2)

  (use-normal-screen-buffer)
  (restore-cursor-position)
  (princ "Back to Normal screen buffer.")
  (force-output)
  (sleep 1))

(defun t06 ()
  "Set individual termios flags to enable raw and disable echo mode.

Enabling raw mode allows read-char to return immediately after a key is pressed.

In the default cooked mode, the entry has to be confirmed by pressing enter."
  (set-tty-mode t :ignbrk nil
                  :brkint nil
                  :parmrk nil
                  :istrip nil
                  :inlcr  nil
                  :igncr  nil
                  :icrnl  nil
                  :ixon   nil
                  :opost  nil
                  :echo   nil
                  :echonl nil
                  :icanon nil
                  :isig   nil
                  :iexten nil
                  :csize  nil
                  :parenb nil
                  :vmin 1
                  :vtime 0)  
  (erase)
  (cursor-position 1 1)
  (force-output)
  (let ((a (read-char)))
    (cursor-position 3 1)
    (princ a)
    (force-output))
  
  (set-tty-mode t :echo t
                  :brkint t
                  :ignpar t
                  :istrip t
                  :icrnl t
                  :ixon t
                  :opost t
                  :isig t
                  :icanon t
                  :veol 0))

(defun t07 ()
  "Use combination modes that consist of several individual flags.

Cooked and raw are opposite modes. Enabling cooked disbles raw and vice versa."
  (set-tty-mode t :cooked nil)
  (erase)
  (cursor-position 1 1)
  (force-output)
  (let ((a (read-char)))
    (cursor-position 3 1)
    (princ a)
    (force-output))
  (set-tty-mode t :raw nil))

(defun t08 ()
  "Why doesnt calling the stty utility work?"
  (uiop:run-program "stty raw -echo" :ignore-error-status t)
  (erase)
  (cursor-position 1 1)
  (force-output)
  (let ((a (read-char)))
    (cursor-position 2 1)
    (princ a)
    (force-output))
  (uiop:run-program "stty -raw echo" :ignore-error-status t))
