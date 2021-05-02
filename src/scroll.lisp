(in-package :de.anvi.croatoan)

;;; scroll
;;; scroll a curses window
;;; http://invisible-island.net/ncurses/man/curs_scroll.3x.html

(defun scroll (window &optional (n 1))
  "Scroll the window up or down for n lines relative to its contents.

If n is positive, move the contents up relative to the window.
The line i becomes line i-n. The window view moves n lines down.

If n is negative, move the contents down relative to the window.
The line i becomes line i+n. The window view moves n lines up.

Initial   Scroll up  Scroll down
            n = 1      n = -1

              1
+-----+    +-----+    +-----+
|  1  |    |  2  |    |     |
|  2  |    |  3  |    |  1  |
|  3  |    |     |    |  2  |
+-----+    +-----+    +-----+
                         3

The content lines that leave the scrolled window aren't buffered and
can not be retrieved when moving back, they are lost. (If you need to
scroll up and down without losing lines, consider using pad windows
instead of simple windows.)

The cursor position in the window is not changed when the contents are
scrolled."
  (ncurses:wscrl (winptr window) n))
