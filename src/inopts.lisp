(in-package :de.anvi.croatoan)

;; nodelay FALSE = getch blocking
;; nodelay TRUE  = getch non-blocking.
;; halfdelay 5   = waits for 5/10 seconds.

;; halfdelay is turned off by nocbreak.

;; terminal input modes:
;;
;;                  canonical             non canonical
;;                  line buffered         character based
;;                  ctrl chars processed  ^C,^S,^Q,^D processed  no ctrl chars processed
;;                  cooked                cbreak                 raw
;;
;; buffering        t                     nil                    nil
;; control          t                     t                      nil

;;           | cooked | cbreak | raw
;; ----------+--------+--------+-----
;; buffering | t      | nil    | nil
;; ----------+--------+--------+-----
;; control   | t      | t      | nil

;; The combination echo+getch should not be used during buffered input
(defun set-input-mode (input-buffering process-control-chars)
  (if input-buffering
      ;; to turn on buffering, turn off cbreak or raw
      (if process-control-chars (ncurses:nocbreak) (ncurses:noraw))
      ;; to turn off buffering, turn on cbreak or raw
      (if process-control-chars (ncurses:cbreak) (ncurses:raw))))

(defun set-input-blocking (winptr status)
  "Set the window input blocking behavior.

Possible values are t, nil and a blocking duration in (positive integer) miliseconds."
  (cond ((eq status t) (ncurses:wtimeout winptr -1))
        ((eq status nil) (ncurses:wtimeout winptr 0))
        ((and (typep status 'integer) (plusp status))
         (ncurses:wtimeout winptr status))
        (t (error "set-input-blocking error: possible blocking states: t, nil, delay in miliseconds"))))
