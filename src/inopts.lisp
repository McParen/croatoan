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
      (if process-control-chars (%nocbreak) (%noraw))
      ;; to turn off buffering, turn on cbreak or raw
      (if process-control-chars (%cbreak) (%raw))))

;; Ported to clos, used in clos.
(defun set-input-blocking (window status)
  "Set window input blocking behavior.

Possible values are t, nil and a blocking duration in (positive integer) miliseconds."
  (cond ((eq status t) (%wtimeout window -1))
        ((eq status nil) (%wtimeout window 0))
        ((and (typep status 'integer) (plusp status))
         (%wtimeout window status))
        (t (error "possible blocking states: t, nil, delay in miliseconds"))))

;; Not used in clos because too simple. obsolete.
(defun set-input-echoing (flag)
  "Set whether chars will be echoed on input."
  (if flag
      (%echo)
      (%noecho)))

;; Not used in clos because too simple. obsolete.
(defun set-enable-fkeys (window flag)
  "If flag is t, bind function keys to known codes when returned by get-char.

If flag is nil, F keys will be system-dependent multi-character escape codes."
  (%keypad (winptr window) flag))

;; Obscure functions I never used before:

(defun flush-on-interrupt (window flag)
  (%intrflush window flag))

(defun enable-8bit-char-input (window flag)
  (%meta window flag))

(defun io-queue-flush (flag)
  (if flag
      (%qiflush)
      (%noqiflush)))

;; if it would work at all, which it doesnt,
;; it would work only for (function-keys win t)
(defun escape-sequence-delay (window flag)
  (%notimeout window flag))

(defun type-ahead-fd (fd)
  (%typeahead fd))

;;; TODOs

;; [X] do not mix cbreak and raw. use either the one or the other.
;; [ ] for now, we consider only global optins. work in a window parameter as well.
