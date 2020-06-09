(in-package :de.anvi.ansi-escape)

#|

From /usr/include/x86_64-linux-gnu/bits/termios.h

typedef unsigned char   cc_t;
typedef unsigned int    speed_t;
typedef unsigned int    tcflag_t;

#define NCCS 32

struct termios
  {
    tcflag_t c_iflag;           /* input mode flags */
    tcflag_t c_oflag;           /* output mode flags */
    tcflag_t c_cflag;           /* control mode flags */
    tcflag_t c_lflag;           /* local mode flags */
    cc_t c_line;                /* line discipline */
    cc_t c_cc[NCCS];            /* control characters */
    speed_t c_ispeed;           /* input speed */
    speed_t c_ospeed;           /* output speed */
  };

|#

(defun mode-type (mode)
  "Return the keyword designating the type of the terminal mode:

:input, :output, :control, :local, :character, :combination."
  (let ((iflags
          '(:IGNBRK :BRKINT :IGNPAR :PARMRK :INPCK :ISTRIP :INLCR :IGNCR
            :ICRNL :IUCLC :IXON :IXANY :IXOFF :IMAXBEL :IUTF8))
        (oflags
          '(:OPOST :OLCUC :ONLCR :OCRNL :ONOCR :ONLRET :OFILL :OFDEL :NLDLY
            :NL0 :NL1 :CRDLY :CR0 :CR1 :CR2 :CR3 :TABDLY :TAB0 :TAB1 :TAB2
            :TAB3 :BSDLY :BS0 :BS1 :FFDLY :FF0 :FF1 :VTDLY :VT0 :VT1 :XTABS))
        (cflags
          '(:CBAUD :B0 :B50 :B75 :B110 :B134 :B150 :B200 :B300 :B600 :B1200
            :B1800 :B2400 :B4800 :B9600 :B19200 :B38400 :CSIZE :CS5 :CS6
            :CS7 :CS8 :CSTOPB :CREAD :PARENB :PARODD :HUPCL :CLOCAL :CBAUDEX
            :B57600 :B115200 :B230400 :B460800 :B500000 :B576000 :B921600
            :B1000000 :B1152000 :B1500000 :B2000000 :B2500000 :B3000000
            :B3500000 :B4000000 :CIBAUD :CMSPAR :CRTSCTS))
        (lflags
          '(:ISIG :ICANON :XCASE :ECHO :ECHOE :ECHOK :ECHONL :NOFLSH :TOSTOP
            :ECHOCTL :ECHOPRT :ECHOKE :FLUSHO :PENDIN :IEXTEN :EXTPROC))
        (cc
          '(:VINTR :VQUIT :VERASE :VKILL :VEOF :VTIME :VMIN :VSWTC :VSTART
            :VSTOP :VSUSP :VEOL :VREPRINT :VDISCARD :VWERASE :VLNEXT :VEOL2))
        (combination
          '(:COOKED :RAW)))
    (cond ((member mode iflags) :iflag)
          ((member mode oflags) :oflag)
          ((member mode cflags) :cflag)
          ((member mode lflags) :lflag)
          ((member mode cc) :cc)
          ((member mode combination) :combination)
          (t nil))))

(defun mode-accessor (mode)
  "Return the appropriate accessor depending on the mode type."
  (case (mode-type mode)
    (:iflag 'sb-posix:termios-iflag)
    (:oflag 'sb-posix:termios-oflag)
    (:cflag 'sb-posix:termios-cflag)
    (:lflag 'sb-posix:termios-lflag)
    (:cc    'sb-posix:termios-cc)
    (t nil)))

(defun stream-fd (stream)
  "Return the posix file descriptor associated with the lisp stream."
  (let ((stream (typecase stream
                  ;; *standard-input*, *standard-output*, *terminal-io*, etc.
                  (synonym-stream (symbol-value (synonym-stream-symbol stream)))
                  ;; sb-sys:*stdin*, *stdout*, *tty*, etc.
                  (sb-sys:fd-stream stream))))
    ;; requires a fd-stream, not a synonym-stream
    (sb-posix:file-descriptor stream)))

;; ncurses:
;; cooked: ixon brkint parmrk
;; raw =   -cooked -icanon -isig -iexten
;; noraw =  cooked  icanon  isig  iexten 

;; stty:
;; raw    = -ignbrk -brkint -ignpar -parmrk  -inpck  -istrip  -inlcr -igncr  -icrnl  -ixon  -ixoff -icanon -opost -isig -iuclc -ixany -imaxbel -xcase min 1 time 0
;; cooked =          brkint  ignpar                   istrip                  icrnl   ixon          icanon  opost  isig                               eof ^D eol 0

(defparameter *combinations*
  '(((:raw t)
     :ignbrk nil :brkint nil :ignpar nil :parmrk nil :inpck nil :istrip nil
     :inlcr nil :igncr nil :icrnl nil :ixon nil :ixoff nil :icanon nil
     :opost nil :isig nil
     ;; not available in sb-posix:
     ;;:iuclc nil :ixany nil :imaxbel nil :xcase nil
     :iexten nil :csize nil :parenb nil :vmin 1 :vtime 0)
    ((:raw nil)
     :brkint t :ignpar t :istrip t :icrnl t :ixon t :icanon t :opost t
     :isig t :veol 0)
    ((:cooked t)
     :raw nil)
    ((:cooked nil)
     :raw t)))

(defun mode-combination (mode value)
  "If mode is a combination, return its contents as a plist."
  (cdr (assoc (list mode value) *combinations* :test #'equal)))

(defun set-termios-flag (termios mode value)
  "Take a termios struct, a flag and a value, update the termios struct in place."
  (let* (;; get the appropriate accessor for the flag
         (read-flag (fdefinition (mode-accessor mode)))
         (write-flag (fdefinition (list 'setf (mode-accessor mode))))         
         ;; get the current bitmask
         (old-flag (funcall read-flag termios))
         ;; get the new mode bitmask from the constants in sb-posix
         ;; TODO 200609: what to do with constants not available in sb-posix?
         (new-flag (symbol-value (find-symbol (symbol-name mode) 'sb-posix))))
    ;; write the new values to the termios struct
    ;; (funcall #'(setf acc) val obj) = (setf (acc obj) val)
    (funcall write-flag
             ;; the value for a flag can be t or nil
             (if value
                 ;; if t, add new flag to old flag
                 (logior old-flag new-flag)
                 ;; if nil, remove new flag from old
                 (logand old-flag (lognot new-flag)))
             termios)))

(defun set-termios-param (termios mode value)
  "Take a termios struct, a cc key and a value, update the termios struct in place."
  ;; the mode flags are 32bit unsigned integers
  ;; get the cc array
  (let ((cc-array (sb-posix:termios-cc termios))
        ;; the param name translates to an array index
        (cc-param (symbol-value (find-symbol (symbol-name mode) 'sb-posix))))
    (setf (aref cc-array cc-param) value)))

(defun update-termios (termios modes)
  "Update the settings in the termios struct in place with the values in modes plist."
  (loop for (mode value) on modes by #'cddr do
    (case (mode-type mode)
      (:combination
       (update-termios termios (mode-combination mode value)))
      ((:iflag :oflag :cflag :lflag)
       (set-termios-flag termios mode value))
      (:cc
       (set-termios-param termios mode value)))))

;; Examples: t06, t07
(defun set-tty-mode (stream &rest modes)
  "Enable or disable one or more tty modes."
  (let* ((stream (if (eq stream t) *standard-input* stream))
         (fd (stream-fd stream))
         ;; get the current attributes in a termios object
         (termios (sb-posix:tcgetattr fd)))
    ;; Update the termios struct in place.
    (update-termios termios modes)
    ;; write the new termios struct to the fd of the tty now.
    (sb-posix:tcsetattr fd sb-posix:tcsanow termios)))
