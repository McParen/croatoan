(in-package :de.anvi.croatoan)

(defclass dialog-window (menu-window)
  ;; this has to be a pad, so we can scroll it if the message is large.
  ;; TODO: check how scrolling would work with a subwindow.
  ((message-pad
    :initform      nil
    :type          (or null pad)
    :reader        message-pad
    :documentation "Passive content window, for example for menu descriptions.")

   ;; check how message is called in the dialog program.
   (message-text
    :initarg       :message-text
    :initform      nil
    :accessor      message-text
    :type          (or null string)
    :documentation "Optional message text to describe the choices in the menu below.")
   ;; make shorter slot names
   (message-height
    :initarg       :message-height
    :initform      0
    :reader        message-height
    :type          integer
    :documentation "Max number of lines reserved for the optional message text.")

   (message-pad-coordinates
    :initform      nil
    :reader        message-pad-coordinates
    :type          (or null cons)
    :documentation "List of four coordinates where to refresh/display the message pad: min-y min-x max-y max-x."))

  (:documentation  "A dialog is a decorated menu with a title, a message and items."))

;; also see menu.lisp / draw dialog-window
(defmethod initialize-instance :after ((win dialog-window) &key color-pair center)
  (with-slots (winptr items height width (y position-y) (x position-x) sub-window borderp layout max-item-length current-item-mark
                      fgcolor bgcolor message-pad message-text message-height message-pad-coordinates) win
    ;; only for dialog windows
    (when (eq (type-of win) 'dialog-window)
      (let ((padding (if borderp 1 0))
            (item-length (+ (length current-item-mark) max-item-length)))
        ;; if no layout was given, use a horizontal list (1 n).
        (unless layout (setf layout (list (length items) 1)))

        ;; if height and width are not given as initargs, they will be calculated,
        ;; according to the number of rows +/- border, and _not_ maximized like normal windows.
        (unless height
          (setf height (+ (if (zerop padding) 3 2) message-height (* 2 padding) (car layout))))
        (unless width
          (setf width (+ (if (zerop padding) 2 4) (* (cadr layout) item-length))))

        ;; if the key center was given, calculate position automatically, even if it was explicitely given.
        (when center
          (setf y (- (round (/ ncurses:LINES 2)) (round (/ height 2)))
                x (- (round (/ ncurses:COLS  2)) (round (/ width  2)))))

        (setf winptr (ncurses:newwin height width y x))
        (setf sub-window
              (make-instance 'sub-window
                             :parent win :height (car layout)
                             :width (* (cadr layout) item-length)
                             :position (list (+ 2 message-height padding) (+ padding 1)) :relative t))

        ;; if there is space reserved for a message, and the message is provided,
        ;; initialize a pad and set the background color.
        (when (and message-text (> message-height 0))
          (setf message-pad (make-instance 'pad :height message-height :width (- width (+ 2 (* 2 padding)))))
          (setf message-pad-coordinates
                (list (+ (1+ padding) y)         ;screen-min-y
                      (+ (1+ padding) x)         ;screen-min-x
                      (+ (+ 2 y) message-height) ;screen-max-y
                      (+ (+ 2 x (- width 4)))))  ;screen-max-x
          (when color-pair
            (setf (background message-pad) (make-instance 'complex-char :color-pair color-pair)))
          (format message-pad message-text))

        (cond ((or fgcolor bgcolor)
               (set-color-pair winptr (color-pair win))
               (setf (color-pair sub-window) (color-pair win)
                     (background win) (make-instance 'complex-char :color-pair (color-pair win))
                     (background sub-window) (make-instance 'complex-char :color-pair (color-pair win)) ))
              ;; when a color-pair is passed as a keyword
              (color-pair
               ;; set fg and bg, pass to ncurses
               (setf (color-pair win) color-pair
                     (color-pair sub-window) color-pair
                     (background win) (make-instance 'complex-char :color-pair color-pair)
                     (background sub-window) (make-instance 'complex-char :color-pair color-pair)))) ))))

(defmethod close ((stream dialog-window) &key abort)
  (declare (ignore abort))
  (ncurses:delwin (winptr (message-pad stream)))
  (ncurses:delwin (winptr (sub-window stream)))
  (ncurses:delwin (winptr stream)))

(defclass msgbox (form-window)
  ((message
    :initarg       :message
    :initform      nil
    :type          (or null string)
    :documentation
    "Message to display to the user.")

   (msg-area
    :initform      nil
    :type          (or null textarea)
    :documentation
    "An inactive textarea containing the user message. Since the area is inactive, the
    message can not be scrolled. The size is calculated automatically from the msgbox
    dimensions.")

   (ok-button
    :initform      nil
    :type          (or null button)
    :documentation
    "The OK button below the message allows the user to accept and exit the dialog."))

  (:default-initargs
   :enable-function-keys t
   :input-blocking t
   :border t)

  (:documentation
   "A msgbox is a form-window presenting the user a message and an OK button to accept it."))

(defmethod initialize-instance :before ((msgbox msgbox) &key center)
  (with-slots (items msg-area ok-button) msgbox
    (setf msg-area (make-instance 'textarea :position '(1 1) :active nil))
    (setf ok-button (make-instance 'button :name :ok-button :title "  OK  "))
    (setf (callback ok-button) 'accept)
    ;; the elements list has to be assembled :before the primary form initialization method
    ;; otherwise the :after method for form doesnt work
    (setf items (list msg-area ok-button))))

(defmethod initialize-instance :after ((msgbox msgbox) &key center (message-wrap t))
  (with-slots (winptr height width (y position-y) (x position-x) sub-window window message msg-area ok-button) msgbox
    (when (eq (type-of msgbox) 'msgbox)
      ;; The default size of a msgbox is half the height, 2/3 the width of the screen
      (unless height (setf height (round (/ ncurses:LINES 2))))
      (unless width (setf width (round (* ncurses:COLS 2/3))))
      ;; If the center keyword is t, it replaces the default window position '(0 0).
      (when center
        (setf y (- (round (/ ncurses:LINES 2)) (round (/ height 2)))
              x (- (round (/ ncurses:COLS  2)) (round (/ width  2)))))
      (setf winptr (ncurses:newwin height width y x))
      (setf sub-window (make-instance 'sub-window :parent msgbox :height (- height 2) :width (- width 2)
                                                  :position (list 1 1) :relative t :enable-function-keys t))
      (setf window sub-window)
      (setf (height msg-area) (- height 5))
      (setf (width msg-area) (- width 4))
      (setf (widget-position ok-button) (list (- height 3)
                                               (- (floor width 2)
                                                  ;; why do we need 4 here?
                                                  (floor (+ 4 (length (title ok-button))) 2))))
      (when message
        (setf (value msg-area) (if message-wrap
                                   (wrap-string message (1- (width msg-area)))
                                   message))))))
