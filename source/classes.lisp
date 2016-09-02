(in-package :de.anvi.croatoan)

;; (defclass name (superclasses) (slots) class-options)

;;; data types

;; will be abbreviated as "xchar" or "xch" in function parameters.
(defclass complex-char ()
  ((simple-char
    :initarg :simple-char
    :initform nil
    :type (or null character keyword)
    :accessor .simple-char
    :documentation "Lisp primitive character type, like #\a.")

   (attributes
    :initarg :attributes
    :initform '()
    :type (or null cons)
    :accessor .attributes
    :documentation "List of keywords denoting attributes.")

   (color-pair
    :initarg :color-pair
    :initform nil
    :type (or null cons)
    :accessor .color-pair
    :documentation "Two element list of keywords denoting a foreground and background color of the char."))

  (:documentation
   "A complex char consists of a simple char, a list of attribute keywords and a pair of color keywords."))

(defclass complex-string ()
  ((complex-char-array
    :initarg :complex-char-array
    :initform (make-array 0 :element-type 'complex-char :fill-pointer 0 :adjustable t)
    :type vector
    :accessor .complex-char-array
    :documentation "Lisp primitive string type."))

  (:documentation
   "A complex string consists of an array of complex characters."))

(defmethod initialize-instance :after ((cstr complex-string) &key string attributes color-pair)
  (with-slots (complex-char-array) cstr
    (when string
      (loop for char across string
         do (vector-push-extend
             (make-instance 'complex-char :simple-char char :attributes attributes :color-pair color-pair)
             complex-char-array)))))

(defclass window (fundamental-character-input-stream fundamental-character-output-stream)
  ((position
    ;; has to be a 2el-list so we can use 1 arg with setf.
    :initarg       :position
    :initform      '(0 0)
    :type          cons
    :documentation "The (y=row x=column) coordinate of the top left corner of the window.")

   (width
    :initarg       :width
    :initform      0
    :type          integer
    :documentation "The width (second, horizontal, x dimension) of the window.")

   (height
    :initarg       :height
    :initform      0
    :type          integer
    :documentation "The height (first, vertical, y dimension) of the window.")

   ;; has to be a 2el-list so we can use 1 arg with setf.
   (cursor-position
    :type          cons
    :documentation "The current cursor position coordinates in the form (y x).")

   (input-blocking
    :initarg       :input-blocking
    :initform      t
    :type          (or boolean integer)
    :documentation "Input mode: blocking (t), non-blocking (nil) or blocking duration in (positive integer) miliseconds.")

   (enable-fkeys
    :initarg       :enable-fkeys
    :initform      nil
    :type          boolean
    :documentation "Enable (t) or disable (nil) support for function keys.")

   (enable-scrolling
    :initarg       :enable-scrolling
    :initform      nil
    :type          boolean
    :documentation "Enable (t) or disable (nil) support for window scrolling.")

   (scrolling-region
    :initarg       :scrolling-region
    :initform      nil
    :type          (or null cons)
    :documentation "When scrolling is enabled, only scroll the window region from line y1 to y2 given as the list (y1 y2).")

   ;; IC / INS / Insert / Einfg key
   ;; this will only apply to lisps standard output functions.
   ;; ncurses functions add-char and insert-char can be used explicitely.
   (insert-enabled
    :initarg       :insert-enabled
    :initform      nil
    :type          boolean
    :accessor      .insert-enabled
    :documentation "Printing a new char will insert (t) it before the character under the cursor instead of overwriting (nil) it.")

   (background
    :initarg       :background
    :initform      nil
    :type          (or null complex-char)
    :documentation "A complex char to form the background of a window.")

   (attributes
    :initarg       :attributes
    :initform      nil
    :type          (or null cons)
    :documentation "A list of attribute keywords.")

   (color-pair
    :initarg       :color-pair
    :initform      nil
    :type          (or null cons)
    :documentation "A two element list of keywords denoting the foregreound and background color of text displayed in the window.")

   ;; TODO: Doesnt survive a "clear" command yet.
   (border
    :initarg       :border
    :initform      nil
    :type          boolean
    :documentation "Enable (t) or disable (nil, default) an initial border around a window.")

   (winptr
    :initform      nil
    :reader        .winptr
    :documentation "Pointer to a C/ncurses WINDOW structure after the window is created."))

  (:documentation "A curses window object as returned by newwin."))

(defclass screen (window)
  ((enable-colors
    :initarg       :enable-colors
    :initform      nil
    :type          boolean
    :documentation "Enable (t) or disable (nil) display of colors, if the terminal supports it.")

   (use-default-colors
    :initarg       :use-default-colors
    :initform      nil
    :type          boolean
    :documentation "Use (t) the default colors of the terminal, instead of the ncurses default white on black.")

   (cursor-visibility
    :initarg       :cursor-visibility
    :initform      t
    :type          boolean
    :documentation "Enable (t) or disable (nil) displaying a visible cursor.")

   (input-echoing
    :initarg       :input-echoing
    :initform      t
    :type          boolean
    :documentation "Enable (t) or disable (nil) echoing of chars during keyboard input.")

   (input-reading
    :initarg       :input-reading
    :initform      :unbuffered
    :type          keyword
    :documentation "Set whether typed characters will be line :buffered or directly passed as :unbuffered or :unbuffered-raw.")

   (closed-p
    :type          boolean
    :documentation "Check whether the screen has been closed, without a subsequent call to refresh to reactivate it."))

  (:documentation "Represents the main window created upon screen initialisation."))

(defclass sub-window (window)
  ((parent
    :initarg       :parent
    :initform      nil
    :type          window
    :documentation "The parent window which will contain the sub-window.")
   (relative
    :initarg       :relative
    :initform      nil
    :type          boolean
    :documentation "The position of the sub-window is relative to the parent window (t) or to the screen (nil, default).")
   (source
    :initarg       :source
    :initform      nil
    :type          (or null cons)
    :documentation "Position (y x) of the area of the parent window, which is mapped to the subwindow. By default it is identical to the position of the subwindow."))

  (:documentation  "A sub-window shares the memory and the display with and has to be contained within a parent window."))

(defclass decorated-window (window)
  ((sub-window
    :initarg       :sub-window
    :initform      nil
    :type          sub-window
    :reader        .sub-window
    :documentation "Content window, for example for menu contents.")
    
   (title
    :initarg       :title
    :initform      nil
    :accessor      .title
    :type          (or null string)
    :documentation "Title of the window displayed over the border."))

  (:documentation  "A decorated-window is a window consisting of a background window for a border and a content window."))

;; default size of ncurses menus is 16 rows, 1 col.
(defclass menu-window (decorated-window)
  ((items
    :initarg       :items
    :initform      nil
    :accessor      .items
    :type          (or null cons)
    ;; TODO: what about a list of symbols?
    :documentation "List of strings denoting menu entries.")

   (current-item
    :initform      0
    :type          integer
    :accessor      .current-item
    :documentation "Currently selected item's index.")

   (layout
    :initarg       :layout
    :initform      nil
    :type          (or null cons)
    :accessor      .layout
    :documentation "Layout (no-of-rows no-of-columns) of the items in the menu."))

  (:documentation  "A menu is a window providing a list of items to be selected by the user."))

(defmethod initialize-instance :after ((win menu-window) &key)
  (with-slots (winptr items width position sub-window) win
    ;; only for menu windows
    (when (eq (type-of win) 'menu-window)
      (setf winptr (%newwin (+ 2 (length items)) width (car position) (cadr position)))
      (setf sub-window
            (make-instance 'sub-window :parent win :height (length items) :width (- width 2) :position (list 1 1) :relative t))
      (setf (.background win)        (make-instance 'complex-char :color-pair '(:red :yellow)))
      (setf (.background sub-window) (make-instance 'complex-char :color-pair '(:red :yellow))) )))

#|
;; this will be called for both window and screen.
;; create a curses window when an instance is created.
(defmethod initialize-instance :after ((win window) &key)
  (with-slots (winptr cursor-visibility enable-colors input-echoing input-reading 
                      input-blocking enable-fkeys enable-scrolling height width position) win

    ;; different initialisations depending on the window type.
    ;; %initscr initializes a screen, %newwin initializes a window.
    ;; TODO: subwin, derwin, pad...
    ;; CAUTION: these initializations have to be done _before_ any other commands.
    ;; because of that we cant use call-next-method.
    (case (type-of win)
      ;; a screen is initialized when we start ncurses.
      (screen (progn
                (setf winptr (%initscr))
                (when enable-colors (%start-color))
                (if input-echoing (%echo) (%noecho))
                (set-input-reading winptr input-reading)
                (set-cursor-visibility cursor-visibility))) ;kernel.lisp
      ;; a window is initialized when we create a new window.
      (window (setf winptr (%newwin height width (car position) (cadr position)))))

    ;; the following settings have to be executed for all window types.
    (set-input-blocking winptr input-blocking)
    (%scrollok winptr enable-scrolling)
    (%keypad winptr enable-fkeys)))
|#

;; We cant do this because _all_ auxiliary methods are always used and combined.

(defmethod initialize-instance :after ((win window) &key)
  (with-slots (winptr height width position) win
    ;; just for WINDOW types
    (when (eq (type-of win) 'window)
      (setf winptr (%newwin height width (car position) (cadr position))))))

(defmethod initialize-instance :after ((scr screen) &key)
  (with-slots (winptr enable-colors use-default-colors cursor-visibility input-echoing input-reading input-blocking enable-fkeys enable-scrolling) scr
    ;; just for screen window types.
    (when (eq (type-of scr) 'screen)
      (setf winptr (%initscr))
      (when enable-colors
        (%start-color)
        (set-default-color-pair use-default-colors))
      (if input-echoing (%echo) (%noecho))
      (set-input-reading winptr input-reading)
      (set-cursor-visibility cursor-visibility))))

;; sub-window has to be contained within a parent window
;; touch parent before refreshing a subwindow
;; move also needs a method for subwindows.
;; Subwindows must be deleted before the main window can be deleted.
(defmethod initialize-instance :after ((win sub-window) &key)
  (with-slots (winptr parent height width position relative) win
    ;; just for SUB-WINDOW types
    (when (eq (type-of win) 'sub-window)
      (if relative
          (setf winptr (%derwin (slot-value parent 'winptr) height width (car position) (cadr position)))
          (setf winptr (%subwin (slot-value parent 'winptr) height width (car position) (cadr position)))))))

(defmethod initialize-instance :after ((win decorated-window) &key)
  (with-slots (winptr width height position sub-window) win
    ;; only for menu windows
    (when (eq (type-of win) 'decorated-window)
      (setf winptr (%newwin height width (car position) (cadr position)))
      (setf sub-window
            (make-instance 'sub-window :parent win :height (- height 2) :width (- width 2) :position (list 1 1) :relative t)))))

;; called after _all_ :after aux methods.
;; for all window types in the hierarchy.
(defmethod initialize-instance :around ((win window) &key)
  ;; before :before, :after and primary.
  (let ((result (call-next-method)))
    ;; after :before, :after and primary.
    (with-slots (winptr input-blocking enable-fkeys enable-scrolling border) win
      (set-input-blocking winptr input-blocking)
      (%scrollok winptr enable-scrolling)
      (when border (%box winptr 0 0))
      (%keypad winptr enable-fkeys))

    ;; why do we have to return the result in :around aux methods?
    result))

;; Accessors

(defgeneric .position (window))
(defmethod .position ((window window))
  (list (%getbegy (slot-value window 'winptr)) (%getbegx (slot-value window 'winptr))))
(defmethod .position ((win sub-window))
  (with-slots (winptr relative) win
    (if relative
        (list (%getpary winptr) (%getparx winptr))
        (list (%getbegy winptr) (%getbegx winptr)))))
(defgeneric (setf .position) (coordinates window))
(defmethod (setf .position) (coordinates (w window))
  (setf (slot-value w 'position) coordinates)
  (%mvwin (slot-value w 'winptr) (car coordinates) (cadr coordinates)))

;; "The screen-relative parameters of the window are not changed. 
;; This routine is used to display different parts of the parent 
;; window at the same physical position on the screen."
;;
;; "The mvderwin() function specifies a mapping of characters.
;; The function identifies a mapped area of the parent of the specified window"
(defgeneric (setf .source) (coordinates sub-window))
(defmethod (setf .source) (coordinates (w sub-window))
  (setf (slot-value w 'source) coordinates)
  (%mvderwin (slot-value w 'winptr) (car coordinates) (cadr coordinates)))

(defgeneric .width (window))
(defmethod .width ((window window))
  (%getmaxx (slot-value window 'winptr)))

(defgeneric .height (window))
(defmethod .height ((window window))
  (%getmaxy (slot-value window 'winptr)))

(defgeneric .cursor-position (window))
(defmethod .cursor-position ((window window))
  (list (%getcury (slot-value window 'winptr)) (%getcurx (slot-value window 'winptr))))

;; we can move the cursor by doing this, or by calling "move". 
;; both will use %wmove in the background.
;; note that incd and decf dont work.
(defgeneric (setf .cursor-position) (coordinates window))
(defmethod (setf .cursor-position) (coordinates (w window))
  (setf (slot-value w 'cursor-position) coordinates)
  (%wmove (slot-value w 'winptr) (car coordinates) (cadr coordinates)))

(defgeneric .cursor-visibility (window))
(defmethod .cursor-visibility ((screen screen))
  (slot-value screen 'cursor-visibility))
(defgeneric (setf .cursor-visibility) (status screen))
(defmethod (setf .cursor-visibility) (status (screen screen))
  (setf (slot-value screen 'cursor-visibility) status)
  (set-cursor-visibility status))

(defgeneric .input-blocking (window))
(defmethod .input-blocking ((window window))
  (slot-value window 'input-blocking))
(defgeneric (setf .input-blocking) (status window))
(defmethod (setf .input-blocking) (status (window window))
  (setf (slot-value window 'input-blocking) status)
  (set-input-blocking (slot-value window 'winptr) status))

(defgeneric .enable-fkeys (window))
(defmethod .enable-fkeys ((window window))
  (slot-value window 'enable-fkeys))
(defgeneric (setf .enable-fkeys) (status window))
(defmethod (setf .enable-fkeys) (status (window window))
  (setf (slot-value window 'enable-fkeys) status)
  (%keypad (slot-value window 'winptr) status))

(defgeneric .enable-scrolling (window))
(defmethod .enable-scrolling ((window window))
  (slot-value window 'enable-scrolling))
(defgeneric (setf .enable-scrolling) (status window))
(defmethod (setf .enable-scrolling) (status (window window))
  (setf (slot-value window 'enable-scrolling) status)
  (%scrollok (slot-value window 'winptr) status))

(defgeneric .scrolling-region (window))
(defmethod .scrolling-region ((window window))
  (slot-value window 'scrolling-region))
(defgeneric (setf .scrolling-region) (list window))
(defmethod (setf .scrolling-region) (list (window window))
  (setf (slot-value window 'scrolling-region) list)
  (%wsetscrreg (slot-value window 'winptr)
               (first  (slot-value window 'scrolling-region))
               (second (slot-value window 'scrolling-region))))
;; TODO: setf only when scrolling is enabled.

(defgeneric .input-echoing (screen))
(defmethod .input-echoing ((screen screen))
  (slot-value screen 'input-echoing))
(defgeneric (setf .input-echoing) (status screen))
(defmethod (setf .input-echoing) (status (screen screen))
  (setf (slot-value screen 'input-echoing) status)
  ;;(set-input-echoing status))
  (if status (%echo) (%noecho)))

(defgeneric .input-reading (screen))
(defmethod .input-reading ((screen screen))
  (slot-value screen 'input-reading))
(defgeneric (setf .input-reading) (status screen))
(defmethod (setf .input-reading) (status (screen screen))
  (set-input-reading screen status)
  (setf (slot-value screen 'input-reading) status))

(defgeneric .background (window))
(defmethod .background ((window window))
  (slot-value window 'background))
(defgeneric (setf .background) (char window &optional target))
(defmethod (setf .background) (char (window window) &optional (target :all-chars))
  (setf (slot-value window 'background) char)
  (set-background-char (slot-value window 'winptr) char target))
;; (setf (.background window :new-chars) xchar)
;; (setf (.background window :all-chars) xchar) = (setf (.background window) xchar)

;(defgeneric .attributes (window))
(defmethod .attributes ((window window))
  (slot-value window 'attributes))

;(defgeneric (setf .attributes) (attributes window))
(defmethod (setf .attributes) (attributes (window window))
  (let ((added (set-difference attributes (slot-value window 'attributes)))
        (removed (set-difference (slot-value window 'attributes) attributes)))
    (setf (slot-value window 'attributes) attributes)
    (add-attributes window added)
    (remove-attributes window removed)))
;; TODO use %wattron and %wattroff here.


;; we dont want to use set-attributes because it also overwrites the color attribute.
;; we want the attributes function to just handle attributes and leave the color alone.
;; to do that, we have to treat the attributes as a set and ignore set-attributes.

;; (defmethod (setf .attributes) (attributes (window window))
;;   (setf (slot-value window 'attributes) attributes)
;;   (set-attributes window attributes))

(defmethod .color-pair ((window window))
  (slot-value window 'color-pair))
(defmethod (setf .color-pair) (color-pair (window window))
  (setf (slot-value window 'color-pair) color-pair)
  (set-color-pair (slot-value window 'winptr) color-pair))



#|

classes and methods for the gray stream interface.

http://www.nhplace.com/kent/CL/Issues/stream-definition-by-user.html
http://www.gnu.org/software/clisp/impnotes/gray-gf-char-out.html

before including this, add trivial-gray-streams to asd or use the sb-gray stream package.

Up to now, window and screen had no superclasses, thus they were subclasses of standard-object.
Now, they will become bi-directional character streams.

That means that we dont need separate classes for defining streams, and that windows will have
a stream as a feature, windows now will _be_ specialized streams.

fundamental-character-output-stream
fundamental-character-input-stream
  window
    screen
    subwin

For the existing code, nothing will change. we will still be able to use all ncurses and croatoan
functions.

Actually, we still _need_ those functions to define the gray stream functions. But once defined,
we will not need add-char and add-string any more, we will simply use Lisp's format, read, print, etc.

|#

;;; Character Output stream

;;; Mandatory methods: stream-write-char, stream-line-column

(defmethod stream-write-char ((stream window) (ch character))
  (let ((code (char-code ch))
        (winptr (.winptr stream)))
    (if (.insert-enabled stream)
        (progn
          (%winsch winptr code)
          ;; move the cursor after the inserted character.
          (move-to stream :right))
        (if (and (>= code 0) (<= code 127))
            ;; ascii characters (0-127) are safe to use with addch.
            (%waddch winptr code)
            ;; for every other char, dont use waddch, but waddstr, so we can output unicode without add_wch.
            ;; to use waddstr, convert the lisp char into a one-character string before output.
            (%waddstr winptr (princ-to-string ch))))))

;; write-char, format ~C
(defmethod stream-write-char ((stream window) (ch complex-char))
  (%waddch (.winptr stream) (x2c ch)))

;; print, prin1, princ, format ~A, ~S
(defmethod print-object ((ch complex-char) (stream window))
  (%waddch (.winptr stream) (x2c ch)))

(defmethod print-object ((cstr complex-string) stream)
  (loop for ch across (.complex-char-array cstr)
     do (princ (.simple-char ch))))

(defmethod print-object ((cstr complex-string) (stream window))
  (loop for ch across (.complex-char-array cstr)
     do (add-char stream ch)))

;; Returns the column number where the next character would be written, i.e. the current y position
(defmethod stream-line-column ((stream window))
  (%getcurx (.winptr stream)))

;;; Non-mandatory methods

;; Default method uses repeated calls to stream-write-char
(defmethod stream-write-string ((stream window) (str string) &optional (start 0) (end nil))
  (%waddstr (.winptr stream) str))

;;; Character Input Stream

(defmethod stream-read-char ((stream window))
  (code-char (%wgetch (.winptr stream))))

;;(defmethod stream-read-char-no-hang ((stream window))
;; %wgetch wie bei read-char, nur muss input-blocking nil sein.

(defmethod stream-unread-char ((stream window) (ch character))
  (%ungetch (char-code ch)))

;; listen = read-char-no-hang + unread
;;(defmethod stream-listen ((stream window))

;; methods to close streams, and thus screen _and_ windows, since they are now streams.
;; end-screen = %endwin
;; delete-window = %delwin
;; see t07a
;; remember we cant use open to open a window. it works only for file streams.
;; we have to make-instance of a window or a screen.

(defmethod close ((stream window) &key abort)
  (declare (ignore abort))
  (%delwin (.winptr stream)))

(defmethod close ((stream sub-window) &key abort)
  (declare (ignore abort))
  (%delwin (.winptr stream)))

(defmethod close ((stream screen) &key abort)
  (declare (ignore abort))
  (%endwin))

(defmethod close ((stream decorated-window) &key abort)
  (declare (ignore abort))
  (%delwin (.winptr (.sub-window stream)))
  (%delwin (.winptr stream)))

;; although it is not a stream, we will abuse close to close a menu's window and subwindow, which _are_ streams.
(defmethod close ((stream menu-window) &key abort)
  (declare (ignore abort))
  (%delwin (.winptr (.sub-window stream)))
  (%delwin (.winptr stream)))

;; SBCL bug when specializing close on gray streams:
;; STYLE-WARNING:
;;    Generic function CLOSE clobbers an earlier FTYPE proclamation
;;    (FUNCTION (STREAM &KEY (:ABORT T)) (VALUES (MEMBER T) &OPTIONAL)) for the
;; same name with (FUNCTION (T &KEY (:ABORT T)) *).

(defgeneric .closed-p (s)
  (:documentation "Check whether the screen has been closed, without a subsequent call to refresh to reactivate it."))

(defmethod .closed-p ((s screen))
  (declare (ignore s))
  (%isendwin))
