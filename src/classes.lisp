(in-package :de.anvi.croatoan)

#|

event     +--> mouse-event

keymap

grid

collection

component +--> widget ---+--> window  +--> screen
          |              |            |
          |              |            +--> sub-window
          |              |            |
          |              |            +--> extended-window
          |              |            |
          |              |            +--> pad --> sub-pad
          |              |            |
          |              |            +--> panel
          |              |
          |              +--> element +--> field
          |              |            |
          |              |            +--> button
          |              |            |
          |              |            +--> label
          |              |            |
          |              |            +--> checkbox --> menu-item
          |              |            |
          |              |            +--> menu +--> menu-window --> dialog-window
          |              |                      |
          |              |                      +--> menu-panel
          |              |                      |
          |              |                      +--> checklist
          |              |
          |              +--> layout
          |
          +--> form --------> form-window
|#

(defclass event ()
  ((key
    :initarg       :key
    :initform      nil
    :reader        event-key
    :type          (or null keyword character)
    :documentation "Character or keyword representing a function key, terminal :resize or mouse event.")

   (code
    :initarg       :code
    :initform      nil
    :reader        event-code
    :type          (or null integer)
    :documentation "Integer code representing the character or function key as returned by ncurses."))

  (:documentation  ""))

(defclass mouse-event (event)
  ((position-y
    :initarg       :y
    :initform      nil
    :type          (or null integer)
    :documentation "The y coordinate (row) of mouse event.")

   (position-x
    :initarg       :x
    :initform      nil
    :type          (or null integer)
    :documentation "The x coordinate (column) of the mouse event.")

   (modifiers
    :initarg       :modifiers
    :initform      nil
    :reader        event-modifiers
    :type          (or null cons)
    :documentation "A list containing any combination of :ctrl, :shift and :alt"))

  (:documentation  "The class represents the ncurses MEVENT struct as returned by getmouse."))

(defclass keymap ()
  ((bindings
    :initarg       :bindings
    :initform      nil
    :type          (or null cons)
    :accessor      bindings
    :documentation "Alist of events and handler functions."))

  (:documentation  "A keymap contains an alist of events as keys and event handlers or chained keymaps as values."))

;; initialize instance takes a plist initarg and converts it to an alist.
(defmethod initialize-instance :after ((keymap keymap) &key bindings-plist)
  (with-slots (bindings) keymap
    ;; when the alist is not provided, but the plist, convert the plist to an alist
    ;; the plist is easier to provide when there is a large number of bindings.
    (when (and (null bindings) bindings-plist)
      (setf bindings (plist2alist (convert-strings bindings-plist))))))

(defclass component ()
  ((name
    :initarg       :name
    :initform      nil
    :reader        name
    :type          (or null symbol keyword)
    :documentation
    "Optional unique name by which the object can be identified and accessed.
    If the title is t, the name is displayed instead of the title.")

   (title
    :initarg       :title
    :initform      nil
    :accessor      title
    :type          (or boolean string)
    :documentation
    "Title of the object to be displayed to the user. If nil, no title is displayed.
    If the title is t, the name is displayed.")

   (style
    :initarg       :style
    :initform      nil
    :type          (or null cons)
    :accessor      style
    :documentation
    "A style is a plist with properties relevant to the rendering of an object.

The basic style corresponds to the slots of a complex-char:
:fgcolor, :bgcolor, :attributes and :simple-char

Currently, the compound styles for the following objects can be set:

form:
A plist of default styles for each form element type.

menu (styles of the menu items)
:foreground, :background, :selected-foreground, :selected-background

The default style of the selected item is the attribute :reverse,
and nil for other items.

button, checkbox:
:foreground and :selected-foreground.

field, textarea:
:foreground, :background, :selected-foreground, :selected-background.")

   (bindings
    :initarg       :bindings
    :initform      nil
    :type          (or null cons)
    :accessor      bindings
    :documentation
    "Alist of events (characters, keywords or integers) as keys and
    handler functions as values. Used by the run-event-loop function.")

   (keymap
    :initarg       :keymap
    :initform      nil
    :type          (or null symbol keyword keymap)
    :accessor      keymap
    :documentation
    "Keymap containing the key bindings to be used by run-event-loop instead
    of the object's own bindings. If using an instance-local binding isn't
    sufficient, we can create an external keymap and reference it in the object.")

   (current-keymap
    :initform      nil
    :type          (or null keymap)
    :documentation
    "If not nil, this is a pointer to the keymap that should be used for the
    lookup of the next event handler instead of the widgets bindings and keymap.
    Used as an utility only in the handle-event method.")

   (hooks
    :initform      nil
    :type          (or null cons)
    :accessor      hooks
    :documentation
    "Alist of hooks registered with the object."))

  (:documentation "Base class for all croatoan objects."))

;; called first for all objects other than windows
;; if it is not a window (form or form element), set the timeout of its associated window
;; we only init if the rate is not the default nil
;; because the default window blocking already is t = rate nil
(defmethod initialize-instance :after ((obj component) &key frame-rate)
  (unless (typep obj 'window)
    (when frame-rate
      (setf (frame-rate obj) frame-rate))))

(defclass widget (component)
  ((position-y
    :initarg       :y
    :initform      0
    :type          integer
    :documentation "The y coordinate (row) of the top left corner.")

   (position-x
    :initarg       :x
    :initform      0
    :type          integer
    :documentation "The x coordinate (column) of the top left corner.")

   (width
    :initarg       :width
    :initform      nil
    :type          (or null integer)
    :documentation "The width (second, horizontal, x dimension), number of columns.")

   (height
    :initarg       :height
    :initform      nil
    :type          (or null integer)
    :documentation "The height (first, vertical, y dimension), number of rows.")

   (initial-dimension-hint
    :initarg       :initial-dimension-hint
    :initform      nil
    :type          (or null integer)
    :documentation
"Internal variable to hold the initial dimension hint for layouts whose geometry
is recalculated.

It can be either a width or height, which is interpreted by whether it is part
of a row or column layout."))

  (:documentation
   "A widget is a visible component defined by its dimensions (h w) and displayed
at a given position (y x).

It represents a visible region on the screen, either a window, or a form element
like a button or input field."))

(defmethod initialize-instance :after ((obj widget) &key position dimensions geometry)
  (with-slots (height width (y position-y) (x position-x) initial-dimension-hint) obj
    ;; the keyword position overrides the keywords y and x
    (when position
      (setf y (car position)
            x (cadr position)))
    ;; the keyword dimensions overrides width and height
    (when dimensions
      (setf height (car dimensions)
            width  (cadr dimensions)))
    ;; geometry overrides y, x, width and height
    (when geometry
      (setf y (nth 0 geometry)
            x (nth 1 geometry)
            height (nth 2 geometry)
            width (nth 3 geometry)))
    ;; if _neither_ width nor height are given, the hint stays nil.
    ;; if _only_one_ of width and height has been given, save it as the initial dimension hint.
    ;; if _both_ width and height are given, the hint stays nil. (window is free)
    ;; if _both_ are given, and the hint is given, keep the hint. (window given as plist)
    (cond ((and height (null width))
           (setf initial-dimension-hint height))
          ((and (null height) width)
           (setf initial-dimension-hint width)))))

(defclass window (widget fundamental-character-input-stream fundamental-character-output-stream)
  (;; has to be a 2el-list so we can use 1 arg with setf.
   ;; TODO 200724 we actually do not need this slot.
   ;; we only need accessors which access ncurses functions getcuryx and move.
   ;; we need other slots like width and height for :after methods
   (cursor-position
    :type          cons
    :initform      '(0 0)
    :documentation "The current cursor position coordinates in the form (y x).")

   (input-blocking
    :initarg       :input-blocking
    :initform      t
    :type          (or boolean integer)
    :documentation "Input mode: blocking (t), non-blocking (nil) or blocking duration in (positive integer) miliseconds.")

   (function-keys-enabled-p
    :initarg       :enable-function-keys
    :initform      nil
    :type          boolean
    :documentation "Enable (t) or disable (nil) support for function keys.")

   (scrolling-enabled-p
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
   (insert-mode-p
    :initarg       :insert-mode
    :initform      nil
    :type          boolean
    :accessor      insert-mode-p
    :documentation "Printing a new char will insert (t) it before the character under the cursor instead of overwriting it (nil, default).")

   (background
    :initarg       :background
    :initform      nil
    :type          (or null complex-char)
    :documentation
    "Sets a complex char with its attributes and colors as the default style of unrendered text and empty cells of the window.

Complex chars with pre-existing attributes and colors are not changed.

If the background char with attributes is set after the foreground attributes, the background attributes override the
foreground attributes. To prevent this, set the background char with attributes before the foreground attributes.")

   (attributes
    :initarg       :attributes
    :initform      nil
    :type          (or null cons)
    :documentation "A list of keywords denoting attributes of new characters added to a window.")

   (fgcolor
    :initarg       :fgcolor
    :initform      nil
    :type          (or null keyword integer list)
    :documentation "A keyword denoting the foreground color of new characters added to the window.")

   (bgcolor
    :initarg       :bgcolor
    :initform      nil
    :type          (or null keyword integer list)
    :documentation "A keyword denoting the background color of new characters added to the window.")

   (borderp
    :initarg       :border
    :initform      nil
    :type          boolean
    :reader        borderp
    :documentation "Draw (t) or don't draw (nil, default) an initial border around a window. Not redrawn automatically.")

   (stackedp
    :initarg       :stacked
    :initform      nil
    :type          boolean
    :documentation "If stacked is t, the window is added to the global window stack, so overlapping windows can be refreshed in the stacking order.")

   (visiblep
    :initarg       :visible
    :initform      t
    :type          boolean
    :accessor      visiblep
    :documentation "If visible is nil, do not refresh the stacked window when refreshing the window stack.")

   (winptr
    :initform      nil
    :reader        winptr
    :documentation "Pointer to a C/ncurses WINDOW structure after the window is created."))

  (:documentation "A curses window object as returned by newwin."))

#|
:around - most specific first (call-next-method)

:before - most specific first
              screen
            window
          widget

primary - most specific first (call-next-method)

:after  - most specific LAST = all window type after methods start here
          widget
            window
              screen
|#

(defmethod initialize-instance :after ((win window) &key color-pair)
  (with-slots (winptr height width (y position-y) (x position-x) fgcolor bgcolor background) win
    ;; for ALL window types
    ;; called BEFORE more specific after methods

    ;; just for WINDOW types
    (when (eq (type-of win) 'window)
      ;; the default value 0 for width or height is "to the end of the screen".
      (unless width (setf width 0))
      (unless height (setf height 0))
      (setf winptr (ncurses:newwin height width y x))

      ;; fg/bg should not be passed together with the color-pair keyword.
      (cond ((or fgcolor bgcolor)
             (set-color-pair winptr (color-pair win)))
            (color-pair
             ;; set fg and bg, pass to ncurses
             (setf (color-pair win) color-pair)))
      (when background (setf (background win) background)))))

;; called after _all_ :after aux methods.
;; for all window types in the hierarchy.
;; this has to be called after other aux methods because the windows (winptrs) have to be created first.
(defmethod initialize-instance :around ((win window) &key frame-rate)
  ;; before :before, :after and primary.
  (let ((result (call-next-method))) ; this calls the primary method, which creates the winptr
    ;; after :before, :after and primary.
    (with-slots (winptr input-blocking function-keys-enabled-p scrolling-enabled-p borderp stackedp style) win
      ;; the frame rate argument, if available, takes precedence over input blocking
      ;; because they access the same ncurses timeout
      ;; a non-nil frame-rate always implies non-blocking input with a delay
      (if frame-rate
          (setf (frame-rate win) frame-rate)
          (set-input-blocking winptr input-blocking))
      (ncurses:scrollok winptr scrolling-enabled-p)
      (ncurses:keypad winptr function-keys-enabled-p)
      (when stackedp (stack-push win *main-stack*))
      ;; if there is a style parameter, it overrides any other arg (fgcolor, bgcolor, background, attributes)
      (when style
        (setf (style win) style))
      ;; We have to check for a panel here in :around, after :after because the winptr has to be created in
      ;; :after first. Set the border for all types except the (main) panel window, panels set their own border.
      (unless (typep win 'panel)
        (when borderp (ncurses:box winptr 0 0))))

    ;; why do we have to return the result in :around aux methods?
    result))

(defclass screen (window)
  ((colors-enabled-p
    :initarg       :enable-colors
    :initform      nil
    :type          boolean
    :documentation "Enable (t) or disable (nil) display of colors, if the terminal supports it.")

   (use-terminal-colors-p
    :initarg       :use-terminal-colors
    :initform      nil
    :type          boolean
    :documentation
    "Use (t) colors set by the terminal (named :terminal) as the default instead of the ncurses default white on black.")

   (cursor-visible-p
    :initarg       :cursor-visible
    :initform      t
    :type          boolean
    :documentation "Enable (t) or disable (nil) the display of a visible cursor.")

   (input-echoing-p
    :initarg       :input-echoing
    :initform      t
    :type          boolean
    :documentation "Enable (t) or disable (nil) echoing of chars during keyboard input.")

   (input-buffering-p
    :initarg       :input-buffering
    :initform      nil
    :type          boolean
    :documentation
    "Set whether typed characters will be returned immediately when they are typed (nil, default)
    or buffered until Return is typed (t).")

   (process-control-chars-p
    :initarg       :process-control-chars
    :initform      t
    :type          boolean
    :documentation
    "If input-buffering is nil, set whether some control characters like ^C, ^S, ^Q, ^D will be processed (t, default)
    or passed directly to the program (nil).
    When input-buffering is t, control chars are always processed and this option has no effect.")

   (newline-translation-enabled-p
    :initarg       :enable-newline-translation
    :initform      t
    :type          boolean
    :documentation
    "If t (default), the #\return character (CR ^M \r) is automatically translated to newline (NL) on input,
     and NL is translated to CR LF on output.
     NL is the standard, system independent, portable way to end a line.
     It can be either #\linefeed (LF ^J \n) on Linux, carriage #\return on MacOS, CRLF \r\n on Windows.
     Setting newline translation to nil is necessary to be able to detect the #\return key.")

   (closed-p
    :type          boolean
    :documentation "Check whether the screen has been closed, without a subsequent call to refresh to reactivate it."))

  (:documentation "Represents the main window created upon screen initialisation."))

(defmethod initialize-instance :after ((scr screen) &key color-pair)
  (with-slots (winptr colors-enabled-p use-terminal-colors-p cursor-visible-p input-echoing-p
                      function-keys-enabled-p newline-translation-enabled-p fgcolor bgcolor
                      scrolling-enabled-p background input-buffering-p process-control-chars-p) scr
    ;; just for screen window types.
    (when (eq (type-of scr) 'screen)
      ;; pass the environment locale to the ncurses C library
      ;; has to be done explicitely since sbcl 2.0.3.
      (ncurses:setlocale ncurses:+LC-ALL+ "")
      (setf winptr (ncurses:initscr))
      (when colors-enabled-p
        (if (ncurses:has-colors)
            (progn
              (ncurses:start-color)
              (set-default-color-pair use-terminal-colors-p))
            ;; TODO 201031 signaling an error does not cleanly exit ncurses
            (error "initialize-instance screen: This terminal does not support colors.")))

      (cond ((or fgcolor bgcolor)
             (set-color-pair winptr (color-pair scr)))
            (color-pair
             ;; set fg and bg, pass to ncurses
             (setf (color-pair scr) color-pair)))

      (when background (setf (background scr) background))
      (if newline-translation-enabled-p (ncurses:nl) (ncurses:nonl))
      (if input-echoing-p (ncurses:echo) (ncurses:noecho))
      (set-input-mode input-buffering-p process-control-chars-p)
      (set-cursor-visibility cursor-visible-p))))

(defclass sub-window (window)
  ((parent
    :initarg       :parent
    :initform      nil
    :type          (or null window)
    :documentation "The parent window which will contain the sub-window.")

   (relativep
    :initarg       :relative
    :initform      nil
    :type          boolean
    :documentation "The position of the sub-window is relative to the parent window (t) or to the screen (nil, default).")

   (source-position
    :initarg       :source-position
    :initform      nil
    :type          (or null cons)
    :documentation
    "Position (y x) of the area of the parent window, which is mapped to the subwindow.
    By default it is identical to the position of the subwindow."))

  (:documentation  "A sub-window shares the memory and the display with and has to be contained within a parent window."))

;; sub-window has to be contained within a parent window
;; touch parent before refreshing a subwindow
;; move also needs a method for subwindows.
;; Subwindows must be deleted before the main window can be deleted.
(defmethod initialize-instance :after ((win sub-window) &key)
  (with-slots (winptr parent height width (y position-y) (x position-x) relativep) win
    ;; just for SUB-WINDOW types
    (when (eq (type-of win) 'sub-window)
      ;; the default value 0 for width or height is "to the end of the screen".
      (unless width (setf width 0))
      (unless height (setf height 0))
      (if relativep
          (setf winptr (let ((val (ncurses:derwin (slot-value parent 'winptr) height width y x)))
                         (if (cffi:null-pointer-p val)
                             ;; may also be null if the parent window passed is null
                             (error "Subwindow could not be created. Probably too big and not contained in the parent window.")
                             val)))
          (setf winptr (ncurses:subwin (slot-value parent 'winptr) height width y x))))))

;; TODO: add a method that draws the title and can be called with call-next-method
;; as of now we have to write the title for every class derived from extended-window
(defclass extended-window (window)
  ((border-width
    :initform      0 ; default should be 0 and only set to 1 if :border is t.
    :type          integer
    :reader        border-width
    :documentation "")

   ;; TODO 210110 rename to something else, like "content"
   (sub-window
    :initform      nil
    :type          (or null sub-window)
    :reader        sub-window
    :documentation "Subwindow for content, for example a menu or a form."))

  (:documentation
   "An extended window contains a subwindow for the content.

It can optionally be surrounded with space for a border or a title.

Since the window and the sub share the space, they also implicitely
will share any attributes.

The primary use of extended windows are menu-windows and form-windows,
which have to explicitely target the sub-window to write their contents
by setting the :window slot to the sub-window.

Except for those special cases you probably want to use a panel, which
behaves more like a simple window."))

(defmethod initialize-instance :after ((win extended-window) &key)
  (with-slots (winptr width height (y position-y) (x position-x) sub-window border-width) win
    ;; only for extended-window
    (when (eq (type-of win) 'extended-window)
      ;; first make the main window
      (setf winptr (ncurses:newwin height width y x))
      ;; then make the content sub-window
      (setf sub-window (make-instance 'sub-window :parent win
                                                  :height (- height (* border-width 2))
                                                  :width (- width (* border-width 2))
                                                  :position (list border-width border-width)
                                                  :relative t)))))

(defclass panel (window)
  (;; TODO 210701 use default initargs instead of a separate slot
   (border-width
    :initarg       :border-width
    :initform      1
    :type          integer
    :documentation "")

   (border-win
    :initform      nil
    :type          (or null window)
    :documentation "A border window for decorations like title, border, scroll bar.")

   (shadowp
    :initarg       :shadow
    :initform      nil
    :type          boolean
    :documentation "Draw (t) or don't draw (nil, default) a shadow behind the panel.")

   (shadow-win
    :initform      nil
    :type          (or null window)
    :documentation "A window to provide the shadow."))

  (:documentation
   "A panel is a window displaying the main content decorated by a background window for
the border, title and other decorations and a second window providing a shadow.

The border and the shadow are displayed outside of the content window, so they change the
absolute position and dimensions of the panel."))

;; make :position the position of the whole panel, not just of the main window.
;; how would it be possible to delay winptr creation, to introduce lazyness???
(defmethod initialize-instance :after ((win panel) &key)
  (with-slots (winptr width height (y position-y) (x position-x) borderp border-win shadowp shadow-win border-width style) win
    ;; only for panels
    (when (eq (type-of win) 'panel)
      (let* ((y1 y) ; main
             (x1 x)
             (y2 (- y1 border-width)) ; border
             (x2 (- x1 border-width))
             (h2 (+ height (* border-width 2)))
             (w2 (+ width (* border-width 2)))
             (y3 (+ y2 1)) ; shadow
             (x3 (+ x2 1)))
        ;; the content window gets (h w), the aux windows are enlarged
        (setf winptr (ncurses:newwin height width y1 x1))
        ;; check if border is t, if nil, do not create the border window
        (when borderp
          (setf border-win (make-instance 'window :height h2 :width w2 :position (list y2 x2) :border t)))
        ;; check if shadow is t, if nil, do not create the shadow window
        (when shadowp
          (setf shadow-win (make-instance 'window :height h2 :width w2 :position (list y3 x3))))))))

(defmethod refresh ((win panel) &rest args)
  (declare (ignore args))
  (with-slots (borderp border-win shadow-win shadowp) win
    (when shadowp
      (touch shadow-win)
      (refresh shadow-win))
    (when borderp
      (touch border-win)
      (refresh border-win))
    (touch win)
    (ncurses:wrefresh (winptr win))))

(defmethod close ((win panel) &key abort)
  (declare (ignore abort))
  (with-slots (borderp border-win shadow-win shadowp) win
    (when borderp
      (ncurses:delwin (winptr border-win)))
    (when shadowp
      (ncurses:delwin (winptr shadow-win)))
    (ncurses:delwin (winptr win))))

;; :foreground
;;   :fgcolor
;;   :bgcolor
;;   :attributes
;; :background (passed automatically as a plist to make-instance)
;;   :fgcolor
;;   :bgcolor
;;   :attributes
;;   :simple-char

(defmethod (setf style) (style (win window))
  (setf (slot-value win 'style) style)
  (let ((fg (getf style :foreground))
        (bg (getf style :background)))
    ;; the bg char is set before the fg because else the bg attributes override
    ;; the fg attributes
    (when bg
      (setf (background win) (apply #'make-instance 'complex-char bg)))
    (when fg
      (let ((fgcolor (getf fg :fgcolor))
            (bgcolor (getf fg :bgcolor))
            (attr    (getf fg :attributes)))
        (setf (color-pair win) (list fgcolor bgcolor)
              (attributes win) attr)))))

(defmethod (setf style) (style (win extended-window))
  ;; first set the style of the parent
  (call-next-method)
  ;; we have to explicitely set the fg of the sub-window
  ;; the bg is inherited, but the fg for some strange reason isnt
  (let ((fg (getf style :foreground)))
    (when fg
      (let ((fgcolor (getf fg :fgcolor))
            (bgcolor (getf fg :bgcolor))
            (attr    (getf fg :attributes)))
        (setf (color-pair (sub-window win)) (list fgcolor bgcolor)
              (attributes (sub-window win)) attr)))))

(defmethod (setf style) (style (win panel))
  ;; first set the style of the window
  (call-next-method)
  ;; then set the style of the border and shadow windows
  (with-slots (borderp border-win shadowp shadow-win style) win
    (when (and borderp
               (getf style :border))
      (setf (style border-win) (getf style :border)))
    (when (and shadowp
               (getf style :shadow))
      (setf (style shadow-win) (getf style :shadow)))))

;; if a position is given during make-instance, it can be simply ignored.
;; or we can check for position and signal an error.
(defclass pad (window)
  ()
  (:documentation "A pad is a window without a specified position on screen, which is specified dynamically during refresh."))

(defmethod initialize-instance :after ((win pad) &key)
  (with-slots (winptr height width) win
    ;; just for a pad window
    (when (eq (type-of win) 'pad)
      (setf winptr (ncurses:newpad height width)))))

(defclass sub-pad (pad)
  ((parent
    :initarg       :parent
    :initform      nil
    :type          (or null pad)
    :documentation "The parent pad which will contain the sub-pad."))
  (:documentation  "A sub-pad shares the memory and the display with a parent pad and has to be contained within it."))

;; sub-pads always use positions relative to parent pads
(defmethod initialize-instance :after ((win sub-pad) &key)
  (with-slots (winptr parent height width (y position-y) (x position-x)) win
    ;; just for a sub-pad window
    (when (eq (type-of win) 'sub-pad)
      (setf winptr (ncurses:subpad (slot-value parent 'winptr) height width y x)))))

(defclass element (widget)
  ((value
    :initarg       :value
    :initform      nil
    :accessor      value
    ;; TODO 200809 we have to comment this out because it clashes with the type of menu-item
    ;; why does a latter type spec not override this earlier type spec?
    ;;:type          (or symbol keyword string number)
    :documentation "Value of the element, mostly the result of the form editing.")

   ;; we need this to draw selected and other elements with different styles.
   ;; this has to be toggled at the same time as current-element of a form
   (selectedp
    :initform      nil
    :type          boolean
    :accessor      selectedp
    :documentation "Flag denoting whether the element is currently selected in a form.")

   (activep
    :initarg       :active
    :initform      t
    :type          boolean
    :accessor      activep
    :documentation "If t (default), the element can be selected. Used to prevent labels from being selected.")

   (parent
    :initform      nil
    :type          (or null form)
    :accessor      parent
    :documentation "Parent form of the element. Added to every element upon the initialization of the form.")

   ;; elements do not necessarily have to have an associated window, only when they are used stand-alone.
   ;; when they are part of a form, we can reference the window associated with the parent form.
   (window
    :initarg       :window
    :initform      nil
    :type          (or null window)
    :documentation "If the element is not part of a form, a window can also be associated with the stand-alone element."))

  (:documentation "An element of a form, like a field or button."))

(defclass button (element)
  ((callback
    :initarg       :callback
    :initform      nil
    :accessor      callback
    :type          (or null symbol function)
    :documentation "Callback function called when the button is activated."))

   ;; for every button we need a general activation event like newline or space,
   ;; and we need a hotkey for every button alone, like :f4 :f5, etc.
   ;; they hotkeys would then be global keys added to the form, not to a button.

  (:default-initargs :keymap 'button-map)
  (:documentation "An element that can call a function by pressing enter (or in future, with a mouse click)."))

;; TODO: the reference should not be an object, but the element name
;; for that we need a function that takes a name and returns the object
(defclass label (element)
  ((reference
    :initarg       :reference
    :initform      nil
    :accessor      reference
    :type          (or null symbol keyword string)
    :documentation
    "If the name of a reference element is specified, the element's title will be displayed instead of the label's.
    If a title for the label is explicitely provided, it overrides the title of the reference element.")

   ;; TODO: width, style should be element slots and not label slots because the label doesnt do anything with them.

   (width
    :initarg       :width
    :initform      nil
    :type          (or null integer)
    :accessor      width
    :documentation "The width of the label.")

   ;; TODO 201222 add default initargs instead of duplicating the slot
   (activep
    :initform      nil
    :documentation "Labels are by default not active and can not be selected when cycling through the elements."))

  (:documentation "A single-line string displayed at the specified position."))

(defclass checkbox (element)
  ((checkedp
    :initarg       :checked
    :initform      nil
    :accessor      checkedp
    :type          boolean
    :documentation "t if the checkbox has been checked, nil if it hasn't."))

  (:default-initargs :keymap 'checkbox-map)
  (:documentation "A boolean element that can be checked (t) or unchecked (nil)"))

(defclass node ()
  ((parent
    :initarg       :parent
    :initform      nil
    :accessor      parent
    ;;:type          (or null window)
    :documentation "Pointer to the parent object.")

   (children
    :initarg       :children
    :initform      nil
    :accessor      children
    :type          (or null cons)
    :documentation "List of children."))

  (:documentation "Base class for objects that can be organized in a tree, like layouts."))

(defclass collection (node)
  ((current-item-number
    :initform      nil
    :accessor      current-item-number
    :type          (or null integer)
    :documentation "Number (row-major mode) of the currently selected item, nil if the list is empty."))

  (:documentation "Base for objects that contain a list of other objects that can be selected."))

(defmethod initialize-instance :after ((obj collection) &key)
  (with-slots (items) obj
    ;; if items has been passed as an initarg, init the current item pointer
    (when items
      (setf (current-item-number obj) 0))))

(defmethod items ((obj collection))
  "The children of a collection can be accessed as items."
  (slot-value obj 'children))

(defmethod (setf items) (items (obj collection))
  "The children of a collection can be accessed as items."
  (setf (slot-value obj 'children) items))

(defmethod initialize-instance :after ((obj collection) &key items)
  (with-slots (children current-item-number) obj
    ;; the children slot can be initialized with the :items initarg.
    (when items
      (setf children items))
    ;; if the children have been initialized, initialize the counter.
    ;; if the children are added later, the counter has to be initialized manually.
    (when children
      (setf current-item-number 0))))

(defgeneric current-item (collection))

(defmethod current-item ((obj collection))
  "Return the current object from the collection."
  (with-slots (children current-item-number) obj
    (when children
      (nth current-item-number children))))

(defgeneric select-previous-item (collection)
  (:documentation "")
  (:method (collection)
    (with-accessors ((items items) (current-item-number current-item-number)) collection
      (setf current-item-number (mod (1- current-item-number) (length items))))))

(defgeneric select-next-item (collection)
  (:documentation "")
  (:method (collection)
    (with-accessors ((items items) (current-item-number current-item-number)) collection
      (setf current-item-number (mod (1+ current-item-number) (length items))))))

(defgeneric select-first-item (collection)
  (:documentation "")
  (:method (collection)
    (with-accessors ((items items) (current-item-number current-item-number)) collection
      (setf current-item-number 0))))

(defgeneric select-last-item (collection)
  (:documentation "")
  (:method (collection)
    (with-accessors ((items items) (current-item-number current-item-number)) collection
      (setf current-item-number (1- (length items))))))

(defclass form (component collection)
  ((window
    :initarg       :window
    :initform      nil
    :type          (or null window)
    :accessor      window
    :documentation "Window created separately and then associated with the form."))

  (:default-initargs :keymap 'form-map)
  (:documentation
   "A form is a collection of elements like fields, textareas, checkboxes, menus and buttons."))

(defmethod initialize-instance :after ((form form) &key layout elements)
  (with-slots (children current-item-number) form
    ;; For backward compatibility, if a list of elements has been passed, store it in the children slot.
    (when elements
      (setf children elements))
    ;; When a layout has been passed, calculate the positions of the elements.
    (when layout
      (calculate-positions layout)
      (setf children (flatten-items layout)))
    ;; Check that only elements are passed to a form.
    (when (notevery (lambda (x) (typep x 'element)) children)
      (error "Form init: All items passed to a form have to be element objects."))
    (if children
        (progn
          ;; Initialize the current element as the first active element from the passed elements list.
          ;; we have to set the current element before we can change it with select-previous-element and select-next-element
          (setf current-item-number (position-if #'activep children))
          ;; mark the current element selected so it can be highlighted
          (setf (slot-value (current-item form) 'selectedp) t)
          ;; set the parent form slot of every element.
          (loop for element in children
                do (setf (slot-value element 'parent) form)))
        ;; if a list of elements was not passed, signal an error.
        (error "Form init: A list of elements is required to initialize a form."))))

(defmethod elements ((obj form))
  (slot-value obj 'children))

(defmethod (setf elements) (elements (obj form))
  (setf (slot-value obj 'children) elements))

;; TODO: use both window-free forms and form-windows, window-free menus und menu-windows.
(defclass form-window (extended-window form)
  ()
  (:documentation ""))

(defmethod initialize-instance :after ((win form-window) &key)
  (with-slots (winptr height width (y position-y) (x position-x) sub-window borderp border-width window) win
    ;; only for form windows
    (when (eq (type-of win) 'form-window)
      (setf border-width (if borderp 1 0))

      ;; TODO: this isnt form specific, this should be part of window initialization
      ;; TODO 200612 see window and sub-window init
      (setf winptr (ncurses:newwin height width y x))

      ;; TODO: this isnt form specific, this should be part of panel initialization
      (setf sub-window (make-instance 'sub-window :parent win
                                                  :height (- height (* border-width 2))
                                                  :width (- width (* border-width 2))
                                                  :position (list border-width border-width)
                                                  :relative t
                                                  :enable-function-keys t))

      ;; set the sub of the panel to be the associated window of the form
      ;; the form will be drawn to the associated window, i.e. to the sub
      (setf window sub-window))))

;; Accessors

;;; generic functions and default methods

(defgeneric widget-position (object)
  (:documentation "Return the position (y x) of the top left corner of the widget.")
  (:method (object)
    "The default method returns the values of the y and x slots in a two-element list.

If both y and x slots are nil, nil is returned instead of (nil nil)."
    (with-slots (position-y position-x) object
      (if (and (null position-y)
               (null position-x))
          nil
          (list position-y position-x)))))

(defgeneric (setf widget-position) (position object)
  (:documentation "Set the position (y x) of the top left corner of the widget.")
  (:method (position object)
    "The default method sets the values of the y and x slots from a two-element list.

If position is nil, both y and x are set to nil."
    (with-slots (position-y position-x) object
      (if (null position)
          (setf position-y nil
                position-x nil)
          (setf position-y (car position)
                position-x (cadr position))))))

(defgeneric position-y (object)
  (:documentation "Return the y position (row) of the top left corner of the widget.")
  (:method (object)
    (slot-value object 'position-y)))

(defgeneric position-x (object)
  (:documentation "Return the x position (column) of the top left corner of the widget.")
  (:method (object)
    (slot-value object 'position-x)))

(defgeneric (setf position-y) (y object)
  (:documentation "Set the y position (row) of the top left corner of the widget.")
  (:method (y object)
    (with-slots (position-y) object
      (setf position-y y))))

(defgeneric (setf position-x) (x object)
  (:documentation "Set the x position (column) of the top left corner of the widget.")
  (:method (x object)
    (with-slots (position-x) object
      (setf position-x x))))

;;; specialized methods

(defmethod widget-position ((win window))
  (with-slots (winptr) win
    (list (ncurses:getbegy winptr)
          (ncurses:getbegx winptr))))

(defmethod widget-position ((win sub-window))
  (with-slots (winptr relativep) win
    (if relativep
        (list (ncurses:getpary winptr) (ncurses:getparx winptr))
        (list (ncurses:getbegy winptr) (ncurses:getbegx winptr)))))

(defmethod (setf widget-position) (pos (win window))
  (with-slots (winptr position-y position-x) win
    (when (next-method-p)
      (call-next-method))
    (ncurses:mvwin winptr position-y position-x)))

(defmethod position-y ((win window))
  (ncurses:getbegy (slot-value win 'winptr)))

(defmethod position-y ((win sub-window))
  (with-slots (winptr relativep) win
    (if relativep
        (ncurses:getpary winptr)
        (ncurses:getbegy winptr))))

(defmethod position-x ((win window))
  (ncurses:getbegx (slot-value win 'winptr)))

(defmethod position-x ((win sub-window))
  (with-slots (winptr relativep) win
    (if relativep
        (ncurses:getparx winptr)
        (ncurses:getbegx winptr))))

(defmethod (setf position-y) (y (win window))
  (with-slots (winptr position-y position-x) win
    (when (next-method-p)
      (call-next-method))
    (ncurses:mvwin winptr position-y position-x)))

(defmethod (setf position-x) (x (win window))
  (with-slots (winptr position-y position-x) win
    (when (next-method-p)
      (call-next-method))
    (ncurses:mvwin winptr position-y position-x)))

(defgeneric window-position (window)
  (:documentation "Deprecated but kept for backward compatibility, use the inherited widget-position instead."))
(defmethod window-position ((win window))
  (widget-position win))
(defgeneric (setf window-position) (position window))
(defmethod (setf window-position) (pos (win window))
  (setf (widget-position win) pos))

;; "The screen-relative parameters of the window are not changed.
;; This routine is used to display different parts of the parent
;; window at the same physical position on the screen."
;;
;; "The mvderwin() function specifies a mapping of characters.
;; The function identifies a mapped area of the parent of the specified window"

(defgeneric (setf source-position) (position sub-window)
  (:documentation
   "Set the position of the parent window that will be mirrored in the sub-window.

By default it is identical to the position of the sub-window."))

(defmethod (setf source-position) (position (w sub-window))
  (setf (slot-value w 'source-position) position)
  (ncurses:mvderwin (slot-value w 'winptr) (car position) (cadr position)))

(defgeneric width (object)
  (:documentation "")
  (:method (object)
    (slot-value object 'width)))

(defgeneric height (object)
  (:documentation "")
  (:method (object)
    (slot-value object 'height)))

(defmethod width ((window window))
  (ncurses:getmaxx (slot-value window 'winptr)))

(defmethod height ((window window))
  (ncurses:getmaxy (slot-value window 'winptr)))


(defmethod width ((obj button))
  (with-slots (title name) obj
    (length (format nil "<~A>" (if title title name)))))

(defmethod height ((obj button))
  1)

(defmethod height ((obj label))
  1)

(defmethod height ((obj (eql nil)))
  0)

(defmethod width ((obj (eql nil)))
  ;; nil = "", so length = 0
  0)

(defmethod width ((obj label))
  (with-slots (width title name reference parent) obj
    (if width
        ;; if the object was initialized with a width, take that
        width
        ;; otherwise calc the width from the title or name
        (let* ((text (or title
                         (title (find-element parent reference))
                         (name (find-element parent reference))
                         name))
               (string (when text (format nil "~A" text))))
          (length string)))))

(defmethod width ((obj string))
  (length obj))

(defmethod width ((obj number))
  (length (princ-to-string obj)))

(defmethod width ((obj symbol))
  (length (symbol-name obj)))

(defmethod height ((obj string))
  1)

(defmethod height ((obj number))
  1)

(defmethod height ((obj symbol))
  1)

(defgeneric (setf height) (new-height object)
  (:documentation "Resize the object to have the new height.")
  (:method (new-height object)
    "The default method sets the height slot of the object."
    (with-slots (height) object
      (setf height new-height))))

(defmethod (setf height) (new-height (win window))
  (with-slots (width) win
    ;; set the slots first
    (when (next-method-p)
      (call-next-method))
    (resize win new-height width)))

(defgeneric (setf width) (new-width object)
  (:documentation "Resize the object to have the new width.")
  (:method (new-width object)
    "The default method sets the width slot of the object."
    (with-slots (width) object
      (setf width new-width))))

(defmethod (setf width) (new-width (win window))
  (with-slots (height) win
    ;; set the slots first
    (when (next-method-p)
      (call-next-method))
    (resize win height new-width)))

(defgeneric dimensions (object)
  (:documentation "Return a two-element list with the height and width of the object.")
  (:method (object)
    (list (height object) (width object))))

(defgeneric (setf dimensions) (dimensions object)
  (:documentation "Take a list (height width) and an object, set its height and width.")
  (:method (dimensions object)
    (with-slots (height width) object
      (destructuring-bind (h w) dimensions
        (setf height h
              width  w)))))

(defmethod (setf dimensions) (dimensions (win window))
  (destructuring-bind (height width) dimensions
    ;; set the slots first
    (when (next-method-p)
      (call-next-method))
    (resize win height width)))

(defgeneric geometry (object)
  (:documentation "Return the geometry of the object as a list (y x width height).")
  (:method (object)
    (with-slots (position-y position-x height width) object
      (list position-y position-x height width))))

(defmethod geometry ((object layout))
  (with-slots (position-y position-x height width) object
    (list position-y position-x height width)))

(defmethod geometry ((object window))
  "Return the geometry (y x h w) as returned by ncurses, not the slots."
  (list (position-y object)
        (position-x object)
        (height object)
        (width object)))

(defgeneric (setf geometry) (geometry object)
  (:documentation
   "Set the geometry (y x width height) of the object.

The default method sets the slots of the object.")
  (:method (geometry object)
    (with-slots (position-y position-x height width) object
      (destructuring-bind (y x h w) geometry
        (setf position-y y
              position-x x
              height     h
              width      w)))))

;; the method for layout objects just sets the slots.
(defmethod (setf geometry) (geometry (object layout))
  (with-slots (position-y position-x height width) object
    (destructuring-bind (y x h w) geometry
      (setf position-y y
            position-x x
            height     h
            width      w))))

(defmethod (setf geometry) (geometry (win window))
  ;; first set the slots
  (when (next-method-p)
    (call-next-method))
  (destructuring-bind (y x h w) geometry
    ;; then resize the window, then reposition, strictly in that order to
    ;; prevent parts of the the window from leaving the screen.
    (resize win h w)
    (ncurses:mvwin (winptr win) y x)))

(defun set-geometry-plist (plist yxhw)
  "Take a plist and a (y x h w) geometry list, return the updated plist."
  (loop for i in '(:y :x :height :width)
        for j in yxhw
        do (setf (getf plist i) j))
  plist)

(defun get-geometry-plist (plist)
  (loop for i in '(:y :x :height :width)
        collect (getf plist i)))

(defgeneric cursor-position (window))
(defmethod cursor-position ((win window))
  (list (ncurses:getcury (slot-value win 'winptr))
        (ncurses:getcurx (slot-value win 'winptr))))

;; we can move the cursor by doing this, or by calling "move".
;; both will use ncurses:wmove in the background.
;; note that incf and decf dont work.
(defgeneric (setf cursor-position) (position window))
(defmethod (setf cursor-position) (pos (win window))
  (with-slots (winptr cursor-position) win
    (setf cursor-position pos)
    (apply #'ncurses:wmove winptr pos)))

;; The slots position-y and position-x dont exist, but the pseudo accessors
;; exist for convenience.

(defgeneric cursor-position-y (window))
(defmethod cursor-position-y ((win window))
  (ncurses:getcury (slot-value win 'winptr)))

(defgeneric cursor-position-x (window))
(defmethod cursor-position-x ((win window))
  (ncurses:getcurx (slot-value win 'winptr)))

(defgeneric (setf cursor-position-y) (y window))
(defmethod (setf cursor-position-y) (y (win window))
  (let ((x (cursor-position-x win)))
    (setf (slot-value win 'cursor-position) (list y x))
    (ncurses:wmove (slot-value win 'winptr) y x)))

(defgeneric (setf cursor-position-x) (x window))
(defmethod (setf cursor-position-x) (x (win window))
  (let ((y (cursor-position-y win)))
    (setf (slot-value win 'cursor-position) (list y x))
    (ncurses:wmove (slot-value win 'winptr) y x)))

(defgeneric cursor-visible-p (window))
(defmethod cursor-visible-p ((screen screen))
  (slot-value screen 'cursor-visible-p))
(defgeneric (setf cursor-visible-p) (status screen))
(defmethod (setf cursor-visible-p) (status (screen screen))
  (setf (slot-value screen 'cursor-visible-p) status)
  (set-cursor-visibility status))

(defgeneric input-blocking (window))
(defmethod input-blocking ((window window))
  (slot-value window 'input-blocking))
(defgeneric (setf input-blocking) (status window))
(defmethod (setf input-blocking) (status (window window))
  (setf (slot-value window 'input-blocking) status)
  (set-input-blocking (slot-value window 'winptr) status)) ; see inopts.lisp

(defgeneric frame-rate (obj)
  (:documentation
   "Set the frame rate of the event loop in fps (frames per second).

If the widget is a window, this sets the input-blocking duration of the window.

If the widget is not a window (for example a menu, form or element), setting
the frame rate essentially sets the input-blocking duration of its associated
window.

We have the following relation between the frame rate and the blocking duration:

If the frame rate is nil (default) or 0, input blocking is t (default).

If the frame rate is an integer below 2000, the blocking is 1000/rate miliseconds.

If the frame rate is greater or equal than 2000 fps, the blocking is nil."))

(defmethod frame-rate (obj)
  "If the object is not a window, return the frame rate of its underlying window."
  (frame-rate (window obj)))

(defmethod frame-rate ((win window))
  "Return the frame-rate of a window from its input blocking delay.

blocking  frame-rate
t         nil
nil       2000
100       1000/100"
  (with-slots (input-blocking) win
    (cond ((eq input-blocking t)
           nil)
          ((eq input-blocking nil)
           2000)
          ((numberp input-blocking)
           (round (/ 1000.0 input-blocking))))))

(defgeneric (setf frame-rate) (frame-rate obj)
  (:documentation
   "Set the frame rate of a window or a widget.

A non-nil frame rate implies non-blocking delayed input.

Setting the frame rate sets the input-blocking delay of a window."))

(defmethod (setf frame-rate) (frame-rate obj)
  "If the widget is not a window, set the delay of its associated window."
  (setf (frame-rate (window obj)) frame-rate))

(defmethod (setf frame-rate) (frame-rate (win window))
  "Set the input blocking by setting the frame rate in fps (frames per second).

If the frame rate is nil or 0, input blocking is t.

If the frame rate is a number below 2000, the blocking delay is 1000/rate ms.

If the frame rate is greater or equal than 2000, the blocking is nil.

frame-rate blocking
nil,0      t
<2000      1000/frame-rate
>=2000     nil"
  (setf (input-blocking win)
        (cond ((or (null frame-rate)
                   (zerop frame-rate))
               t)
              ((numberp frame-rate)
               (if (< frame-rate 2000)
                   (round (/ 1000.0 frame-rate))
                   nil)))))

(defgeneric function-keys-enabled-p (window))
(defmethod function-keys-enabled-p ((window window))
  (slot-value window 'function-keys-enabled-p))
(defgeneric (setf function-keys-enabled-p) (status window))
(defmethod (setf function-keys-enabled-p) (status (window window))
  (setf (slot-value window 'function-keys-enabled-p) status)
  (ncurses:keypad (slot-value window 'winptr) status))

(defgeneric scrolling-enabled-p (window))
(defmethod scrolling-enabled-p ((window window))
  (slot-value window 'scrolling-enabled-p))
(defgeneric (setf scrolling-enabled-p) (status window))
(defmethod (setf scrolling-enabled-p) (status (window window))
  (setf (slot-value window 'scrolling-enabled-p) status)
  (ncurses:scrollok (slot-value window 'winptr) status))

(defgeneric scrolling-region (window))
(defmethod scrolling-region ((window window))
  (slot-value window 'scrolling-region))
(defgeneric (setf scrolling-region) (list window))
(defmethod (setf scrolling-region) (list (window window))
  (setf (slot-value window 'scrolling-region) list)
  (ncurses:wsetscrreg (slot-value window 'winptr)
               (first  (slot-value window 'scrolling-region))
               (second (slot-value window 'scrolling-region))))
;; TODO: setf only when scrolling is enabled.

(defgeneric input-echoing-p (screen))
(defmethod input-echoing-p ((screen screen))
  (slot-value screen 'input-echoing-p))
(defgeneric (setf input-echoing-p) (status screen))
(defmethod (setf input-echoing-p) (status (screen screen))
  (setf (slot-value screen 'input-echoing-p) status)
  ;;(set-input-echoing status))
  (if status (ncurses:echo) (ncurses:noecho)))

(defgeneric input-buffering-p (screen))
(defmethod input-buffering-p ((screen screen))
  (slot-value screen 'input-buffering-p))
(defgeneric (setf input-buffering-p) (status screen))
(defmethod (setf input-buffering-p) (status (screen screen))
  (setf (slot-value screen 'input-buffering-p) status)
  (set-input-mode (slot-value screen 'input-buffering-p)
                  (slot-value screen 'process-control-chars-p)))

(defgeneric process-control-chars-p (screen))
(defmethod process-control-chars-p ((screen screen))
  (slot-value screen 'process-control-chars-p))
(defgeneric (setf process-control-chars-p) (status screen))
(defmethod (setf process-control-chars-p) (status (screen screen))
  (with-slots (input-buffering-p process-control-chars-p) screen
    ;; only make a change when the status changed.
    (when (not (eq process-control-chars-p status))
      ;; only call ncurses when buffering is nil.
      (unless input-buffering-p
        ;; before we switch the unbuffered mode, we switch to the default cooked mode.
        (if process-control-chars-p (ncurses:nocbreak) (ncurses:noraw))
        ;; then make a "clean" switch back to the new unbuffered mode.
        (set-input-mode input-buffering-p status))
      ;; then save the new status.
      (setf process-control-chars-p status) )))

(defgeneric newline-translation-enabled-p (screen))
(defmethod newline-translation-enabled-p ((screen screen))
  (slot-value screen 'newline-translation-enabled-p))
(defgeneric (setf newline-translation-enabled-p) (status screen))
(defmethod (setf newline-translation-enabled-p) (status (screen screen))
  (setf (slot-value screen 'newline-translation-enabled-p) status)
  (if status (ncurses:nl) (ncurses:nonl)))

;; TODO: change this to use wide chars, so we can use unicode chars additionally to the limited small set of ACS chars
;; (setf (background window nil) xchar)
;; (setf (background window t) xchar) = (setf (background window) xchar)
(defgeneric background (window))
(defmethod background ((win window))
  ;; TODO: compare whether slot and ncurses return equal xchars
  ;; this isnt the case if we set a ACS char, which gets translated to a code.
  ;;(let ((slot (slot-value win 'background))
  ;;  (ncurses (get-background-cchar_t win)))
  (slot-value win 'background))

;; TODO: writing a normal string waddstr on a wide cchar background causes an SB-KERNEL::CONTROL-STACK-EXHAUSTED-ERROR
(defgeneric (setf background) (char window &optional apply))
(defmethod (setf background) (char (window window) &optional (apply t))
  (setf (slot-value window 'background) char)
  ;; ncurses
  ;;(set-background-char (slot-value window 'winptr) char apply))
  ;; ncursesw
  (set-background-cchar_t window char apply))

;(defgeneric attributes (window))
(defmethod attributes ((window window))
  (slot-value window 'attributes))

;;(defgeneric (setf attributes) (attributes window))
#|
(defmethod (setf attributes) (attributes (window window))
  (let ((added (set-difference attributes (slot-value window 'attributes)))
        (removed (set-difference (slot-value window 'attributes) attributes)))
    (setf (slot-value window 'attributes) attributes)
    (add-attributes window added)
    (remove-attributes window removed)))
|#
;; TODO: use ncurses:wattron and ncurses:wattroff here.

(defmethod (setf attributes) (attributes (win window))
  (with-slots ((win-attributes attributes)) win
    (let ((added   (set-difference     attributes win-attributes))
          (removed (set-difference win-attributes     attributes)))
      ;; set the list of attributes to the window slot
      (setf win-attributes attributes)
      ;; pass the attrbutes to add and remove to ncurses
      (add-attributes win added)
      (remove-attributes win removed))))

;; we dont want to use set-attributes because it also overwrites the color attribute.
;; we want the attributes function to just handle attributes and leave the color alone.
;; to do that, we have to treat the attributes as a set and ignore set-attributes.

;; (defmethod (setf attributes) (attributes (window window))
;;   (setf (slot-value window 'attributes) attributes)
;;   (set-attributes window attributes))


;; TODO 200731 we do not need separate methods for windows and complex-chars
;; they are identical

(defgeneric color-pair (object)
  (:documentation
   "Return the color pair of the object as a 2-element list (fg bg).")
  (:method (object)
    (with-slots (fgcolor bgcolor) object
      (if (or fgcolor bgcolor)
          (list fgcolor bgcolor)
          ;; when both colors are nil, do not return (nil nil) but nil
          nil))))

(defgeneric (setf color-pair) (color-pair object)
  (:documentation
   "Set the color pair of the object by setting the fgcolor and bgcolor properties.")
  (:method (color-pair object)
    (with-slots (fgcolor bgcolor) object
      (if color-pair
          (setf fgcolor (car color-pair)
                bgcolor (cadr color-pair))
          ;; when color-pair is nil, both fg and bg are set to nil
          (setf fgcolor nil
                bgcolor nil)))))

(defmethod (setf color-pair) (color-pair (win window))
  ;; calls the default method
  (call-next-method)
  ;; after the slots are set, pass the new pair to ncurses
  (set-color-pair (slot-value win 'winptr) (color-pair win)))

(defmethod fgcolor ((win window))
  (slot-value win 'fgcolor))

(defmethod bgcolor ((win window))
  (slot-value win 'bgcolor))

(defmethod (setf fgcolor) (fgcolor (win window))
  (setf (slot-value win 'fgcolor) fgcolor)
  ;; set the color pair in the underlying ncurses lib.
  (set-color-pair (slot-value win 'winptr) (color-pair win)))

(defmethod (setf bgcolor) (bgcolor (win window))
  (setf (slot-value win 'bgcolor) bgcolor)
  ;; set the color pair in the underlying ncurses lib.
  (set-color-pair (slot-value win 'winptr) (color-pair win)))

(defun center-position (window)
  "Return the position (y x) of the center of the window."
  (with-accessors ((h height) (w width)) window
    (list (if (oddp h)
              (floor h 2)
              (/ h 2))
          (if (oddp w)
              (floor w 2)
              (/ w 2)))))

(defun random-position (win)
  "Return a random valid position (y x) inside the window."
  (list (random (height win))
        (random (width win))))

(defun toggle-insert-mode (object)
  "Toggle the insert mode boolean property of the object.

This applies to window, field and textarea objects, for example."
  (if (slot-exists-p object 'insert-mode-p)
      (setf (insert-mode-p object) (not (insert-mode-p object)))
      (error "toggle-insert-mode: the object does not have an insert-mode-p property.")))

(defmethod stackedp ((win window))
  (slot-value win 'stackedp))

(defmethod (setf stackedp) (stackedp (win window))
  "Add or remove the window to the global window stack."
  (if stackedp
      ;; t: check if in stack, if not, add to stack, if yes, error
      (if (member win (items *main-stack*) :test #'eq)
          (error "setf stackedp t: window already on stack")
          (progn
            (stack-push win *main-stack*)
            (setf (slot-value win 'stackedp) t)))
      ;; nil: check if in stack, if yes, remove from stack, if not, error
      (if (member win (items *main-stack*) :test #'eq)
          (progn
            (setf (items *main-stack*) (remove win (items *main-stack*) :test #'eq))
            (setf (slot-value win 'stackedp) nil))
          (error "setf stackedp nil: window not on stack"))))

;; (window (if (window field) (window field) (window (parent field))))
(defmethod window ((element element))
  "Return the window associated with an element, which can optionally be part of a form.

If there is no window asociated with the element, return the window associated with the parent form."
  (with-slots (window parent) element
    (if window
        window
        ;; not every element (for example a menu) has a parent form.
        (if parent
            (if (slot-value parent 'window)
                (slot-value parent 'window)
                (error "(window element) ERROR: No window is associated with the parent form."))
            (error "(window element) ERROR: Neither a window nor a parent form are associated with the element.")))))

(defmethod (setf window) (window (element element))
  (setf (slot-value element 'window) window))

;; TODO: add a way to specify element default styles outside of a form style
(defmethod style ((element element))
  "If the element's style slot is empty, check whether a default style has been defined in the parent form."
  (with-slots (style parent) element
    (if style
        style
        ;; not every element (for example a menu) has a parent form.
        (when parent
          (if (slot-value parent 'style)
              ;; get the default element style from the form style slot.
              (getf (slot-value parent 'style) (type-of element))
              nil)))))

;;; print, prin1, princ, format ~A, ~S

;; print a wide but simple lisp character to a window
;; TODO: add check for insert-mode
(defmethod print-object ((ch character) (stream window))
  (add-wide-char stream ch))

;; TODO: add check for insert-mode
(defmethod print-object ((ch complex-char) (stream window))
  (add-wide-char stream ch))

;; print only simple chars to the lisp repl.
(defmethod print-object ((ch complex-char) stream)
  (princ (simple-char ch) stream))

;; print simple strings to a ncurses widow.
(defmethod print-object ((str string) (stream window))
  (ncurses:waddstr (winptr stream) str))

;; print complex strings to a ncurses window.
(defmethod print-object ((cstr complex-string) (stream window))
  (loop for ch across (complex-char-array cstr)
     do (add-wide-char stream ch)))

;; print only simple chars to the lisp repl.
(defmethod print-object ((cstr complex-string) stream)
  (loop for ch across (complex-char-array cstr)
     do (princ (simple-char ch) stream)))

;; methods to close streams, and thus screen _and_ windows, since they are now gray streams.
;; end-screen = ncurses:endwin
;; delete-window = ncurses:delwin
;; see t07a
;; remember we cant use open to open a window. it works only for file streams.
;; we have to make-instance of a window or a screen.

(defmethod close ((stream window) &key abort)
  (declare (ignore abort))
  ;; if by time of closing, the window is still on the stack, remove it first.
  (if (member stream (items *main-stack*) :test #'eq)
      (setf (items *main-stack*) (remove stream (items *main-stack*) :test #'eq)))
  (ncurses:delwin (winptr stream))
  ;; The default method provided by class FUNDAMENTAL-STREAM sets a flag for OPEN-STREAM-P.
  ;; TODO 210117 we have to do this for other window types too
  (call-next-method))

(defmethod close ((stream sub-window) &key abort)
  (declare (ignore abort))
  (ncurses:delwin (winptr stream)))

(defmethod close ((stream screen) &key abort)
  (declare (ignore abort))
  (ncurses:endwin))

(defmethod close ((stream extended-window) &key abort)
  (declare (ignore abort))
  (ncurses:delwin (winptr (sub-window stream)))
  (ncurses:delwin (winptr stream)))

(defmethod close ((stream form-window) &key abort)
  (declare (ignore abort))
  (ncurses:delwin (winptr (sub-window stream)))
  (ncurses:delwin (winptr stream)))

;; SBCL bug when specializing close on gray streams:
;; STYLE-WARNING:
;;    Generic function CLOSE clobbers an earlier FTYPE proclamation
;;    (FUNCTION (STREAM &KEY (:ABORT T)) (VALUES (MEMBER T) &OPTIONAL)) for the
;; same name with (FUNCTION (T &KEY (:ABORT T)) *).
