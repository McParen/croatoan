(in-package :de.anvi.croatoan.dialog)

(defclass dialog (form-window)
  ()
  (:default-initargs
   :enable-function-keys t
   :input-blocking t
   :border t
   :center t
   :border-width 2
   :wrap-message t)

  (:documentation
   "Dialog is the base class for different spacialized dialog windows."))

;; called after the primary methods of all dialogs
(defmethod initialize-instance :after ((obj dialog) &key center message buttons wrap-message)
  (with-slots (winptr height width (y position-y) (x position-x) window layout sub-window current-item-number borderp border-width) obj
    ;; layout positions
    ;; dimensions
    (calculate-positions layout)
    (setf height (+ (* 2 border-width) (height layout)))
    (when center
      (setf y (- (round (/ ncurses:LINES 2)) (round (/ height 2)))
            x (- (round (/ ncurses:COLS  2)) (round (/ width  2)))))

    ;; form window
    (when (and borderp (zerop border-width))
      (setf border-width 1))
    (setf winptr (ncurses:newwin (1- height) width y x))
    (setf sub-window (make-instance 'sub-window :parent obj :height (- height (* 2 border-width)) :width (- width (* 2 border-width))
                                                :position (list border-width border-width) :relative t :enable-function-keys t))
    (setf window sub-window)

    ;; form
    (setf (children obj) (flatten-items layout))
    (setf current-item-number (position-if #'activep (children obj)))
    (setf (slot-value (current-item obj) 'selectedp) t)
    (dolist (i (children obj))
      (setf (slot-value i 'parent) obj))))

(defclass msgbox (dialog)
  ()
  (:default-initargs
   :buttons '(("OK" . t)))
  (:documentation
"A message box displays a (wrapped) message and one default OK button.

The message is wrapped by default, but the wrapping can be disabled to
display a pre-formatted text, for example.

The title of the activated button is returned, or its value, if provided."))

(defun make-dialog-button (item)
  "Take an item describing a dialog button, return the object.

If it is a single string, use the string as both title and value.

If it is a cons, use the cdr as the returned value.

If the car is a symbol, use it as the button name, if it's a string,
use it as the title."
  (let ((text (if (consp item) (car item) item))
        (val  (if (consp item) (cdr item) item)))
    (typecase text
      (string (make-instance 'button :name (gensym "BUTTON") :title text :value val :callback 'return-form-values :border t :padding '(0 1)))
      (symbol (make-instance 'button :name text                          :value val :callback 'return-form-values :border t :padding '(0 1))))))

(defun make-dialog-layout (width message wrapp buttons &rest elements)
  (make-instance 'column-layout
                 :position '(0 0)
                 :children
                 (append (list (make-instance 'label :title (if wrapp
                                                                (wrap-string message (- width 6))
                                                                message)
                                                     :width (- width 4)
                                                     :border nil
                                                     :padding-bottom 1))
                         elements
                         (list (make-instance 'row-layout :children (mapcar #'make-dialog-button buttons))))))

(defmethod initialize-instance ((obj msgbox) &rest initargs &key center message buttons wrap-message)
  (apply #'shared-initialize obj t initargs)
  (with-slots (width layout) obj
    (unless width (setf width (round (* ncurses:COLS 2/3))))
    (setf layout (apply #'make-dialog-layout width message wrap-message buttons nil))))

(defclass menubox (dialog) ())

(defmethod initialize-instance ((obj menubox) &rest initargs &key center message buttons wrap-message choices)
  (apply #'shared-initialize obj t initargs)
  (with-slots (width layout) obj
    (unless width (setf width (round (* ncurses:COLS 2/3))))
    (setf layout (apply #'make-dialog-layout width message wrap-message buttons
                        (list (make-instance 'menu
                                             :name :menu
                                             :items choices
                                             :border t
                                             :item-padding-left 1
                                             ;:padding-left 1
                                             ;:padding-bottom 1
                                             :max-item-length (- width 8)
                                             :enable-scrolling t
                                             :region-dimensions '(4 1)))))))

(defclass checklist (dialog) ())

(defmethod initialize-instance ((obj checklist) &rest initargs &key center message buttons wrap-message choices)
  (apply #'shared-initialize obj t initargs)
  (with-slots (width layout) obj
    (unless width (setf width (round (* ncurses:COLS 2/3))))
    (setf layout (apply #'make-dialog-layout width message wrap-message buttons
                        (list (make-instance 'crt:checklist
                                             :name :checklist
                                             :items choices
                                             :border t
                                             ;:padding-bottom 1
                                             :max-item-length (- width 11)
                                             :enable-scrolling t
                                             :region-dimensions '(4 1)))))))

(defclass inputbox (dialog)
  ()
  (:default-initargs
   :buttons '((ok . t) (cancel . nil))
   :fields '(:field))
  (:documentation
   "An input box provides one or more input fields and buttons to accept or cancel the input."))

(defmethod initialize-instance ((obj inputbox) &rest initargs &key center message buttons wrap-message fields)
  (apply #'shared-initialize obj t initargs)
  (with-slots (width layout) obj
    (unless width (setf width (round (* ncurses:COLS 2/3))))
    (setf layout (apply #'make-dialog-layout width message wrap-message buttons
                        (loop for i in fields collect
                          (make-instance 'field
                                         :name i
                                         :width (- width 8)
                                         :border t))))))
