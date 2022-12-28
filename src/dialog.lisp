(in-package :de.anvi.croatoan.dialog)

(defclass dialog (form-window)
  ()
  (:default-initargs
   :enable-function-keys t
   :input-blocking t
   :border t
   :center t
   :border-width 2)

  (:documentation
   "Dialog is the base class for different spacialized dialog windows."))

;; called after the primary methods of all dialogs
(defmethod initialize-instance :after ((obj dialog) &key center message buttons)
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
    (setf winptr (ncurses:newwin height width y x))
    (setf sub-window (make-instance 'sub-window :parent obj :height (- height (* 2 border-width)) :width (- width (* 2 border-width))
                                                :position (list border-width border-width) :relative t :enable-function-keys t))
    (setf window sub-window)

    ;; form
    (setf (children obj) (flatten-items layout))
    (setf current-item-number (position-if #'activep (children obj)))
    (setf (slot-value (current-item obj) 'selectedp) t)
    (dolist (i (children obj))
      (setf (slot-value i 'parent) obj))))

(defclass msgbox (dialog) ())

(defmethod initialize-instance ((obj msgbox) &rest initargs &key center message buttons)
  (apply #'shared-initialize obj t initargs)
  (with-slots (width layout) obj
    (unless width (setf width (round (* ncurses:COLS 2/3))))
    (setf layout (make-instance 'column-layout
                                :position '(0 0)
                                :children (list (make-instance 'label :title (wrap-string message (- width 6)) :width (- width 4)
                                                                      :border nil :padding '(1 1))
                                                (make-instance 'row-layout
                                                               :children (mapcar (lambda (i)
                                                                                   (make-instance 'button :title (if (consp i) (car i) i)
                                                                                                          :value (if (consp i) (cdr i) i)
                                                                                                          :callback 'return-element-value
                                                                                                          :border t
                                                                                                          :padding '(0 1)))
                                                                                 buttons)))))))

(defclass menubox (dialog) ())

(defmethod initialize-instance ((obj menubox) &rest initargs &key center message buttons choices)
  (apply #'shared-initialize obj t initargs)
  (with-slots (width layout) obj
    (unless width (setf width (round (* ncurses:COLS 2/3))))
    (setf layout (make-instance 'column-layout
                                :position '(0 0)
                                :children (list (make-instance 'label :title (wrap-string message (- width 6)) :width (- width 4)
                                                                      :border nil :padding '(1 1))
                                                (make-instance 'menu
                                                               :items choices
                                                               :padding '(0 1)
                                                               :max-item-length (- width 6)
                                                               :enable-scrolling t
                                                               :region-dimensions '(4 1))
                                                (make-instance 'row-layout
                                                               :children (mapcar (lambda (i)
                                                                                   (make-instance 'button :title (if (consp i) (car i) i)
                                                                                                          :value (if (consp i) (cdr i) i)
                                                                                                          :callback 'return-form-values
                                                                                                          :border t
                                                                                                          :padding '(0 1)))
                                                                                 buttons)))))))

(defclass checklist (dialog) ())

(defmethod initialize-instance ((obj checklist) &rest initargs &key center message buttons choices)
  (apply #'shared-initialize obj t initargs)
  (with-slots (width layout) obj
    (unless width (setf width (round (* ncurses:COLS 2/3))))
    (setf layout (make-instance 'column-layout
                                :position '(0 0)
                                :children (list (make-instance 'label :title (wrap-string message (- width 6)) :width (- width 4)
                                                                      :border nil :padding-bottom 1)
                                                (make-instance 'crt:checklist
                                                               :items choices
                                                               :padding-bottom 1
                                                               :max-item-length (- width 9)
                                                               :enable-scrolling t
                                                               :region-dimensions '(4 1))
                                                (make-instance 'row-layout
                                                               :children (mapcar (lambda (i)
                                                                                   (make-instance 'button :title (if (consp i) (car i) i)
                                                                                                          :value (if (consp i) (cdr i) i)
                                                                                                          :callback (lambda () (throw obj (value (nth 1 (children obj)))))
                                                                                                          :border t
                                                                                                          :padding '(0 1)))
                                                                                 buttons)))))))

(defclass inputbox (dialog) ())

(defmethod initialize-instance ((obj inputbox) &rest initargs &key center message buttons)
  (apply #'shared-initialize obj t initargs)
  (with-slots (width layout) obj
    (unless width (setf width (round (* ncurses:COLS 2/3))))
    (setf layout (make-instance 'column-layout
                                :position '(0 0)
                                :children (list (make-instance 'label :title (wrap-string message (- width 6)) :width (- width 4)
                                                                      :border nil :padding-bottom 1)
                                                (make-instance 'field
                                                               :width (- width 10)
                                                               :border t)
                                                (make-instance 'row-layout
                                                               :children (mapcar (lambda (i)
                                                                                   (make-instance 'button :title (if (consp i) (car i) i)
                                                                                                          :value (if (consp i) (cdr i) i)
                                                                                                          :callback (lambda () (throw obj (value (nth 1 (children obj)))))
                                                                                                          :border t
                                                                                                          :padding '(0 1)))
                                                                                 buttons)))))))
