(in-package :de.anvi.croatoan.test)

(defparameter *dialog-message*
  "This is my textarea. There are many like it, but this one is mine.
My textarea is my best friend. It is my life. I must master it as I
must master my life.")

(defparameter *dialog-style*
  '(:foreground (:fgcolor :red)
    :title (:fgcolor :black :bgcolor :yellow)
    label  (:foreground (:fgcolor :yellow :bgcolor :green))
    button (:selected-border (:fgcolor :cyan)
            :selected-foreground (:fgcolor :cyan))))

(defparameter *dialog-style-2*
  '(button   (:selected-foreground (:fgcolor :white :bgcolor :blue))
    textarea (:foreground (:fgcolor :blue :bgcolor :white)
              :background (:fgcolor :blue :bgcolor :green :simple-char #\.))
    :title (:fgcolor :black :bgcolor :yellow :attributes (:bold))
    :foreground (:fgcolor :red :bgcolor :yellow)
    :background (:fgcolor :white :bgcolor :black :simple-char #\-)))

(defun dlg01 ()
  (with-screen (scr :input-echoing nil :cursor-visible t :enable-colors t :enable-function-keys t :input-blocking t)
    (let* ((dlg (make-instance 'dlg:msgbox
                               :title "Form window"
                               :message *t16k-message*
                               :buttons '("OK")
                               :style *dialog-style*)))
      (setf (background scr) (make-instance 'complex-char :simple-char #x2592 :color-pair (list :white :black)))
      (refresh scr)
      (format scr "~A" (edit dlg))
      (refresh scr)
      (get-char scr)
      (close dlg))))

(defun dlg02 ()
  (with-screen (scr :input-echoing nil :cursor-visible t :enable-colors t :enable-function-keys t :input-blocking t)
    (let* ((dlg (make-instance 'dlg:menubox
                               :title "Checklist dialog"
                               :message *t16k-message*
                               :choices (mapcar (lambda (i) (format nil "~R" i)) '(1 2 3 4 5 6 7 8 9))
                               :buttons '("OK" "Cancel")
                               :style *dialog-style*)))
      (setf (background scr) (make-instance 'complex-char :simple-char #x2592 :color-pair (list :white :black)))
      (refresh scr)
      (format scr "~A" (edit dlg))
      (refresh scr)
      (get-char scr)
      (close dlg))))

(defun dlg03 ()
  (with-screen (scr :input-echoing nil :cursor-visible t :enable-colors t :enable-function-keys t :input-blocking t)
    (let* ((dlg (make-instance 'dlg:checklist
                               :title "Checklist dialog"
                               :message *t16k-message*
                               :choices (mapcar (lambda (i) (format nil "~R" i)) '(1 2 3 4 5 6 7 8 9))
                               :buttons '("OK" "Cancel")
                               :style *dialog-style*)))
      (setf (background scr) (make-instance 'complex-char :simple-char #x2592 :color-pair (list :white :black)))
      (refresh scr)
      (format scr "~A" (edit dlg))
      (refresh scr)
      (get-char scr)
      (close dlg))))

(defun dlg04 ()
  (let (result)
    (with-screen (scr :input-echoing nil :cursor-visible t :enable-colors t :enable-function-keys t :input-blocking t)
      (let* ((dlg (make-instance 'dlg:inputbox
                                 :title "Inputbox dialog"
                                 :message *t16k-message*
                                 :buttons '("OK" "Cancel")
                                 :style *dialog-style*)))
        (setf (background scr) (make-instance 'complex-char :simple-char #x2592 :color-pair (list :white :black)))
        (refresh scr)
        (setq result (edit dlg))
        (close dlg)))
    (format t "~A" result)))
