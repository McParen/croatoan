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
  "A message box displays a (wrapped) message and one default OK button."
  (with-screen (scr :input-echoing nil :cursor-visible t :enable-colors t :enable-function-keys t :input-blocking t)
    (let* ((dlg (make-instance 'dlg:msgbox
                               :title "Msgbox dialog"
                               :message *dialog-message*
                               :buttons '((:ok . t) (:cancel . nil))
                               :style *dialog-style*)))
      (setf (background scr) (make-instance 'complex-char :simple-char #x2592 :color-pair (list :white :black)))
      (refresh scr)
      (format scr "~A" (prog1 (edit dlg) (clear scr)))
      (refresh scr)
      (get-char scr)
      (close dlg))))

(defun dlg02 ()
  "A menu box displays a list of items as a menu and buttons to accept the selection."
  (with-screen (scr :input-echoing nil :cursor-visible t :enable-colors t :enable-function-keys t :input-blocking t)
    (let* ((dlg (make-instance 'dlg:menubox
                               :title "Menubox dialog"
                               :message *dialog-message*
                               :choices (mapcar (lambda (i) (format nil "~R" i)) '(1 2 3 4 5 6 7 8 9))
                               :buttons '(ok (cancel . nil)))))
      (setf (background scr) (make-instance 'complex-char :simple-char #x2592 :color-pair (list :white :black)))
      (refresh scr)
      (multiple-value-bind (btn frm) (edit dlg)
        (clear scr)
        (format scr "~A~%~A" btn frm))
      (refresh scr)
      (get-char scr)
      (close dlg))))

(defun dlg03 ()
  (with-screen (scr :input-echoing nil :cursor-visible t :enable-colors t :enable-function-keys t :input-blocking t)
    (let* ((dlg (make-instance 'dlg:checklist
                               :title "Checklist dialog"
                               :message *dialog-message*
                               :choices (mapcar (lambda (i) (format nil "~R" i)) '(1 2 3 4 5 6 7 8 9))
                               :buttons '("OK" "Cancel")
                               :style *dialog-style*)))
      (setf (background scr) (make-instance 'complex-char :simple-char #x2592 :color-pair (list :white :black)))
      (refresh scr)
      (multiple-value-bind (btn frm) (edit dlg)
        (clear scr)
        (format scr "~A~%~A" btn frm))
      (refresh scr)
      (get-char scr)
      (close dlg))))

(defun dlg04 ()
  (with-screen (scr :input-echoing nil :cursor-visible t :enable-colors t :enable-function-keys t :input-blocking t)
    (let* ((dlg (make-instance 'dlg:inputbox
                               :title "Inputbox dialog"
                               :message *dialog-message*
                               ;; One field is provided by default, additional fields can be added as a list of field names
                               ;; At the moment, their names/titles are not displayed as labels, WIP.
                               :fields '(f1 f2)
                               :style *dialog-style*)))
      (setf (background scr) (make-instance 'complex-char :simple-char #x2592 :color-pair (list :white :black)))
      (refresh scr)
      (multiple-value-bind (btn frm) (edit dlg)
        (clear scr)
        (format scr "~A~%~A" btn frm))
      (refresh scr)
      (get-char scr)
      (close dlg))))
