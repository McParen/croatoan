(in-package :de.anvi.croatoan.test)

(defparameter *dialog-message*
  "This is my textarea. There are many like it, but this one is mine.
My textarea is my best friend. It is my life. I must master it as I
must master my life.")

(defparameter *dialog-style*
  '(:foreground (:fgcolor :red)
    :title (:fgcolor :black :bgcolor :yellow)
    field  (:background (:simple-char #\.)
            :selected-foreground (:fgcolor :yellow :attributes (:bold))
            :selected-background (:fgcolor :yellow :attributes (:bold) :simple-char #\.))
    checklist (:border (:fgcolor :blue)
               :foreground (:fgcolor :yellow)
               :selected-foreground (:bgcolor :yellow :attributes (:bold)))
    label  (:foreground (:fgcolor :black :bgcolor :green))
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
  "A checklist dialog is similar to a menu, but allows to select zero or more items from a list."
  (with-screen (scr :input-echoing nil :cursor-visible t :enable-colors t :enable-function-keys t :input-blocking t)
    (let* ((dlg (make-instance 'dlg:checklist
                               :title "Checklist dialog"
                               :message *dialog-message*
                               :choices (mapcar (lambda (i) (format nil "~R" i)) '(1 2 3 4 5 6 7 8 9))
                               :buttons '("OK there" ("Cancel this" . nil))
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
  "An input box allows the user to enter one (default) or more values."
  (with-screen (scr :input-echoing nil :cursor-visible t :enable-colors t :enable-function-keys t :input-blocking t)
    (let* ((dlg (make-instance 'dlg:inputbox
                               :title "Inputbox dialog"
                               :message *dialog-message*
                               :width 50
                               ;; If omitted, one field is provided by default, other fields can be provided as a list
                               ;; of names or conses of names (to return in the alist) and titles (to display).
                               :fields '(forename surname (address . "my address") :age)
                               :style *dialog-style*)))
      (setf (background scr) (make-instance 'complex-char :simple-char #x2592 :color-pair (list :white :black)))
      (refresh scr)
      (multiple-value-bind (btn frm) (edit dlg)
        (clear scr)
        (format scr "~A~%~A" btn frm))
      (refresh scr)
      (get-char scr)
      (close dlg))))
