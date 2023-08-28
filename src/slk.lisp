(in-package :de.anvi.croatoan.soft-labels)

(defun initialize (&optional (mode :mode-4-4-4))
  "Initialize soft function-key labels on the bottom screen line.

Soft labels are used to display short titles of the functions bound
to the function keys F1 to F12.

The following 4 modes (label layouts) are supported:

Show 8 labels, each with a width of 8 characters:

:mode-3-2-3       - 3 labels left, 2 in the center, 3 right
:mode-4-4         - 4 labels left, 4 labels right

Show 12 labels, each with a width of 5 characters:

:mode-4-4-4       - 4 labels left, 4 in the center, 4 right
:mode-4-4-4-index - display an line with F1-F12 above the labels.

The single-line 4-4-4 mode without the index line is the default,
as most PC keyboards support 12 function keys."
  (let ((modes '(:mode-3-2-3       0
                 :mode-4-4         1
                 :mode-4-4-4       2
                 :mode-4-4-4-index 3)))
    (if (getf modes mode)
        (ncurses:slk-init (getf modes mode))
        (error "init-soft-labels: unknown mode ~A" mode))))

(defun label (number)
  "Return the title of the soft label given by the function key number."
  (ncurses:slk-label number))

;; (setf (soft-label 1) "foo")
;; (setf (soft-label 2 :right) "bar")
(defun (setf label) (title number &optional (alignment :left))
  "Set the title of the label of the function key given by the number.

Note that since function keys are F1-F12, the numbering starts at 1."
  (let* ((aligns '(:left   0
                   :center 1
                   :right  2))
         (align (getf aligns alignment)))
    (if align
        (if (every (lambda (i) (< (char-code i) 255)) title)
            ;; if every char in title is narrow, use set
            (ncurses:slk-set number title align)
            ;; for multibyte (wide) chars use wset
            (let ((n (length title)))
              (cffi:with-foreign-object (wstr 'ncurses::wchar_t n)
                (dotimes (i n)
                  (setf (cffi:mem-aref wstr 'ncurses::wchar_t i) 0))
                (dotimes (i n)
                  (setf (cffi:mem-aref wstr 'ncurses::wchar_t i) (nth i (mapcar #'char-code (coerce title 'list)))))
                (ncurses:slk-wset number wstr align))))
        (error "set-soft-label: unsupported alignment ~A" alignment))))

(defun refresh ()
  "Refresh the soft label line(s)."
  (ncurses:slk-refresh))

(defun mark-for-refresh ()
  "Mark the soft labels for a later refresh with refresh-marked.

This allows the soft labes to be refreshed in one sweep with other
elements of the screen."
  (ncurses:slk-noutrefresh))

(defun touch ()
  "Force all labels to be output on the next refresh.

Touch marks the label line as changed because refresh only considers
changed cells."
  (ncurses:slk-touch))

(defun clear ()
  "Clear labels from the screen. Use restore to restore the labels afterwards."
  (ncurses:slk-clear))

(defun restore ()
  "Restore the labels previously removed by clear."
  (ncurses:slk-restore))

(defun add-attribute (attribute)
  "Add a single attribute to the rendition of soft labels.

The labels maybe have to be touched for attribute changes to take
effect."
  (ncurses:slk-attron (crt::get-bitmask attribute)))

(defun add-attributes (attributes)
  "Add attributes to the rendition of the soft labels.

The default attribute used to highlight soft labels is :standout."
  (mapc #'add-attribute attributes))

(defun remove-attribute (attribute)
  "Remove attribute from the rendition of soft labels.

The labels maybe have to be touched for attribute changes to take
effect."
  (ncurses:slk-attroff (crt:get-bitmask attribute)))

(defun remove-attributes (attributes)
  "Takes a list of keywords and turns the appropriate attributes off."
  (mapc #'remove-attribute attributes))

(defun attributes ()
  "Return a list of attributes used for the rendition of soft labels."
  (crt:chtype2attrs (ncurses:slk-attr)))

(defmethod (setf attributes) (new)
  "Set the attributes used for the rendition of soft labels.

This function only adds or removes attributes and does not affect the
color attributes."
  (let* ((current (attributes))
         (added   (set-difference new current))
         (removed (set-difference current new)))
    (add-attributes added)
    (remove-attributes removed)))

(defun set-attributes (attributes color-pair)
  "Set the attributes and color-pair for the rendition of soft labels.

This function overwrites any previous used attributes including the
color. If you only want to set the attributes, you might want to use
setf attributes instead."
  (ncurses:slk-attr-set (crt:attrs2chtype attributes)
                        (crt:pair-to-number (crt:complete-default-pair color-pair))
                        (cffi:null-pointer)))

(defun color-pair ()
  "Return the color pair used for soft labels.

The color pair is a 2-element list (:foreground :background)."
  (crt:chtype2colors (ncurses:slk-attr)))

(defun (setf color-pair) (pair)
  "Set the color pair used for soft labels.

The color pair is a 2-element list (:foreground :background)."
  (ncurses:slk-color (crt:pair-to-number (crt:complete-default-pair pair))))
