(in-package :de.anvi.croatoan)

;; 0 are the default line characters.
(defun box (window &optional (hline-char 0) (vline-char 0))
  "Draw a border around the window."
  (let ((winptr (.winptr window)))
    (%box winptr hline-char vline-char)))

#|

(defun border (window 
               left-side right-side top-side bottom-side 
               top-left-corner top-right-corner bottom-left-corner bottom-right-corner)
  "Draw a border around the window using the given chars.

If any parameter is zero, the default ACS char will be used."
  (%wborder window 
            left-side right-side top-side bottom-side 
            top-left-corner top-right-corner bottom-left-corner bottom-right-corner))

(defun hline (window line-char max-length &key y x)
  (cond ((and y x)
         (%mvwhline window y x line-char max-length))
        (t
         (%whline window line-char max-length))))

(defun vline (window line-char max-length &key y x)
  (cond ((and y x)
         (%mvwhline window y x line-char max-length))
        (t
         (%wvline window line-char max-length))))

|#
