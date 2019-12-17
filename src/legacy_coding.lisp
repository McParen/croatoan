(in-package :croatoan)

;;; legacy coding
;;; http://invisible-island.net/ncurses/man/legacy_coding.3x.html

;;; C prototypes

;; int use_legacy_coding(int level);

;;; Low-level C functions

(defcfun ("use_legacy_coding" %use-legacy-coding) :int (level :int))

;;; High-level Lisp wrappers

;; Possible values: 0 (default), 1 and 2.
(defun set-char-representation (level)
  "Set how char-to-string will represent a char."
  (%use-legacy-coding level))

;;; NOTES

;; This affects %unctrl/char-to-string. See util.lisp and the manpage.

;;; TODOs

