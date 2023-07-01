(in-package :de.anvi.ncurses)

;;; libc

;; since SBCL links to libc by default, we dont have to use-foreign-library explicitely.

;;; C prototypes

;; #include <locale.h>

;; /usr/include/locale.h
;; /usr/include/x86_64-linux-gnu/bits/locale.h

;; https://www.gnu.org/software/libc/manual/html_node/Setting-the-Locale.html
;; char* setlocale (int category, const char* locale);

;;; Low-level CFFI wrappers

(cffi:defcfun ("setlocale" setlocale) :string (category :int) (locale :string))

;; (cffi:defcvar ("LC_ALL" +LC-ALL+ :read-only t) :int)
;; ERROR: Trying to access undefined foreign variable "LC_ALL".

(defconstant +LC-CTYPE+           0)
(defconstant +LC-NUMERIC+         1)
(defconstant +LC-TIME+            2)
(defconstant +LC-COLLATE+         3)
(defconstant +LC-MONETARY+        4)
(defconstant +LC-MESSAGES+        5)
(defconstant +LC-ALL+             6)
(defconstant +LC-PAPER+           7)
(defconstant +LC-NAME+            8)
(defconstant +LC-ADDRESS+         9)
(defconstant +LC-TELEPHONE+      10)
(defconstant +LC-MEASUREMENT+    11)
(defconstant +LC-IDENTIFICATION+ 12)

;; #include <wchar.h>
;; int wcwidth(wchar_t wc);

(cffi:defcfun ("wcwidth" wcwidth) :int (ucs wchar_t))
