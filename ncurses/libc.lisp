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
