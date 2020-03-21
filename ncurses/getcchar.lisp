(in-package :de.anvi.ncurses)

;;; getcchar
;;; Get a wide character string and rendition from a cchar_t or set a cchar_t from a wide-character string
;;; http://invisible-island.net/ncurses/man/curs_getcchar.3x.html

;;; C prototypes

;; int getcchar(const cchar_t *wcval, wchar_t *wch, attr_t *attrs, short *color_pair, void *opts);
;; int setcchar(cchar_t *wcval, const wchar_t *wch, const attr_t attrs, short color_pair, void *opts);

;;; Low-level CFFI wrappers

(cffi:defcfun ("getcchar" %getcchar) :int
  (wcval      (:pointer (:struct cchar_t)))
  (wch        (:pointer wchar_t))
  (attrs      (:pointer attr))
  (color_pair (:pointer :short))
  (opts       (:pointer :void)))

(cffi:defcfun ("setcchar" %setcchar) :int
  (wcval      (:pointer (:struct cchar_t)))
  (wch        (:pointer wchar_t))
  (attrs      attr)
  (color_pair :short)
  (opts       (:pointer :void)))
