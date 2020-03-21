(in-package :de.anvi.ncurses)

;;; attr
;;; curses character and window attribute control routines
;;; http://invisible-island.net/ncurses/man/curs_attr.3x.html
;;; http://publib.boulder.ibm.com/infocenter/pseries/v5r3/topic/com.ibm.aix.genprogc/doc/genprogc/manip_video_attrs.htm

;;; C prototypes

;; int attroff(int attrs);
;; int wattroff(WINDOW *win, int attrs);
;; int attron(int attrs);
;; int wattron(WINDOW *win, int attrs);
;; int attrset(int attrs);
;; int wattrset(WINDOW *win, int attrs);

;; int color_set(short color_pair_number, void* opts);
;; int wcolor_set(WINDOW *win, short color_pair_number, void* opts);

;; int standend(void);
;; int wstandend(WINDOW *win);
;; int standout(void);
;; int wstandout(WINDOW *win);

;; int attr_get(attr_t *attrs, short *pair, void *opts);
;; int wattr_get(WINDOW *win, attr_t *attrs, short *pair, void *opts);

;; int attr_off(attr_t attrs, void *opts);
;; int wattr_off(WINDOW *win, attr_t attrs, void *opts);
;; int attr_on(attr_t attrs, void *opts);
;; int wattr_on(WINDOW *win, attr_t attrs, void *opts);

;; int attr_set(attr_t attrs, short pair, void *opts);
;; int wattr_set(WINDOW *win, attr_t attrs, short pair, void *opts);

;; int chgat(int n, attr_t attr, short color, const void *opts)
;; int wchgat(WINDOW *win, int n, attr_t attr, short color, const void *opts)
;; int mvchgat(int y, int x, int n, attr_t attr, short color, const void *opts)
;; int mvwchgat(WINDOW *win, int y, int x, int n, attr_t attr, short color, const void *opts)

;;; Low-level CFFI wrappers

;; :unsigned-int should be the same size as :uint32, 4 bytes, the size of a chtype.
(cffi:defcfun ("attroff"     %attroff)     :int              (attrs :unsigned-int))
(cffi:defcfun ("wattroff"    %wattroff)    :int (win window) (attrs :unsigned-int))
(cffi:defcfun ("attron"      %attron)      :int              (attrs :unsigned-int))
(cffi:defcfun ("wattron"     %wattron)     :int (win window) (attrs :unsigned-int))
(cffi:defcfun ("attrset"     %attrset)     :int              (attrs :unsigned-int))
(cffi:defcfun ("wattrset"    %wattrset)    :int (win window) (attrs :unsigned-int))

(cffi:defcfun ("color_set"   %color-set)   :int              (color-pair-number :short) (opts (:pointer :void)))
(cffi:defcfun ("wcolor_set"  %wcolor-set)  :int (win window) (color-pair-number :short) (opts (:pointer :void)))

(cffi:defcfun ("standend"    %standend)    :int)
(cffi:defcfun ("wstandend"   %wstandend)   :int (win window))
(cffi:defcfun ("standout"    %standout)    :int)
(cffi:defcfun ("wstandout"   %wstandout)   :int (win window))

(cffi:defcfun ("attr_get"    %attr-get)    :int              (attrs (:pointer attr)) (pair (:pointer :short)) (opts (:pointer :void)))
(cffi:defcfun ("wattr_get"   %wattr-get)   :int (win window) (attrs (:pointer attr)) (pair (:pointer :short)) (opts (:pointer :void)))

(cffi:defcfun ("attr_off"    %attr-off)    :int              (attrs attr)               (opts (:pointer :void)))
(cffi:defcfun ("wattr_off"   %wattr-off)   :int (win window) (attrs attr)               (opts (:pointer :void)))
(cffi:defcfun ("attr_on"     %attr-on)     :int              (attrs attr)               (opts (:pointer :void)))
(cffi:defcfun ("wattr_on"    %wattr-on)    :int (win window) (attrs attr)               (opts (:pointer :void)))
(cffi:defcfun ("attr_set"    %attr-set)    :int              (attrs attr) (pair :short) (opts (:pointer :void)))
(cffi:defcfun ("wattr_set"   %wattr-set)   :int (win window) (attrs attr) (pair :short) (opts (:pointer :void)))

(cffi:defcfun ("chgat"       %chgat)       :int                                (n :int) (attr attr) (color :short) (opts (:pointer :void)))
(cffi:defcfun ("wchgat"      %wchgat)      :int (win window)                   (n :int) (attr attr) (color :short) (opts (:pointer :void)))
(cffi:defcfun ("mvchgat"     %mvchgat)     :int              (y :int) (x :int) (n :int) (attr attr) (color :short) (opts (:pointer :void)))
(cffi:defcfun ("mvwchgat"    %mvwchgat)    :int (win window) (y :int) (x :int) (n :int) (attr attr) (color :short) (opts (:pointer :void)))
