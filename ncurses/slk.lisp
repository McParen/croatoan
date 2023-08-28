(in-package :de.anvi.ncurses)

;;; slk
;;; curses soft label routines
;;; http://invisible-island.net/ncurses/man/curs_slk.3x.html

;;; C prototypes

;; int slk_init(int fmt);

;; int slk_set(int labnum, const char *label, int fmt);
;; int slk_wset(int labnum, const wchar_t *label, int fmt);

;; char *slk_label(int labnum);

;; int slk_refresh(void);
;; int slk_noutrefresh(void);
;; int slk_clear(void);
;; int slk_restore(void);
;; int slk_touch(void);

;; int slk_attron(const chtype attrs);
;; int slk_attroff(const chtype attrs);
;; int slk_attrset(const chtype attrs);

;; int slk_attr_on(attr_t attrs, void *opts);
;; int slk_attr_off(const attr_t attrs, void *opts);
;; int slk_attr_set(const attr_t attrs, short color_pair, void *opts);

;; attr_t slk_attr(void);
;; int slk_color(short color_pair);
;; int extended_slk_color(int pair);

;;; Low-level CFFI wrappers

(cffi:defcfun ("slk_init"        slk-init)        :int    (fmt :int))

(cffi:defcfun ("slk_set"         slk-set)         :int    (labnum :int) (label :string)            (fmt :int))
(cffi:defcfun ("slk_wset"        slk-wset)        :int    (labnum :int) (label (:pointer wchar_t)) (fmt :int))

(cffi:defcfun ("slk_label"       slk-label)       :string (labnum :int))

(cffi:defcfun ("slk_refresh"     slk-refresh)     :int)
(cffi:defcfun ("slk_noutrefresh" slk-noutrefresh) :int)
(cffi:defcfun ("slk_clear"       slk-clear)       :int)
(cffi:defcfun ("slk_restore"     slk-restore)     :int)
(cffi:defcfun ("slk_touch"       slk-touch)       :int)

(cffi:defcfun ("slk_attron"      slk-attron)      :int    (attrs chtype))
(cffi:defcfun ("slk_attroff"     slk-attroff)     :int    (attrs chtype))
(cffi:defcfun ("slk_attrset"     slk-attrset)     :int    (attrs chtype))

(cffi:defcfun ("slk_attr_on"     slk-attr-on)     :int    (attrs attr_t)                     (opts (:pointer :void)))
(cffi:defcfun ("slk_attr_off"    slk-attr-off)    :int    (attrs attr_t)                     (opts (:pointer :void)))
(cffi:defcfun ("slk_attr_set"    slk-attr-set)    :int    (attrs attr_t) (color-pair :short) (opts (:pointer :void)))

(cffi:defcfun ("slk_attr"        slk-attr)        attr_t)
(cffi:defcfun ("slk_color"       slk-color)       :int    (color-pair :short))

(cffi:defcfun ("extended_slk_color" extended-slk-color) :int (pair :int))
