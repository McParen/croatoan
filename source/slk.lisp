(in-package :croatoan)

;;; slk
;;; curses soft label routines
;;; http://invisible-island.net/ncurses/man/curs_slk.3x.html

;;; C prototypes

;; int slk_init(int fmt);
;; int slk_set(int labnum, const char *label, int fmt);
;; int slk_refresh(void);
;; int slk_noutrefresh(void);
;; char *slk_label(int labnum);
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
;; int slk_wset(int labnum, const wchar_t *label, int fmt);

;;; Low-level C functions

(defcfun ("slk_init"        %slk-init)        :int    (fmt :int))
(defcfun ("slk_set"         %slk-set)         :int    (labnum :int) (label :string) (fmt :int))
(defcfun ("slk_refresh"     %slk-refresh)     :int)
(defcfun ("slk_noutrefresh" %slk-noutrefresh) :int)
(defcfun ("slk_label"       %slk-label)       :string (labnum :int))
(defcfun ("slk_clear"       %slk-clear)       :int)
(defcfun ("slk_restore"     %slk-restore)     :int)
(defcfun ("slk_touch"       %slk-touch)       :int)

(defcfun ("slk_attron"      %slk-attron)      :int    (attrs chtype))
(defcfun ("slk_attroff"     %slk-attroff)     :int    (attrs chtype))
(defcfun ("slk_attrset"     %slk-attrset)     :int    (attrs chtype))

(defcfun ("slk_attr_on"     %slk-attr-on)     :int    (attrs attr)                     (opts (:pointer :void)))
(defcfun ("slk_attr_off"    %slk-attr-off)    :int    (attrs attr)                     (opts (:pointer :void)))
(defcfun ("slk_attr_set"    %slk-attr-set)    :int    (attrs attr) (color-pair :short) (opts (:pointer :void)))

(defcfun ("slk_attr"        %slk-attr)        attr)
(defcfun ("slk_color"       %slk-color)       :int    (color-pair :short))

;;; High-level Lisp wrappers

;;; NOTES

;;; TODOs
