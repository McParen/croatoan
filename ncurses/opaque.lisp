(in-package :de.anvi.ncurses)

;;; opaque
;;; curses window properties
;;; http://invisible-island.net/ncurses/man/curs_opaque.3x.html

;;; C prototypes

;; bool is_cleared(const WINDOW *win);
;; bool is_idcok(const WINDOW *win);
;; bool is_idlok(const WINDOW *win);
;; bool is_immedok(const WINDOW *win);
;; bool is_keypad(const WINDOW *win);
;; bool is_leaveok(const WINDOW *win);
;; bool is_nodelay(const WINDOW *win);
;; bool is_notimeout(const WINDOW *win);
;; bool is_pad(const WINDOW *win);
;; bool is_scrollok(const WINDOW *win);
;; bool is_subwin(const WINDOW *win);
;; bool is_syncok(const WINDOW *win);
;; WINDOW *wgetparent(const WINDOW *win);
;; int wgetscrreg(const WINDOW *win, int *top, int *bottom);

;;; Low-level CFFI wrappers

(defcfun ("is_cleared"   %is-cleared)   :boolean (win window))
(defcfun ("is_idcok"     %is-idcok)     :boolean (win window))
(defcfun ("is_idlok"     %is-idlok)     :boolean (win window))
(defcfun ("is_immedok"   %is-immedok)   :boolean (win window))
(defcfun ("is_keypad"    %is-keypad)    :boolean (win window))
(defcfun ("is_leaveok"   %is-leaveok)   :boolean (win window))
(defcfun ("is_nodelay"   %is-nodelay)   :boolean (win window))
(defcfun ("is_notimeout" %is-notimeout) :boolean (win window))
(defcfun ("is_pad"       %is-pad)       :boolean (win window))
(defcfun ("is_scrollok"  %is-scrollok)  :boolean (win window))
(defcfun ("is_subwin"    %is-subwin)    :boolean (win window))
(defcfun ("is_syncok"    %is-syncok)    :boolean (win window))

(defcfun ("wgetparent"   %wgetparent)   window   (win window))
(defcfun ("wgetscrreg"   %wgetscrreg)   :int     (win window) (top (:pointer :int)) (bottom (:pointer :int)))
