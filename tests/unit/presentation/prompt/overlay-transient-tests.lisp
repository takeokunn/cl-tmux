(in-package #:cl-tmux/test)

;;;; Transient overlay and popup/menu lifecycle tests (src/overlay.lisp).

(in-suite overlay-suite)

;;; -- overlay-shown-at accessor -----------------------------------------------

(test overlay-shown-at-returns-zero-initially
  "overlay-shown-at returns 0 when no transient overlay has been shown."
  (with-clean-overlay
    (is (= 0 (overlay-shown-at))
        "overlay-shown-at must return 0 in the initial state")))

(test overlay-shown-at-returns-timestamp-after-show-transient
  "overlay-shown-at returns the timestamp passed to show-transient-overlay."
  (with-clean-overlay
    (show-transient-overlay "msg" :timestamp 77)
    (is (= 77 (overlay-shown-at))
        "overlay-shown-at must return the timestamp set by show-transient-overlay")))

(test overlay-shown-at-updated-by-show-display-panes-overlay
  "overlay-shown-at returns the timestamp set by show-display-panes-overlay."
  (with-clean-overlay
    (show-display-panes-overlay "nums" :timestamp 55)
    (is (= 55 (overlay-shown-at))
        "overlay-shown-at must return the timestamp set by show-display-panes-overlay")))

;;; -- show-transient-overlay --------------------------------------------------

(test show-transient-overlay-activates-and-stamps-timestamp
  "show-transient-overlay activates an overlay and records the supplied timestamp
   accessible via overlay-shown-at; *display-panes-active* remains NIL."
  (with-clean-overlay
    (show-transient-overlay "transient msg" :timestamp 42)
    (assert-overlay-active "show-transient-overlay must activate the overlay")
    (is (= 42 (overlay-shown-at))
        "show-transient-overlay must record the supplied timestamp")
    (is (null *display-panes-active*)
        "show-transient-overlay must leave *display-panes-active* NIL")))

(test show-transient-overlay-resets-scroll-offset
  "show-transient-overlay resets *overlay-scroll-offset* to 0."
  (with-clean-overlay
    (let ((*overlay-scroll-offset* 7))
      (show-transient-overlay "msg" :timestamp 1)
      (is (= 0 *overlay-scroll-offset*)
          "show-transient-overlay must reset scroll offset to 0"))))

(test show-transient-overlay-default-timestamp-is-current-time
  "show-transient-overlay with no :timestamp argument uses (get-universal-time).
   The recorded timestamp accessible via overlay-shown-at must be >= the time before the call."
  (with-clean-overlay
    (let ((before (get-universal-time)))
      (show-transient-overlay "auto-ts")
      (is (>= (overlay-shown-at) before)
          "default timestamp must be >= the universal-time before the call"))))

;;; -- show-display-panes-overlay ----------------------------------------------

(test show-display-panes-overlay-activates-and-sets-display-panes
  "show-display-panes-overlay activates an overlay, sets *display-panes-active* T,
   and records the supplied timestamp accessible via overlay-shown-at."
  (with-clean-overlay
    (show-display-panes-overlay "pane-nums" :timestamp 99)
    (assert-overlay-active "show-display-panes-overlay must activate the overlay")
    (is (eq t *display-panes-active*)
        "show-display-panes-overlay must set *display-panes-active* to T")
    (is (= 99 (overlay-shown-at))
        "show-display-panes-overlay must record the supplied timestamp")))

(test show-overlay-clears-display-panes
  "show-overlay (the non-transient variant) always sets *display-panes-active* to NIL,
   even when display-panes was previously active."
  (with-clean-overlay
    (show-display-panes-overlay "pane-nums" :timestamp 1)
    (is (eq t *display-panes-active*) "precondition: display-panes active")
    (show-overlay "plain overlay")
    (is (null *display-panes-active*)
        "show-overlay must clear *display-panes-active*")))

(test clear-overlay-clears-display-panes
  "clear-overlay resets *display-panes-active* to NIL."
  (with-clean-overlay
    (show-display-panes-overlay "nums" :timestamp 1)
    (is (eq t *display-panes-active*) "precondition: display-panes active")
    (clear-overlay)
    (is (null *display-panes-active*)
        "clear-overlay must reset *display-panes-active* to NIL")))

(test show-transient-overlay-replaces-display-panes-overlay
  "show-transient-overlay (display-panes=NIL) after show-display-panes-overlay
   clears *display-panes-active*."
  (with-clean-overlay
    (show-display-panes-overlay "nums" :timestamp 1)
    (is (eq t *display-panes-active*) "precondition")
    (show-transient-overlay "msg" :timestamp 2)
    (is (null *display-panes-active*)
        "show-transient-overlay must clear *display-panes-active*")))

;;; -- show-popup / close-popup / popup-active-p lifecycle --------------------

(test show-popup-registers-popup-and-popup-active-p-is-true
  "show-popup sets *active-popup* and popup-active-p returns T."
  (let ((*active-popup* nil))
    (let ((p (make-popup :title "test")))
      (show-popup p)
      (is (eq p *active-popup*) "show-popup must register the popup in *active-popup*")
      (is (popup-active-p) "popup-active-p must be T after show-popup"))))

(test close-popup-dismisses-popup-and-popup-active-p-is-false
  "close-popup clears *active-popup* and popup-active-p returns NIL."
  (let ((*active-popup* nil))
    (show-popup (make-popup :title "lifecycle"))
    (is (popup-active-p) "precondition: popup must be active")
    (close-popup)
    (is (null *active-popup*) "close-popup must clear *active-popup*")
    (is (not (popup-active-p)) "popup-active-p must be NIL after close-popup")))

(test close-popup-when-no-popup-is-noop
  "close-popup is a safe no-op when no popup is currently active."
  (let ((*active-popup* nil))
    (finishes (close-popup) "close-popup on nil *active-popup* must not signal")
    (is (not (popup-active-p)) "popup-active-p must remain NIL")))

(test show-popup-replaces-existing-popup
  "show-popup on an already-active popup silently replaces it."
  (let ((*active-popup* nil))
    (let* ((p1 (make-popup :title "first"))
           (p2 (make-popup :title "second")))
      (show-popup p1)
      (show-popup p2)
      (is (eq p2 *active-popup*)
          "second show-popup must replace the first popup")
      (is (popup-active-p) "popup must still be active"))))

;;; -- show-menu / close-menu / menu-active-p lifecycle -----------------------

(test show-menu-registers-menu-and-menu-active-p-is-true
  "show-menu sets *active-menu* and menu-active-p returns T."
  (let ((*active-menu* nil))
    (let ((m (make-menu :title "choose")))
      (show-menu m)
      (is (eq m *active-menu*) "show-menu must register the menu in *active-menu*")
      (is (menu-active-p) "menu-active-p must be T after show-menu"))))

(test close-menu-dismisses-menu-and-menu-active-p-is-false
  "close-menu clears *active-menu* and menu-active-p returns NIL."
  (let ((*active-menu* nil))
    (show-menu (make-menu :title "lifecycle"))
    (is (menu-active-p) "precondition: menu must be active")
    (close-menu)
    (is (null *active-menu*) "close-menu must clear *active-menu*")
    (is (not (menu-active-p)) "menu-active-p must be NIL after close-menu")))

(test close-menu-when-no-menu-is-noop
  "close-menu is a safe no-op when no menu is currently active."
  (let ((*active-menu* nil))
    (finishes (close-menu) "close-menu on nil *active-menu* must not signal")
    (is (not (menu-active-p)) "menu-active-p must remain NIL")))
