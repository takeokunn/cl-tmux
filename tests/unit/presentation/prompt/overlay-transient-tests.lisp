(in-package #:cl-tmux/test)

;;;; Transient overlay and popup/menu lifecycle tests (src/overlay.lisp).

(describe "overlay-suite"

  ;;; -- overlay-shown-at accessor -----------------------------------------------

  ;; overlay-shown-at returns 0 when no transient overlay has been shown.
  (it "overlay-shown-at-returns-zero-initially"
    (with-clean-overlay
      (expect (= 0 (overlay-shown-at)))))

  ;; overlay-shown-at returns the timestamp passed to show-transient-overlay.
  (it "overlay-shown-at-returns-timestamp-after-show-transient"
    (with-clean-overlay
      (show-transient-overlay "msg" :timestamp 77)
      (expect (= 77 (overlay-shown-at)))))

  ;; overlay-shown-at returns the timestamp set by show-display-panes-overlay.
  (it "overlay-shown-at-updated-by-show-display-panes-overlay"
    (with-clean-overlay
      (show-display-panes-overlay "nums" :timestamp 55)
      (expect (= 55 (overlay-shown-at)))))

  ;;; -- show-transient-overlay --------------------------------------------------

  ;; show-transient-overlay activates an overlay and records the supplied timestamp
  ;; accessible via overlay-shown-at; *display-panes-active* remains NIL.
  (it "show-transient-overlay-activates-and-stamps-timestamp"
    (with-clean-overlay
      (show-transient-overlay "transient msg" :timestamp 42)
      (assert-overlay-active "show-transient-overlay must activate the overlay")
      (expect (= 42 (overlay-shown-at)))
      (expect (null *display-panes-active*))))

  ;; show-transient-overlay resets *overlay-scroll-offset* to 0.
  (it "show-transient-overlay-resets-scroll-offset"
    (with-clean-overlay
      (let ((*overlay-scroll-offset* 7))
        (show-transient-overlay "msg" :timestamp 1)
        (expect (= 0 *overlay-scroll-offset*)))))

  ;; show-transient-overlay with no :timestamp argument uses (get-universal-time).
  ;; The recorded timestamp accessible via overlay-shown-at must be >= the time before the call.
  (it "show-transient-overlay-default-timestamp-is-current-time"
    (with-clean-overlay
      (let ((before (get-universal-time)))
        (show-transient-overlay "auto-ts")
        (expect (>= (overlay-shown-at) before)))))

  ;;; -- show-display-panes-overlay ----------------------------------------------

  ;; show-display-panes-overlay activates an overlay, sets *display-panes-active* T,
  ;; and records the supplied timestamp accessible via overlay-shown-at.
  (it "show-display-panes-overlay-activates-and-sets-display-panes"
    (with-clean-overlay
      (show-display-panes-overlay "pane-nums" :timestamp 99)
      (assert-overlay-active "show-display-panes-overlay must activate the overlay")
      (expect (eq t *display-panes-active*))
      (expect (= 99 (overlay-shown-at)))))

  ;; show-overlay (the non-transient variant) always sets *display-panes-active* to NIL,
  ;; even when display-panes was previously active.
  (it "show-overlay-clears-display-panes"
    (with-clean-overlay
      (show-display-panes-overlay "pane-nums" :timestamp 1)
      (expect (eq t *display-panes-active*))
      (show-overlay "plain overlay")
      (expect (null *display-panes-active*))))

  ;; clear-overlay resets *display-panes-active* to NIL.
  (it "clear-overlay-clears-display-panes"
    (with-clean-overlay
      (show-display-panes-overlay "nums" :timestamp 1)
      (expect (eq t *display-panes-active*))
      (clear-overlay)
      (expect (null *display-panes-active*))))

  ;; show-transient-overlay (display-panes=NIL) after show-display-panes-overlay
  ;; clears *display-panes-active*.
  (it "show-transient-overlay-replaces-display-panes-overlay"
    (with-clean-overlay
      (show-display-panes-overlay "nums" :timestamp 1)
      (expect (eq t *display-panes-active*))
      (show-transient-overlay "msg" :timestamp 2)
      (expect (null *display-panes-active*))))

  ;;; -- show-popup / close-popup / popup-active-p lifecycle --------------------

  ;; show-popup sets *active-popup* and popup-active-p returns T.
  (it "show-popup-registers-popup-and-popup-active-p-is-true"
    (let ((*active-popup* nil))
      (let ((p (make-popup :title "test")))
        (show-popup p)
        (expect (eq p *active-popup*))
        (expect (popup-active-p)))))

  ;; close-popup clears *active-popup* and popup-active-p returns NIL.
  (it "close-popup-dismisses-popup-and-popup-active-p-is-false"
    (let ((*active-popup* nil))
      (show-popup (make-popup :title "lifecycle"))
      (expect (popup-active-p))
      (close-popup)
      (expect (null *active-popup*))
      (expect (not (popup-active-p)))))

  ;; close-popup is a safe no-op when no popup is currently active.
  (it "close-popup-when-no-popup-is-noop"
    (let ((*active-popup* nil))
      (finishes (close-popup) "close-popup on nil *active-popup* must not signal")
      (expect (not (popup-active-p)))))

  ;; show-popup on an already-active popup silently replaces it.
  (it "show-popup-replaces-existing-popup"
    (let ((*active-popup* nil))
      (let* ((p1 (make-popup :title "first"))
             (p2 (make-popup :title "second")))
        (show-popup p1)
        (show-popup p2)
        (expect (eq p2 *active-popup*))
        (expect (popup-active-p)))))

  ;;; -- show-menu / close-menu / menu-active-p lifecycle -----------------------

  ;; show-menu sets *active-menu* and menu-active-p returns T.
  (it "show-menu-registers-menu-and-menu-active-p-is-true"
    (let ((*active-menu* nil))
      (let ((m (make-menu :title "choose")))
        (show-menu m)
        (expect (eq m *active-menu*))
        (expect (menu-active-p)))))

  ;; close-menu clears *active-menu* and menu-active-p returns NIL.
  (it "close-menu-dismisses-menu-and-menu-active-p-is-false"
    (let ((*active-menu* nil))
      (show-menu (make-menu :title "lifecycle"))
      (expect (menu-active-p))
      (close-menu)
      (expect (null *active-menu*))
      (expect (not (menu-active-p)))))

  ;; close-menu is a safe no-op when no menu is currently active.
  (it "close-menu-when-no-menu-is-noop"
    (let ((*active-menu* nil))
      (finishes (close-menu) "close-menu on nil *active-menu* must not signal")
      (expect (not (menu-active-p))))))
