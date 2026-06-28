(in-package #:cl-tmux/test)

;;;; Overlay, popup, and menu state tests (src/overlay.lisp).
;;;;
;;;; Covers: overlay state, show-overlay, clear-overlay, overlay-lines,
;;;; overlay-scroll, and the popup and menu struct constructors.
;;;; These were split out of prompt-tests.lisp to match the source split
;;;; between prompt.lisp and overlay.lisp.

(def-suite overlay-suite :description "Dismissible overlay, popup, and menu state")
(in-suite overlay-suite)

;;; -- Dismissible overlay (list-keys help) ------------------------------------

(test overlay-inactive-by-default
  "With no overlay set, overlay-active-p is NIL and overlay-lines is empty."
  (let ((*overlay* nil))
    (assert-overlay-inactive)
    (is (null (overlay-lines)))))

(test overlay-show-splits-lines-and-clears
  "show-overlay activates a multi-line overlay; overlay-lines splits on newline;
   clear-overlay dismisses it."
  (let ((*overlay* nil))
    (show-overlay (format nil "line1~%line2~%line3"))
    (assert-overlay-active)
    (is (equal '("line1" "line2" "line3") (overlay-lines)))
    (clear-overlay)
    (assert-overlay-inactive)
    (is (null (overlay-lines)))))

(test overlay-single-line
  "A single-line overlay yields exactly one line (no trailing empty line)."
  (let ((*overlay* nil))
    (show-overlay "solo")
    (is (equal '("solo") (overlay-lines)))))

;;; -- Overlay scrolling -------------------------------------------------------
;;;
;;; overlay-scroll and *overlay-scroll-offset* are exported and used by
;;; events.lisp (j/k key bindings).  Clamping to [0, max-offset] is tested here.

(test overlay-scroll-down-advances-offset
  "overlay-scroll with a positive delta increases *overlay-scroll-offset*."
  (let ((*overlay* nil)
        (*overlay-scroll-offset* 0))
    (show-overlay (format nil "line1~%line2~%line3~%line4"))
    (overlay-scroll 1)
    (is (= 1 *overlay-scroll-offset*) "one scroll-down step must advance offset to 1")
    (overlay-scroll 1)
    (is (= 2 *overlay-scroll-offset*) "second step advances to 2")))

(test overlay-scroll-up-decreases-offset
  "overlay-scroll with a negative delta decreases *overlay-scroll-offset*."
  (let ((*overlay* nil)
        (*overlay-scroll-offset* 0))
    (show-overlay (format nil "line1~%line2~%line3"))
    (overlay-scroll 2)                         ; advance first
    (is (= 2 *overlay-scroll-offset*))
    (overlay-scroll -1)
    (is (= 1 *overlay-scroll-offset*) "one scroll-up step must decrease offset to 1")))

(test overlay-scroll-clamps-at-zero
  "overlay-scroll never makes *overlay-scroll-offset* negative (clamps at 0)."
  (let ((*overlay* nil)
        (*overlay-scroll-offset* 0))
    (show-overlay (format nil "line1~%line2"))
    (overlay-scroll -5)
    (is (= 0 *overlay-scroll-offset*)
        "scrolling up past the top must clamp at 0")))

(test overlay-scroll-clamps-at-max
  "overlay-scroll never exceeds (1- line-count) (clamps at max-offset)."
  (let ((*overlay* nil)
        (*overlay-scroll-offset* 0))
    ;; Three lines -> max-offset = 2.
    (show-overlay (format nil "line1~%line2~%line3"))
    (overlay-scroll 100)
    (is (= 2 *overlay-scroll-offset*)
        "scrolling far past the bottom must clamp at (1- line-count)")))

(test overlay-scroll-noop-when-no-overlay
  "overlay-scroll is a no-op when no overlay is active."
  (let ((*overlay* nil)
        (*overlay-scroll-offset* 0))
    (overlay-scroll 5)
    (is (= 0 *overlay-scroll-offset*)
        "offset must remain 0 with no active overlay")))

(test show-overlay-resets-scroll-offset
  "show-overlay resets *overlay-scroll-offset* to 0 regardless of prior value."
  (let ((*overlay* nil)
        (*overlay-scroll-offset* 99))
    (show-overlay "fresh")
    (is (= 0 *overlay-scroll-offset*)
        "show-overlay must reset scroll offset to 0")))

(test clear-overlay-resets-scroll-offset
  "clear-overlay resets *overlay-scroll-offset* to 0."
  (let ((*overlay* nil)
        (*overlay-scroll-offset* 0))
    (show-overlay (format nil "a~%b"))
    (overlay-scroll 1)
    (clear-overlay)
    (is (= 0 *overlay-scroll-offset*)
        "clear-overlay must reset scroll offset to 0")))

(test overlay-lines-trailing-newline
  "A trailing newline yields a final empty line: overlay-lines collects the
   empty segment after the last newline."
  (let ((*overlay* nil))
    (show-overlay (format nil "a~%"))
    (is (equal '("a" "") (overlay-lines))
        "text ending in newline produces a trailing empty line")
    (show-overlay (format nil "a~%b~%"))
    (is (equal '("a" "b" "") (overlay-lines)))))

(test overlay-scroll-zero-delta-is-noop
  "overlay-scroll with delta 0 leaves *overlay-scroll-offset* unchanged."
  (let ((*overlay* nil)
        (*overlay-scroll-offset* 0))
    (show-overlay (format nil "line1~%line2~%line3"))
    (overlay-scroll 1)
    (is (= 1 *overlay-scroll-offset*))
    (overlay-scroll 0)
    (is (= 1 *overlay-scroll-offset*)
        "delta 0 must leave the offset unchanged")))

(test show-overlay-empty-string
  "show-overlay with an empty string activates overlay-active-p and overlay-lines returns a single empty line."
  (let ((*overlay* nil)
        (*overlay-scroll-offset* 5))
    (show-overlay "")
    (assert-overlay-active
     "overlay must be active after show-overlay with empty string")
    (is (= 0 *overlay-scroll-offset*)
        "show-overlay must reset scroll offset even for empty string")
    (is (equal '("") (overlay-lines))
        "empty string produces a list with one empty line")))

(test clear-overlay-when-no-overlay-is-noop
  "clear-overlay when no overlay is active is a safe no-op."
  (let ((*overlay* nil)
        (*overlay-scroll-offset* 0))
    (finishes (clear-overlay))
    (assert-overlay-inactive
     "overlay must remain inactive after clear-overlay on nil overlay")
    (is (= 0 *overlay-scroll-offset*)
        "scroll offset must remain 0 after clear-overlay on nil overlay")))

;;; -- Popup struct constructors and accessors ---------------------------------
;;;
;;; make-popup, *active-popup*, and all popup-* accessors are exported from
;;; cl-tmux/prompt.  These tests exercise construction with default values and
;;; with explicitly supplied keyword arguments, plus the global *active-popup*
;;; session state variable.

(test make-popup-defaults
  "make-popup fills all slots to their documented defaults."
  (let ((p (make-popup)))
    (check-table (list (list (popup-x p)      0  "default x is 0")
                       (list (popup-y p)      0  "default y is 0")
                       (list (popup-width p)  40 "default width is 40")
                       (list (popup-height p) 10 "default height is 10")
                       (list (popup-screen p) nil "default screen is nil (text-only)")
                       (list (popup-pane p)   nil "default pane is nil (text-only)"))
                 :test #'equal)
    (is (string= "" (popup-title p))   "default title is empty string")
    (is (eq t (popup-close-on-exit p)) "default close-on-exit is T")))

(test make-popup-keyword-args
  "make-popup accepts keyword arguments that override all defaults."
  (let ((p (make-popup :x 5 :y 10 :width 80 :height 24
                       :title "Test Popup" :close-on-exit nil)))
    (check-table (list (list (popup-x p)      5  "x is 5")
                       (list (popup-y p)      10 "y is 10")
                       (list (popup-width p)  80 "width is 80")
                       (list (popup-height p) 24 "height is 24"))
                 :test #'equal)
    (is (string= "Test Popup" (popup-title p)))
    (is (null (popup-close-on-exit p)) "close-on-exit can be set to NIL")))

(test make-popup-with-non-nil-pane
  "make-popup :pane stores the pane reference; popup-pane returns it."
  (let* ((fake-pane (make-pane :id 42 :fd -1 :screen (make-screen 10 5)))
         (p (make-popup :pane fake-pane :title "live-popup")))
    (is (eq fake-pane (popup-pane p))
        "popup-pane must return the supplied pane, not NIL")
    (is (= 42 (pane-id (popup-pane p)))
        "the pane stored in the popup is the exact object passed in")))

(test active-popup-global-state
  "Setting *active-popup* persists across accesses (session-level state)."
  (let ((*active-popup* nil))
    (is (null *active-popup*) "starts nil")
    (let ((p (make-popup :title "session popup")))
      (setf *active-popup* p)
      (is (eq p *active-popup*)
          "*active-popup* holds the set popup")
      (is (string= "session popup" (popup-title *active-popup*))
          "accessor works via the global")
      (setf *active-popup* nil)
      (is (null *active-popup*) "reset to nil clears it"))))

;;; -- Menu struct constructors and accessors ----------------------------------
;;;
;;; make-menu, *active-menu*, and all menu-* accessors are exported from
;;; cl-tmux/prompt.  These tests cover construction with defaults, with explicit
;;; arguments, and the global *active-menu* session-state variable.

(test make-menu-defaults
  "make-menu fills all slots to their documented defaults."
  (let ((m (make-menu)))
    (is (string= "" (menu-title m))      "default title is empty string")
    (is (null (menu-items m))            "default items list is NIL")
    (is (= 0  (menu-selected-index m))   "default selected-index is 0")))

(test make-menu-keyword-args
  "make-menu accepts keyword arguments that override all defaults."
  (let ((items '(("Option A" . :a) ("Option B" . :b) ("Option C" . :c))))
    (let ((m (make-menu :title "Choose" :items items :selected-index 2)))
      (is (string= "Choose" (menu-title m)))
      (is (equal items (menu-items m))
          "menu-items returns the full items list")
      (is (= 2 (menu-selected-index m))
          "selected-index is set to 2"))))

(test menu-selected-index-mutable
  "setf on menu-selected-index updates the struct slot."
  (let ((m (make-menu :items '((:a) (:b) (:c)) :selected-index 0)))
    (is (= 0 (menu-selected-index m)))
    (setf (menu-selected-index m) 2)
    (is (= 2 (menu-selected-index m)) "index updated to 2")
    (setf (menu-selected-index m) 0)
    (is (= 0 (menu-selected-index m)) "index wrapped back to 0")))

(test active-menu-global-state
  "Setting *active-menu* persists across accesses (session-level state)."
  (let ((*active-menu* nil))
    (is (null *active-menu*) "starts nil")
    (let ((m (make-menu :title "session menu" :items '(("X" . :x)))))
      (setf *active-menu* m)
      (is (eq m *active-menu*)
          "*active-menu* holds the set menu")
      (is (string= "session menu" (menu-title *active-menu*))
          "accessor works via the global")
      (setf *active-menu* nil)
      (is (null *active-menu*) "reset to nil clears it"))))

;;; -- popup-p predicate -------------------------------------------------------

(test popup-p-recognises-popup-struct
  "popup-p returns T for a POPUP struct and NIL for any other value."
  (let ((p (make-popup)))
    (is (popup-p p) "popup-p must return T for a make-popup result")
    (dolist (val (list nil 42 "" (make-menu)))
      (is (not (popup-p val)) "popup-p must return NIL for ~S" val))))

;;; -- menu-p predicate --------------------------------------------------------

(test menu-p-recognises-menu-struct
  "menu-p returns T for a MENU struct and NIL for any other value."
  (let ((m (make-menu)))
    (is (menu-p m) "menu-p must return T for a make-menu result")
    (dolist (val (list nil 42 "" (make-popup)))
      (is (not (menu-p val)) "menu-p must return NIL for ~S" val))))

;;; -- +default-popup-width+ / +default-popup-height+ constants ----------------

(test default-popup-dimension-constants
  "The named defconstants match the documented default slot values."
  (is (= +default-popup-width+  (popup-width  (make-popup)))
      "+default-popup-width+ must equal the default width slot")
  (is (= +default-popup-height+ (popup-height (make-popup)))
      "+default-popup-height+ must equal the default height slot")
  (is (= 40 +default-popup-width+)  "canonical popup width is 40 columns")
  (is (= 10 +default-popup-height+) "canonical popup height is 10 rows"))

;;; -- overlay-shown-at accessor -----------------------------------------------
;;;
;;; overlay-shown-at is the public accessor for *overlay-shown-at*.
;;; These tests call the function directly to ensure a regression that breaks
;;; the accessor itself is detected independently of reading the variable.

(test overlay-shown-at-returns-zero-initially
  "overlay-shown-at returns 0 when no transient overlay has been shown."
  (let ((*overlay-shown-at* 0))
    (is (= 0 (overlay-shown-at))
        "overlay-shown-at must return 0 in the initial state")))

(test overlay-shown-at-returns-timestamp-after-show-transient
  "overlay-shown-at returns the timestamp passed to show-transient-overlay."
  (let ((*overlay* nil)
        (*overlay-shown-at* 0)
        (*overlay-scroll-offset* 0)
        (*display-panes-active* nil))
    (show-transient-overlay "msg" :timestamp 77)
    (is (= 77 (overlay-shown-at))
        "overlay-shown-at must return the timestamp set by show-transient-overlay")))

(test overlay-shown-at-updated-by-show-display-panes-overlay
  "overlay-shown-at returns the timestamp set by show-display-panes-overlay."
  (let ((*overlay* nil)
        (*overlay-shown-at* 0)
        (*overlay-scroll-offset* 0)
        (*display-panes-active* nil))
    (show-display-panes-overlay "nums" :timestamp 55)
    (is (= 55 (overlay-shown-at))
        "overlay-shown-at must return the timestamp set by show-display-panes-overlay")))

;;; -- show-transient-overlay --------------------------------------------------
;;;
;;; Tests for show-transient-overlay via its public timestamp parameter, so
;;; the deterministic timestamp injection path is exercised without having to
;;; directly assign *overlay-shown-at*.

(test show-transient-overlay-activates-and-stamps-timestamp
  "show-transient-overlay activates an overlay and records the supplied timestamp
   accessible via overlay-shown-at; *display-panes-active* remains NIL."
  (let ((*overlay* nil)
        (*overlay-shown-at* 0)
        (*overlay-scroll-offset* 0)
        (*display-panes-active* nil))
    (show-transient-overlay "transient msg" :timestamp 42)
    (assert-overlay-active "show-transient-overlay must activate the overlay")
    (is (= 42 (overlay-shown-at))
        "show-transient-overlay must record the supplied timestamp")
    (is (null *display-panes-active*)
        "show-transient-overlay must leave *display-panes-active* NIL")))

(test show-transient-overlay-resets-scroll-offset
  "show-transient-overlay resets *overlay-scroll-offset* to 0."
  (let ((*overlay* nil)
        (*overlay-shown-at* 0)
        (*overlay-scroll-offset* 7)
        (*display-panes-active* nil))
    (show-transient-overlay "msg" :timestamp 1)
    (is (= 0 *overlay-scroll-offset*)
        "show-transient-overlay must reset scroll offset to 0")))

(test show-transient-overlay-default-timestamp-is-current-time
  "show-transient-overlay with no :timestamp argument uses (get-universal-time).
   The recorded timestamp accessible via overlay-shown-at must be >= the time before the call."
  (let ((*overlay* nil)
        (*overlay-shown-at* 0)
        (*overlay-scroll-offset* 0)
        (*display-panes-active* nil))
    (let ((before (get-universal-time)))
      (show-transient-overlay "auto-ts")
      (is (>= (overlay-shown-at) before)
          "default timestamp must be >= the universal-time before the call"))))

;;; -- show-display-panes-overlay ----------------------------------------------
;;;
;;; show-display-panes-overlay is like show-transient-overlay but also sets
;;; *display-panes-active* to T so the renderer draws per-pane numbers.

(test show-display-panes-overlay-activates-and-sets-display-panes
  "show-display-panes-overlay activates an overlay, sets *display-panes-active* T,
   and records the supplied timestamp accessible via overlay-shown-at."
  (let ((*overlay* nil)
        (*overlay-shown-at* 0)
        (*overlay-scroll-offset* 0)
        (*display-panes-active* nil))
    (show-display-panes-overlay "pane-nums" :timestamp 99)
    (assert-overlay-active "show-display-panes-overlay must activate the overlay")
    (is (eq t *display-panes-active*)
        "show-display-panes-overlay must set *display-panes-active* to T")
    (is (= 99 (overlay-shown-at))
        "show-display-panes-overlay must record the supplied timestamp")))

(test show-overlay-clears-display-panes
  "show-overlay (the non-transient variant) always sets *display-panes-active* to NIL,
   even when display-panes was previously active."
  (let ((*overlay* nil)
        (*overlay-shown-at* 0)
        (*overlay-scroll-offset* 0)
        (*display-panes-active* nil))
    (show-display-panes-overlay "pane-nums" :timestamp 1)
    (is (eq t *display-panes-active*) "precondition: display-panes active")
    (show-overlay "plain overlay")
    (is (null *display-panes-active*)
        "show-overlay must clear *display-panes-active*")))

(test clear-overlay-clears-display-panes
  "clear-overlay resets *display-panes-active* to NIL."
  (let ((*overlay* nil)
        (*overlay-shown-at* 0)
        (*overlay-scroll-offset* 0)
        (*display-panes-active* nil))
    (show-display-panes-overlay "nums" :timestamp 1)
    (is (eq t *display-panes-active*) "precondition: display-panes active")
    (clear-overlay)
    (is (null *display-panes-active*)
        "clear-overlay must reset *display-panes-active* to NIL")))

(test show-transient-overlay-replaces-display-panes-overlay
  "show-transient-overlay (display-panes=NIL) after show-display-panes-overlay
   clears *display-panes-active*."
  (let ((*overlay* nil)
        (*overlay-shown-at* 0)
        (*overlay-scroll-offset* 0)
        (*display-panes-active* nil))
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
