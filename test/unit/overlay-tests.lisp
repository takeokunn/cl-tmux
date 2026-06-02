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
    (is (not (overlay-active-p)))
    (is (null (overlay-lines)))))

(test overlay-show-splits-lines-and-clears
  "show-overlay activates a multi-line overlay; overlay-lines splits on newline;
   clear-overlay dismisses it."
  (let ((*overlay* nil))
    (show-overlay (format nil "line1~%line2~%line3"))
    (is (overlay-active-p))
    (is (equal '("line1" "line2" "line3") (overlay-lines)))
    (clear-overlay)
    (is (not (overlay-active-p)))
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

;;; -- Popup struct constructors and accessors ---------------------------------
;;;
;;; make-popup, *active-popup*, and all popup-* accessors are exported from
;;; cl-tmux/prompt.  These tests exercise construction with default values and
;;; with explicitly supplied keyword arguments, plus the global *active-popup*
;;; session state variable.

(test make-popup-defaults
  "make-popup fills all slots to their documented defaults."
  (let ((p (make-popup)))
    (is (= 0  (popup-x p))      "default x is 0")
    (is (= 0  (popup-y p))      "default y is 0")
    (is (= 40 (popup-width p))  "default width is 40")
    (is (= 10 (popup-height p)) "default height is 10")
    (is (null (popup-screen p)) "default screen is nil (text-only)")
    (is (null (popup-pane p))   "default pane is nil (text-only)")
    (is (string= "" (popup-title p))       "default title is empty string")
    (is (eq t (popup-close-on-exit p))     "default close-on-exit is T")))

(test make-popup-keyword-args
  "make-popup accepts keyword arguments that override all defaults."
  (let ((p (make-popup :x 5 :y 10 :width 80 :height 24
                       :title "Test Popup" :close-on-exit nil)))
    (is (= 5  (popup-x p)))
    (is (= 10 (popup-y p)))
    (is (= 80 (popup-width p)))
    (is (= 24 (popup-height p)))
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
