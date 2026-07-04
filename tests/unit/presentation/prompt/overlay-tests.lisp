(in-package #:cl-tmux/test)

;;;; Overlay, popup, and menu state tests (src/overlay.lisp).
;;;;
;;;; Covers: overlay state, show-overlay, clear-overlay, overlay-lines,
;;;; and overlay-scroll.
;;;;
;;;; All overlay tests use with-clean-overlay (from helpers-input-fixtures.lisp) to
;;;; guarantee no leaked *overlay* / *overlay-scroll-offset* /
;;;; *overlay-shown-at* / *display-panes-active* state between tests,
;;;; mirroring the with-clean-prompt convention in prompt-tests.lisp.

(def-suite overlay-suite :description "Dismissible overlay, popup, and menu state")
(in-suite overlay-suite)

;;; -- Dismissible overlay (list-keys help) ------------------------------------

(test overlay-inactive-by-default
  "With no overlay set, overlay-active-p is NIL and overlay-lines is empty."
  (with-clean-overlay
    (assert-overlay-inactive)
    (is (null (overlay-lines)))))

(test overlay-show-splits-lines-and-clears
  "show-overlay activates a multi-line overlay; overlay-lines splits on newline;
   clear-overlay dismisses it."
  (with-clean-overlay
    (show-overlay (format nil "line1~%line2~%line3"))
    (assert-overlay-active)
    (is (equal '("line1" "line2" "line3") (overlay-lines)))
    (clear-overlay)
    (assert-overlay-inactive)
    (is (null (overlay-lines)))))

(test overlay-single-line
  "A single-line overlay yields exactly one line (no trailing empty line)."
  (with-clean-overlay
    (show-overlay "solo")
    (is (equal '("solo") (overlay-lines)))))

;;; -- Overlay scrolling -------------------------------------------------------
;;;
;;; overlay-scroll and *overlay-scroll-offset* are exported and used by
;;; events.lisp (j/k key bindings).  Clamping to [0, max-offset] is tested here.

(test overlay-scroll-down-advances-offset
  "overlay-scroll with a positive delta increases *overlay-scroll-offset*."
  (with-clean-overlay
    (show-overlay (format nil "line1~%line2~%line3~%line4"))
    (overlay-scroll 1)
    (is (= 1 *overlay-scroll-offset*) "one scroll-down step must advance offset to 1")
    (overlay-scroll 1)
    (is (= 2 *overlay-scroll-offset*) "second step advances to 2")))

(test overlay-scroll-up-decreases-offset
  "overlay-scroll with a negative delta decreases *overlay-scroll-offset*."
  (with-clean-overlay
    (show-overlay (format nil "line1~%line2~%line3"))
    (overlay-scroll 2)                         ; advance first
    (is (= 2 *overlay-scroll-offset*))
    (overlay-scroll -1)
    (is (= 1 *overlay-scroll-offset*) "one scroll-up step must decrease offset to 1")))

(test overlay-scroll-clamps-at-zero
  "overlay-scroll never makes *overlay-scroll-offset* negative (clamps at 0)."
  (with-clean-overlay
    (show-overlay (format nil "line1~%line2"))
    (overlay-scroll -5)
    (is (= 0 *overlay-scroll-offset*)
        "scrolling up past the top must clamp at 0")))

(test overlay-scroll-clamps-at-max
  "overlay-scroll never exceeds (1- line-count) (clamps at max-offset)."
  (with-clean-overlay
    ;; Three lines -> max-offset = 2.
    (show-overlay (format nil "line1~%line2~%line3"))
    (overlay-scroll 100)
    (is (= 2 *overlay-scroll-offset*)
        "scrolling far past the bottom must clamp at (1- line-count)")))

(test overlay-scroll-noop-when-no-overlay
  "overlay-scroll is a no-op when no overlay is active."
  (with-clean-overlay
    (overlay-scroll 5)
    (is (= 0 *overlay-scroll-offset*)
        "offset must remain 0 with no active overlay")))

(test show-overlay-resets-scroll-offset
  "show-overlay resets *overlay-scroll-offset* to 0 regardless of prior value."
  (with-clean-overlay
    (let ((*overlay-scroll-offset* 99))
      (show-overlay "fresh")
      (is (= 0 *overlay-scroll-offset*)
          "show-overlay must reset scroll offset to 0"))))

(test clear-overlay-resets-scroll-offset
  "clear-overlay resets *overlay-scroll-offset* to 0."
  (with-clean-overlay
    (show-overlay (format nil "a~%b"))
    (overlay-scroll 1)
    (clear-overlay)
    (is (= 0 *overlay-scroll-offset*)
        "clear-overlay must reset scroll offset to 0")))

(test overlay-lines-trailing-newline
  "A trailing newline yields a final empty line: overlay-lines collects the
   empty segment after the last newline."
  (with-clean-overlay
    (show-overlay (format nil "a~%"))
    (is (equal '("a" "") (overlay-lines))
        "text ending in newline produces a trailing empty line")
    (show-overlay (format nil "a~%b~%"))
    (is (equal '("a" "b" "") (overlay-lines)))))

(test overlay-scroll-zero-delta-is-noop
  "overlay-scroll with delta 0 leaves *overlay-scroll-offset* unchanged."
  (with-clean-overlay
    (show-overlay (format nil "line1~%line2~%line3"))
    (overlay-scroll 1)
    (is (= 1 *overlay-scroll-offset*))
    (overlay-scroll 0)
    (is (= 1 *overlay-scroll-offset*)
        "delta 0 must leave the offset unchanged")))

(test show-overlay-empty-string
  "show-overlay with an empty string activates overlay-active-p and overlay-lines returns a single empty line."
  (with-clean-overlay
    (let ((*overlay-scroll-offset* 5))
      (show-overlay "")
      (assert-overlay-active
       "overlay must be active after show-overlay with empty string")
      (is (= 0 *overlay-scroll-offset*)
          "show-overlay must reset scroll offset even for empty string")
      (is (equal '("") (overlay-lines))
          "empty string produces a list with one empty line"))))

(test clear-overlay-when-no-overlay-is-noop
  "clear-overlay when no overlay is active is a safe no-op."
  (with-clean-overlay
    (finishes (clear-overlay))
    (assert-overlay-inactive
     "overlay must remain inactive after clear-overlay on nil overlay")
    (is (= 0 *overlay-scroll-offset*)
        "scroll offset must remain 0 after clear-overlay on nil overlay")))
