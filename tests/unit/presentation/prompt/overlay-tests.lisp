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

(describe "overlay-suite"

  ;;; -- Dismissible overlay (list-keys help) ------------------------------------

  ;; With no overlay set, overlay-active-p is NIL and overlay-lines is empty.
  (it "overlay-inactive-by-default"
    (with-clean-overlay
      (assert-overlay-inactive)
      (expect (null (overlay-lines)))))

  ;; show-overlay activates a multi-line overlay; overlay-lines splits on newline;
  ;; clear-overlay dismisses it.
  (it "overlay-show-splits-lines-and-clears"
    (with-clean-overlay
      (show-overlay (format nil "line1~%line2~%line3"))
      (assert-overlay-active)
      (expect (equal '("line1" "line2" "line3") (overlay-lines)))
      (clear-overlay)
      (assert-overlay-inactive)
      (expect (null (overlay-lines)))))

  ;; A single-line overlay yields exactly one line (no trailing empty line).
  (it "overlay-single-line"
    (with-clean-overlay
      (show-overlay "solo")
      (expect (equal '("solo") (overlay-lines)))))

  ;;; -- Overlay scrolling -------------------------------------------------------
  ;;;
  ;;; overlay-scroll and *overlay-scroll-offset* are exported and used by
  ;;; events.lisp (j/k key bindings).  Clamping to [0, max-offset] is tested here.

  ;; overlay-scroll with a positive delta increases *overlay-scroll-offset*.
  (it "overlay-scroll-down-advances-offset"
    (with-clean-overlay
      (show-overlay (format nil "line1~%line2~%line3~%line4"))
      (overlay-scroll 1)
      (expect (= 1 *overlay-scroll-offset*))
      (overlay-scroll 1)
      (expect (= 2 *overlay-scroll-offset*))))

  ;; overlay-scroll with a negative delta decreases *overlay-scroll-offset*.
  (it "overlay-scroll-up-decreases-offset"
    (with-clean-overlay
      (show-overlay (format nil "line1~%line2~%line3"))
      (overlay-scroll 2)                         ; advance first
      (expect (= 2 *overlay-scroll-offset*))
      (overlay-scroll -1)
      (expect (= 1 *overlay-scroll-offset*))))

  ;; overlay-scroll never makes *overlay-scroll-offset* negative (clamps at 0).
  (it "overlay-scroll-clamps-at-zero"
    (with-clean-overlay
      (show-overlay (format nil "line1~%line2"))
      (overlay-scroll -5)
      (expect (= 0 *overlay-scroll-offset*))))

  ;; overlay-scroll never exceeds (1- line-count) (clamps at max-offset).
  (it "overlay-scroll-clamps-at-max"
    (with-clean-overlay
      ;; Three lines -> max-offset = 2.
      (show-overlay (format nil "line1~%line2~%line3"))
      (overlay-scroll 100)
      (expect (= 2 *overlay-scroll-offset*))))

  ;; overlay-scroll is a no-op when no overlay is active.
  (it "overlay-scroll-noop-when-no-overlay"
    (with-clean-overlay
      (overlay-scroll 5)
      (expect (= 0 *overlay-scroll-offset*))))

  ;; show-overlay resets *overlay-scroll-offset* to 0 regardless of prior value.
  (it "show-overlay-resets-scroll-offset"
    (with-clean-overlay
      (let ((*overlay-scroll-offset* 99))
        (show-overlay "fresh")
        (expect (= 0 *overlay-scroll-offset*)))))

  ;; clear-overlay resets *overlay-scroll-offset* to 0.
  (it "clear-overlay-resets-scroll-offset"
    (with-clean-overlay
      (show-overlay (format nil "a~%b"))
      (overlay-scroll 1)
      (clear-overlay)
      (expect (= 0 *overlay-scroll-offset*))))

  ;; A trailing newline yields a final empty line: overlay-lines collects the
  ;; empty segment after the last newline.
  (it "overlay-lines-trailing-newline"
    (with-clean-overlay
      (show-overlay (format nil "a~%"))
      (expect (equal '("a" "") (overlay-lines)))
      (show-overlay (format nil "a~%b~%"))
      (expect (equal '("a" "b" "") (overlay-lines)))))

  ;; overlay-scroll with delta 0 leaves *overlay-scroll-offset* unchanged.
  (it "overlay-scroll-zero-delta-is-noop"
    (with-clean-overlay
      (show-overlay (format nil "line1~%line2~%line3"))
      (overlay-scroll 1)
      (expect (= 1 *overlay-scroll-offset*))
      (overlay-scroll 0)
      (expect (= 1 *overlay-scroll-offset*))))

  ;; show-overlay with an empty string activates overlay-active-p and overlay-lines returns a single empty line.
  (it "show-overlay-empty-string"
    (with-clean-overlay
      (let ((*overlay-scroll-offset* 5))
        (show-overlay "")
        (assert-overlay-active
         "overlay must be active after show-overlay with empty string")
        (expect (= 0 *overlay-scroll-offset*))
        (expect (equal '("") (overlay-lines))))))

  ;; clear-overlay when no overlay is active is a safe no-op.
  (it "clear-overlay-when-no-overlay-is-noop"
    (with-clean-overlay
      (finishes (clear-overlay))
      (assert-overlay-inactive
       "overlay must remain inactive after clear-overlay on nil overlay")
      (expect (= 0 *overlay-scroll-offset*)))))
