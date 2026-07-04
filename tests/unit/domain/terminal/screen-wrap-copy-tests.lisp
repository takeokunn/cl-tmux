(in-package #:cl-tmux/test)

;;;; Screen wrap bookkeeping and copy-mode slot tests.
;;;;
;;;; Covers wrapped-row metadata, ANSI boolean mode slots generated through
;;;; define-boolean-slot-tests, and copy search/rectangular-selection state.

;;; ── SUITE: screen-wrapped-rows and %mark-line-wrapped / %line-wrapped-p ──────

(def-suite wrapped-rows-slot-suite
  :description "screen-wrapped-rows: NIL default, lazy allocation, mark/query primitives"
  :in terminal-suite)
(in-suite wrapped-rows-slot-suite)

(test screen-wrapped-rows-slot-defaults-nil
  "screen-wrapped-rows is NIL on a fresh screen."
  (with-screen (s 10 5)
    (is (null (cl-tmux/terminal/types:screen-wrapped-rows s))
        "wrapped-rows must be NIL initially")))

(test screen-wrapped-rows-lazily-allocated-on-first-mark
  "After %mark-line-wrapped, screen-wrapped-rows holds a hash-table."
  (with-screen (s 10 5)
    (cl-tmux/terminal/types:%mark-line-wrapped s 0)
    (let ((table (cl-tmux/terminal/types:screen-wrapped-rows s)))
      (is (hash-table-p table)
          "wrapped-rows must be a hash-table after first mark"))))

(def-suite mark-line-wrapped-suite
  :description "%mark-line-wrapped and %line-wrapped-p: set, query, absent"
  :in terminal-suite)
(in-suite mark-line-wrapped-suite)

(test mark-line-wrapped-marks-specified-row
  "%mark-line-wrapped sets the flag for the requested row."
  (with-screen (s 10 5)
    (cl-tmux/terminal/types:%mark-line-wrapped s 2)
    (is-true (cl-tmux/terminal/types:%line-wrapped-p s 2)
             "row 2 must be marked wrapped after %mark-line-wrapped")))

(test line-wrapped-p-returns-false-for-unmarked-row
  "%line-wrapped-p returns NIL for a row that was never marked."
  (with-screen (s 10 5)
    (is-false (cl-tmux/terminal/types:%line-wrapped-p s 0)
              "row 0 must not be wrapped on a fresh screen")))

(test mark-line-wrapped-only-marks-specified-row
  "%mark-line-wrapped does not affect adjacent rows."
  (with-screen (s 10 5)
    (cl-tmux/terminal/types:%mark-line-wrapped s 1)
    (is-false (cl-tmux/terminal/types:%line-wrapped-p s 0) "row 0 must remain unmarked")
    (is-true  (cl-tmux/terminal/types:%line-wrapped-p s 1) "row 1 must be marked")
    (is-false (cl-tmux/terminal/types:%line-wrapped-p s 2) "row 2 must remain unmarked")))

(test mark-line-wrapped-multiple-rows
  "%mark-line-wrapped can mark multiple distinct rows independently."
  (with-screen (s 10 5)
    (cl-tmux/terminal/types:%mark-line-wrapped s 0)
    (cl-tmux/terminal/types:%mark-line-wrapped s 3)
    (is-true  (cl-tmux/terminal/types:%line-wrapped-p s 0) "row 0 must be marked")
    (is-false (cl-tmux/terminal/types:%line-wrapped-p s 1) "row 1 must be unmarked")
    (is-true  (cl-tmux/terminal/types:%line-wrapped-p s 3) "row 3 must be marked")))

;;; ── SUITE: %clear-all-line-wrapped ──────────────────────────────────────────

(def-suite clear-all-line-wrapped-suite
  :description "%clear-all-line-wrapped: clears all marks atomically"
  :in terminal-suite)
(in-suite clear-all-line-wrapped-suite)

(test clear-all-line-wrapped-removes-all-flags
  "%clear-all-line-wrapped makes every row report unwrapped."
  (with-screen (s 10 5)
    (cl-tmux/terminal/types:%mark-line-wrapped s 0)
    (cl-tmux/terminal/types:%mark-line-wrapped s 1)
    (cl-tmux/terminal/types:%mark-line-wrapped s 4)
    (cl-tmux/terminal/types:%clear-all-line-wrapped s)
    (dotimes (y 5)
      (is-false (cl-tmux/terminal/types:%line-wrapped-p s y)
                "row ~D must be unwrapped after %clear-all-line-wrapped" y))))

(test clear-all-line-wrapped-on-fresh-screen-is-noop
  "%clear-all-line-wrapped on a screen with no wrap table is a no-op."
  (with-screen (s 10 5)
    (is (null (cl-tmux/terminal/types:screen-wrapped-rows s))
        "pre-condition: no wrap table")
    (finishes (cl-tmux/terminal/types:%clear-all-line-wrapped s))
    (is (null (cl-tmux/terminal/types:screen-wrapped-rows s))
        "wrapped-rows must still be NIL after clear-all on fresh screen")))

;;; ── SUITE: %shift-line-wrapped-up ────────────────────────────────────────────

(def-suite shift-line-wrapped-up-suite
  :description "%shift-line-wrapped-up: region shift preserves outside-region flags"
  :in terminal-suite)
(in-suite shift-line-wrapped-up-suite)

(test shift-line-wrapped-up-moves-flags-in-region
  "%shift-line-wrapped-up: a flag at Y in (top,bottom] moves to Y-1."
  (with-screen (s 10 6)
    (cl-tmux/terminal/types:%mark-line-wrapped s 2)
    (cl-tmux/terminal/types:%mark-line-wrapped s 3)
    (cl-tmux/terminal/types:%mark-line-wrapped s 4)
    (cl-tmux/terminal/types:%shift-line-wrapped-up s 1 5)
    (is-false (cl-tmux/terminal/types:%line-wrapped-p s 0) "row 0 (above region) untouched")
    (is-true  (cl-tmux/terminal/types:%line-wrapped-p s 1) "row 1 gets flag from row 2")
    (is-true  (cl-tmux/terminal/types:%line-wrapped-p s 2) "row 2 gets flag from row 3")
    (is-true  (cl-tmux/terminal/types:%line-wrapped-p s 3) "row 3 gets flag from row 4")
    (is-false (cl-tmux/terminal/types:%line-wrapped-p s 4) "row 4: no source (row 5 unmarked)")
    (is-false (cl-tmux/terminal/types:%line-wrapped-p s 5) "row 5: bottom cleared")))

(test shift-line-wrapped-up-preserves-outside-region
  "%shift-line-wrapped-up does not disturb rows outside [top, bottom]."
  (with-screen (s 10 8)
    (cl-tmux/terminal/types:%mark-line-wrapped s 0)
    (cl-tmux/terminal/types:%mark-line-wrapped s 6)
    (cl-tmux/terminal/types:%shift-line-wrapped-up s 2 5)
    (is-true (cl-tmux/terminal/types:%line-wrapped-p s 0)
             "row 0 (above region) must remain marked")
    (is-true (cl-tmux/terminal/types:%line-wrapped-p s 6)
             "row 6 (below region) must remain marked")))

(test shift-line-wrapped-up-noop-when-no-table
  "%shift-line-wrapped-up on a fresh screen (no hash-table) is a no-op."
  (with-screen (s 10 5)
    (finishes (cl-tmux/terminal/types:%shift-line-wrapped-up s 0 4))
    (is-false (cl-tmux/terminal/types:%line-wrapped-p s 0)
              "all rows must still be unwrapped after shift on empty screen")))

;;; ── SUITE: ANSI mode boolean slots (via define-boolean-slot-tests) ───────────
;;;
;;; screen-insert-mode (IRM), screen-newline-mode (LNM), and screen-reverse-screen
;;; (DECSCNM) all follow the identical defaults-NIL / enable / disable triple.

(define-boolean-slot-tests
  cl-tmux/terminal/types:screen-insert-mode
  screen-insert-mode-suite
  (feed s (esc "[4h"))   ; CSI 4 h — IRM set (insert mode on)
  (feed s (esc "[4l"))   ; CSI 4 l — IRM reset (replace mode)
  :suite-description "screen-insert-mode: defaults NIL, CSI 4h enables, CSI 4l disables")

(define-boolean-slot-tests
  cl-tmux/terminal/types:screen-newline-mode
  screen-newline-mode-suite
  (feed s (esc "[20h"))  ; CSI 20 h — LNM set
  (feed s (esc "[20l"))  ; CSI 20 l — LNM reset
  :suite-description "screen-newline-mode: defaults NIL, CSI 20h enables, CSI 20l disables")

(define-boolean-slot-tests
  cl-tmux/terminal/types:screen-reverse-screen
  screen-reverse-screen-suite
  (feed s (esc "[?5h"))  ; ESC[?5h — DECSCNM set (reverse video on)
  (feed s (esc "[?5l"))  ; ESC[?5l — DECSCNM reset
  :suite-description "screen-reverse-screen: defaults NIL, ESC[?5h enables, ESC[?5l disables")

;;; ── SUITE: screen-copy-search-direction ──────────────────────────────────────

(def-suite copy-search-direction-suite
  :description "screen-copy-search-direction slot: default NIL, forward and backward"
  :in terminal-suite)
(in-suite copy-search-direction-suite)

(test screen-copy-search-direction-defaults-nil
  "screen-copy-search-direction is NIL on a fresh screen."
  (with-screen (s 10 5)
    (is (null (cl-tmux/terminal/types:screen-copy-search-direction s))
        "copy-search-direction must be NIL initially")))

(test screen-copy-search-direction-can-be-set-forward
  "screen-copy-search-direction can be set to :forward."
  (with-screen (s 10 5)
    (setf (cl-tmux/terminal/types:screen-copy-search-direction s) :forward)
    (is (eq :forward (cl-tmux/terminal/types:screen-copy-search-direction s))
        "copy-search-direction must be :forward after setf")))

(test screen-copy-search-direction-can-be-set-backward
  "screen-copy-search-direction can be set to :backward."
  (with-screen (s 10 5)
    (setf (cl-tmux/terminal/types:screen-copy-search-direction s) :backward)
    (is (eq :backward (cl-tmux/terminal/types:screen-copy-search-direction s))
        "copy-search-direction must be :backward after setf")))

(test screen-copy-search-direction-can-be-cleared
  "screen-copy-search-direction can be reset to NIL."
  (with-screen (s 10 5)
    (setf (cl-tmux/terminal/types:screen-copy-search-direction s) :forward)
    (setf (cl-tmux/terminal/types:screen-copy-search-direction s) nil)
    (is (null (cl-tmux/terminal/types:screen-copy-search-direction s))
        "copy-search-direction must be NIL after clearing")))

;;; ── SUITE: screen-copy-rect-select-p ────────────────────────────────────────

(def-suite copy-rect-select-suite
  :description "screen-copy-rect-select-p slot: default NIL and toggle"
  :in terminal-suite)
(in-suite copy-rect-select-suite)

(test screen-copy-rect-select-p-defaults-nil
  "screen-copy-rect-select-p is NIL on a fresh screen."
  (with-screen (s 10 5)
    (is-false (cl-tmux/terminal/types:screen-copy-rect-select-p s)
              "copy-rect-select-p must be NIL initially")))

(test screen-copy-rect-select-p-can-be-set-and-cleared
  "screen-copy-rect-select-p can be toggled via setf."
  (with-screen (s 10 5)
    (setf (cl-tmux/terminal/types:screen-copy-rect-select-p s) t)
    (is-true (cl-tmux/terminal/types:screen-copy-rect-select-p s)
             "copy-rect-select-p must be T after setf T")
    (setf (cl-tmux/terminal/types:screen-copy-rect-select-p s) nil)
    (is-false (cl-tmux/terminal/types:screen-copy-rect-select-p s)
              "copy-rect-select-p must be NIL after setf NIL")))
