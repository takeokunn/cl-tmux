(in-package #:cl-tmux/test)

;;;; Screen wrap bookkeeping and copy-mode slot tests.
;;;;
;;;; Covers wrapped-row metadata, ANSI boolean mode slots generated through
;;;; define-boolean-slot-tests, and copy search/rectangular-selection state.

;;; ── SUITE: screen-wrapped-rows and %mark-line-wrapped / %line-wrapped-p ──────

(describe "terminal-suite/wrapped-rows-slot-suite"

  ;; screen-wrapped-rows is NIL on a fresh screen.
  (it "screen-wrapped-rows-slot-defaults-nil"
    (with-screen (s 10 5)
      (expect (null (cl-tmux/terminal/types:screen-wrapped-rows s)))))

  ;; After %mark-line-wrapped, screen-wrapped-rows holds a hash-table.
  (it "screen-wrapped-rows-lazily-allocated-on-first-mark"
    (with-screen (s 10 5)
      (cl-tmux/terminal/types:%mark-line-wrapped s 0)
      (let ((table (cl-tmux/terminal/types:screen-wrapped-rows s)))
        (expect (hash-table-p table))))))

(describe "terminal-suite/mark-line-wrapped-suite"

  ;; %mark-line-wrapped sets the flag for the requested row.
  (it "mark-line-wrapped-marks-specified-row"
    (with-screen (s 10 5)
      (cl-tmux/terminal/types:%mark-line-wrapped s 2)
      (expect (cl-tmux/terminal/types:%line-wrapped-p s 2) :to-be-truthy)))

  ;; %line-wrapped-p returns NIL for a row that was never marked.
  (it "line-wrapped-p-returns-false-for-unmarked-row"
    (with-screen (s 10 5)
      (expect (cl-tmux/terminal/types:%line-wrapped-p s 0) :to-be-falsy)))

  ;; %mark-line-wrapped does not affect adjacent rows.
  (it "mark-line-wrapped-only-marks-specified-row"
    (with-screen (s 10 5)
      (cl-tmux/terminal/types:%mark-line-wrapped s 1)
      (expect (cl-tmux/terminal/types:%line-wrapped-p s 0) :to-be-falsy)
      (expect (cl-tmux/terminal/types:%line-wrapped-p s 1) :to-be-truthy)
      (expect (cl-tmux/terminal/types:%line-wrapped-p s 2) :to-be-falsy)))

  ;; %mark-line-wrapped can mark multiple distinct rows independently.
  (it "mark-line-wrapped-multiple-rows"
    (with-screen (s 10 5)
      (cl-tmux/terminal/types:%mark-line-wrapped s 0)
      (cl-tmux/terminal/types:%mark-line-wrapped s 3)
      (expect (cl-tmux/terminal/types:%line-wrapped-p s 0) :to-be-truthy)
      (expect (cl-tmux/terminal/types:%line-wrapped-p s 1) :to-be-falsy)
      (expect (cl-tmux/terminal/types:%line-wrapped-p s 3) :to-be-truthy))))

;;; ── SUITE: %clear-all-line-wrapped ──────────────────────────────────────────

(describe "terminal-suite/clear-all-line-wrapped-suite"

  ;; %clear-all-line-wrapped makes every row report unwrapped.
  (it "clear-all-line-wrapped-removes-all-flags"
    (with-screen (s 10 5)
      (cl-tmux/terminal/types:%mark-line-wrapped s 0)
      (cl-tmux/terminal/types:%mark-line-wrapped s 1)
      (cl-tmux/terminal/types:%mark-line-wrapped s 4)
      (cl-tmux/terminal/types:%clear-all-line-wrapped s)
      (dotimes (y 5)
        (expect (cl-tmux/terminal/types:%line-wrapped-p s y) :to-be-falsy))))

  ;; %clear-all-line-wrapped on a screen with no wrap table is a no-op.
  (it "clear-all-line-wrapped-on-fresh-screen-is-noop"
    (with-screen (s 10 5)
      (expect (null (cl-tmux/terminal/types:screen-wrapped-rows s)))
      (finishes (cl-tmux/terminal/types:%clear-all-line-wrapped s))
      (expect (null (cl-tmux/terminal/types:screen-wrapped-rows s))))))

;;; ── SUITE: %shift-line-wrapped-up ────────────────────────────────────────────

(describe "terminal-suite/shift-line-wrapped-up-suite"

  ;; %shift-line-wrapped-up: a flag at Y in (top,bottom] moves to Y-1.
  (it "shift-line-wrapped-up-moves-flags-in-region"
    (with-screen (s 10 6)
      (cl-tmux/terminal/types:%mark-line-wrapped s 2)
      (cl-tmux/terminal/types:%mark-line-wrapped s 3)
      (cl-tmux/terminal/types:%mark-line-wrapped s 4)
      (cl-tmux/terminal/types:%shift-line-wrapped-up s 1 5)
      (expect (cl-tmux/terminal/types:%line-wrapped-p s 0) :to-be-falsy)
      (expect (cl-tmux/terminal/types:%line-wrapped-p s 1) :to-be-truthy)
      (expect (cl-tmux/terminal/types:%line-wrapped-p s 2) :to-be-truthy)
      (expect (cl-tmux/terminal/types:%line-wrapped-p s 3) :to-be-truthy)
      (expect (cl-tmux/terminal/types:%line-wrapped-p s 4) :to-be-falsy)
      (expect (cl-tmux/terminal/types:%line-wrapped-p s 5) :to-be-falsy)))

  ;; %shift-line-wrapped-up does not disturb rows outside [top, bottom].
  (it "shift-line-wrapped-up-preserves-outside-region"
    (with-screen (s 10 8)
      (cl-tmux/terminal/types:%mark-line-wrapped s 0)
      (cl-tmux/terminal/types:%mark-line-wrapped s 6)
      (cl-tmux/terminal/types:%shift-line-wrapped-up s 2 5)
      (expect (cl-tmux/terminal/types:%line-wrapped-p s 0) :to-be-truthy)
      (expect (cl-tmux/terminal/types:%line-wrapped-p s 6) :to-be-truthy)))

  ;; %shift-line-wrapped-up on a fresh screen (no hash-table) is a no-op.
  (it "shift-line-wrapped-up-noop-when-no-table"
    (with-screen (s 10 5)
      (finishes (cl-tmux/terminal/types:%shift-line-wrapped-up s 0 4))
      (expect (cl-tmux/terminal/types:%line-wrapped-p s 0) :to-be-falsy))))

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

(describe "terminal-suite/copy-search-direction-suite"

  ;; screen-copy-search-direction is NIL on a fresh screen.
  (it "screen-copy-search-direction-defaults-nil"
    (with-screen (s 10 5)
      (expect (null (cl-tmux/terminal/types:screen-copy-search-direction s)))))

  ;; screen-copy-search-direction can be set to :forward.
  (it "screen-copy-search-direction-can-be-set-forward"
    (with-screen (s 10 5)
      (setf (cl-tmux/terminal/types:screen-copy-search-direction s) :forward)
      (expect (eq :forward (cl-tmux/terminal/types:screen-copy-search-direction s)))))

  ;; screen-copy-search-direction can be set to :backward.
  (it "screen-copy-search-direction-can-be-set-backward"
    (with-screen (s 10 5)
      (setf (cl-tmux/terminal/types:screen-copy-search-direction s) :backward)
      (expect (eq :backward (cl-tmux/terminal/types:screen-copy-search-direction s)))))

  ;; screen-copy-search-direction can be reset to NIL.
  (it "screen-copy-search-direction-can-be-cleared"
    (with-screen (s 10 5)
      (setf (cl-tmux/terminal/types:screen-copy-search-direction s) :forward)
      (setf (cl-tmux/terminal/types:screen-copy-search-direction s) nil)
      (expect (null (cl-tmux/terminal/types:screen-copy-search-direction s))))))

;;; ── SUITE: screen-copy-rect-select-p ────────────────────────────────────────

(describe "terminal-suite/copy-rect-select-suite"

  ;; screen-copy-rect-select-p is NIL on a fresh screen.
  (it "screen-copy-rect-select-p-defaults-nil"
    (with-screen (s 10 5)
      (expect (cl-tmux/terminal/types:screen-copy-rect-select-p s) :to-be-falsy)))

  ;; screen-copy-rect-select-p can be toggled via setf.
  (it "screen-copy-rect-select-p-can-be-set-and-cleared"
    (with-screen (s 10 5)
      (setf (cl-tmux/terminal/types:screen-copy-rect-select-p s) t)
      (expect (cl-tmux/terminal/types:screen-copy-rect-select-p s) :to-be-truthy)
      (setf (cl-tmux/terminal/types:screen-copy-rect-select-p s) nil)
      (expect (cl-tmux/terminal/types:screen-copy-rect-select-p s) :to-be-falsy))))
