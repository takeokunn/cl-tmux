(in-package #:cl-tmux/test)

;;;; Screen queue and palette tests.
;;;;
;;;; Covers passthrough/clipboard queue slots, atomic queue draining, and
;;;; palette override storage/get/set/clear behavior.

;;; ── SUITE: screen-passthrough-queue and screen-clipboard-queue ───────────────

(describe "terminal-suite/queue-slots-suite"

  ;; screen-passthrough-queue is NIL on a fresh screen.
  (it "screen-passthrough-queue-defaults-nil"
    (with-screen (s 10 5)
      (expect (null (cl-tmux/terminal/types:screen-passthrough-queue s)))))

  ;; Items pushed onto passthrough-queue can be nreversed to drain in FIFO order.
  (it "screen-passthrough-queue-can-be-pushed-and-drained"
    (with-screen (s 10 5)
      (push "pt-a" (cl-tmux/terminal/types:screen-passthrough-queue s))
      (push "pt-b" (cl-tmux/terminal/types:screen-passthrough-queue s))
      (let ((items (nreverse (cl-tmux/terminal/types:screen-passthrough-queue s))))
        (setf (cl-tmux/terminal/types:screen-passthrough-queue s) nil)
        (expect (equal '("pt-a" "pt-b") items)))))

  ;; screen-clipboard-queue is NIL on a fresh screen.
  (it "screen-clipboard-queue-defaults-nil"
    (with-screen (s 10 5)
      (expect (null (cl-tmux/terminal/types:screen-clipboard-queue s)))))

  ;; Items pushed onto clipboard-queue can be nreversed to drain in FIFO order.
  (it "screen-clipboard-queue-can-be-pushed-and-drained"
    (with-screen (s 10 5)
      (push "clip-a" (cl-tmux/terminal/types:screen-clipboard-queue s))
      (push "clip-b" (cl-tmux/terminal/types:screen-clipboard-queue s))
      (let ((items (nreverse (cl-tmux/terminal/types:screen-clipboard-queue s))))
        (setf (cl-tmux/terminal/types:screen-clipboard-queue s) nil)
        (expect (equal '("clip-a" "clip-b") items)))))

  ;; screen-drain-queue returns queued items in push order and clears the slot,
  ;; without the caller ever calling SETF on the queue slot directly.
  (it "screen-drain-queue-reads-and-clears-atomically"
    (with-screen (s 10 5)
      (push "pt-a" (cl-tmux/terminal/types:screen-passthrough-queue s))
      (push "pt-b" (cl-tmux/terminal/types:screen-passthrough-queue s))
      (let ((items (cl-tmux/terminal/types:screen-drain-queue
                    s
                    #'cl-tmux/terminal/types:screen-passthrough-queue
                    (lambda (screen value)
                      (setf (cl-tmux/terminal/types:screen-passthrough-queue screen) value)))))
        (expect (equal '("pt-a" "pt-b") items))
        (expect (null (cl-tmux/terminal/types:screen-passthrough-queue s))))))

  ;; screen-drain-queue on an empty queue returns NIL without error.
  (it "screen-drain-queue-empty-queue-returns-nil"
    (with-screen (s 10 5)
      (expect (null (cl-tmux/terminal/types:screen-drain-queue
                     s
                     #'cl-tmux/terminal/types:screen-clipboard-queue
                     (lambda (screen value)
                       (setf (cl-tmux/terminal/types:screen-clipboard-queue screen) value))))))))

;;; ── SUITE: screen-palette-overrides direct slot ──────────────────────────────

(describe "terminal-suite/palette-overrides-slot-suite"

  ;; screen-palette-overrides is NIL on a fresh screen.
  (it "screen-palette-overrides-slot-defaults-nil"
    (with-screen (s 10 5)
      (expect (null (cl-tmux/terminal/types:screen-palette-overrides s)))))

  ;; After %palette-override-set the slot holds a 256-element simple-vector.
  (it "screen-palette-overrides-lazily-allocated-on-first-set"
    (with-screen (s 10 5)
      (cl-tmux/terminal/types:%palette-override-set s 42 #xABCDEF)
      (let ((overrides (cl-tmux/terminal/types:screen-palette-overrides s)))
        (expect (simple-vector-p overrides))
        (expect (= 256 (length overrides)))))))

;;; ── SUITE: %palette-override-get / %palette-override-set / %palette-override-clear ──
;;;
;;; Single-index round-trip and boundary behaviour, distinct from the
;;; %palette-override-clear-all bulk-reset suite below.

(describe "terminal-suite/palette-override-get-set-clear-suite"

  ;; %palette-override-get returns NIL for any index on a fresh screen (no vector allocated).
  (it "palette-override-get-returns-nil-before-any-set"
    (with-screen (s 10 5)
      (expect (null (cl-tmux/terminal/types:%palette-override-get s 0)))
      (expect (null (cl-tmux/terminal/types:%palette-override-get s 255)))))

  ;; %palette-override-get returns the exact RGB value passed to %palette-override-set.
  (it "palette-override-set-then-get-round-trips"
    (with-screen (s 10 5)
      (cl-tmux/terminal/types:%palette-override-set s 42 #xABCDEF)
      (expect (= #xABCDEF (cl-tmux/terminal/types:%palette-override-get s 42)))))

  ;; %palette-override-set at one index leaves other indices NIL.
  (it "palette-override-set-does-not-disturb-other-indices"
    (with-screen (s 10 5)
      (cl-tmux/terminal/types:%palette-override-set s 5 #x123456)
      (expect (null (cl-tmux/terminal/types:%palette-override-get s 4)))
      (expect (null (cl-tmux/terminal/types:%palette-override-get s 6)))))

  ;; %palette-override-get returns NIL for indices outside 0..255, even after other sets.
  (it "palette-override-get-out-of-range-index-returns-nil"
    (with-screen (s 10 5)
      (cl-tmux/terminal/types:%palette-override-set s 0 #xFFFFFF)
      (expect (null (cl-tmux/terminal/types:%palette-override-get s -1)))
      (expect (null (cl-tmux/terminal/types:%palette-override-get s 256)))))

  ;; %palette-override-set silently ignores an out-of-range index (no error, no allocation forced).
  (it "palette-override-set-out-of-range-index-is-ignored"
    (with-screen (s 10 5)
      (finishes (cl-tmux/terminal/types:%palette-override-set s 256 #xFFFFFF))
      (finishes (cl-tmux/terminal/types:%palette-override-set s -1 #xFFFFFF))))

  ;; %palette-override-clear reverts one index to NIL, leaving other indices intact.
  (it "palette-override-clear-resets-single-index-to-nil"
    (with-screen (s 10 5)
      (cl-tmux/terminal/types:%palette-override-set s 10 #x111111)
      (cl-tmux/terminal/types:%palette-override-set s 20 #x222222)
      (cl-tmux/terminal/types:%palette-override-clear s 10)
      (expect (null (cl-tmux/terminal/types:%palette-override-get s 10)))
      (expect (= #x222222 (cl-tmux/terminal/types:%palette-override-get s 20)))))

  ;; %palette-override-clear on an index that was never set does not signal.
  (it "palette-override-clear-on-unset-index-is-noop"
    (with-screen (s 10 5)
      (cl-tmux/terminal/types:%palette-override-set s 0 #xABCDEF)
      (finishes (cl-tmux/terminal/types:%palette-override-clear s 100))
      (expect (null (cl-tmux/terminal/types:%palette-override-get s 100)))))

  ;; %palette-override-clear on a fresh screen (no overrides vector yet) does not signal.
  (it "palette-override-clear-with-no-overrides-allocated-is-noop"
    (with-screen (s 10 5)
      (expect (null (cl-tmux/terminal/types:screen-palette-overrides s)))
      (finishes (cl-tmux/terminal/types:%palette-override-clear s 0))
      (expect (null (cl-tmux/terminal/types:screen-palette-overrides s)))))

  ;; %palette-override-clear silently ignores an out-of-range index.
  (it "palette-override-clear-out-of-range-index-is-ignored"
    (with-screen (s 10 5)
      (cl-tmux/terminal/types:%palette-override-set s 0 #xFFFFFF)
      (finishes (cl-tmux/terminal/types:%palette-override-clear s 256))
      (finishes (cl-tmux/terminal/types:%palette-override-clear s -1))
      (expect (= #xFFFFFF (cl-tmux/terminal/types:%palette-override-get s 0))))))

;;; ── SUITE: %palette-override-clear-all ──────────────────────────────────────

(describe "terminal-suite/palette-clear-all-suite"

  ;; %palette-override-clear-all sets palette-overrides back to NIL.
  (it "palette-override-clear-all-drops-vector"
    (with-screen (s 10 5)
      (cl-tmux/terminal/types:%palette-override-set s 0 #xFF0000)
      (cl-tmux/terminal/types:%palette-override-set s 255 #x00FF00)
      (expect (cl-tmux/terminal/types:screen-palette-overrides s) :to-be-truthy)
      (cl-tmux/terminal/types:%palette-override-clear-all s)
      (expect (null (cl-tmux/terminal/types:screen-palette-overrides s)))))

  ;; %palette-override-clear-all on a fresh screen (no vector) is a no-op.
  (it "palette-override-clear-all-on-empty-screen-is-noop"
    (with-screen (s 10 5)
      (finishes (cl-tmux/terminal/types:%palette-override-clear-all s))
      (expect (null (cl-tmux/terminal/types:screen-palette-overrides s)))))

  ;; After %palette-override-clear-all, %palette-override-get returns NIL for every index.
  (it "palette-override-clear-all-makes-all-indices-return-nil"
    (with-screen (s 10 5)
      (cl-tmux/terminal/types:%palette-override-set s 0   #x111111)
      (cl-tmux/terminal/types:%palette-override-set s 128 #x888888)
      (cl-tmux/terminal/types:%palette-override-set s 255 #xFFFFFF)
      (cl-tmux/terminal/types:%palette-override-clear-all s)
      (expect (null (cl-tmux/terminal/types:%palette-override-get s 0)))
      (expect (null (cl-tmux/terminal/types:%palette-override-get s 128)))
      (expect (null (cl-tmux/terminal/types:%palette-override-get s 255))))))
