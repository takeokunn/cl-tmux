(in-package #:cl-tmux/test)

;;;; Screen queue and palette tests.
;;;;
;;;; Covers passthrough/clipboard queue slots, atomic queue draining, and
;;;; palette override storage/get/set/clear behavior.

;;; ── SUITE: screen-passthrough-queue and screen-clipboard-queue ───────────────

(def-suite queue-slots-suite
  :description "screen-passthrough-queue and screen-clipboard-queue: default and FIFO drain"
  :in terminal-suite)
(in-suite queue-slots-suite)

(test screen-passthrough-queue-defaults-nil
  "screen-passthrough-queue is NIL on a fresh screen."
  (with-screen (s 10 5)
    (is (null (cl-tmux/terminal/types:screen-passthrough-queue s))
        "passthrough-queue must be NIL initially")))

(test screen-passthrough-queue-can-be-pushed-and-drained
  "Items pushed onto passthrough-queue can be nreversed to drain in FIFO order."
  (with-screen (s 10 5)
    (push "pt-a" (cl-tmux/terminal/types:screen-passthrough-queue s))
    (push "pt-b" (cl-tmux/terminal/types:screen-passthrough-queue s))
    (let ((items (nreverse (cl-tmux/terminal/types:screen-passthrough-queue s))))
      (setf (cl-tmux/terminal/types:screen-passthrough-queue s) nil)
      (is (equal '("pt-a" "pt-b") items)
          "passthrough-queue must drain in push order"))))

(test screen-clipboard-queue-defaults-nil
  "screen-clipboard-queue is NIL on a fresh screen."
  (with-screen (s 10 5)
    (is (null (cl-tmux/terminal/types:screen-clipboard-queue s))
        "clipboard-queue must be NIL initially")))

(test screen-clipboard-queue-can-be-pushed-and-drained
  "Items pushed onto clipboard-queue can be nreversed to drain in FIFO order."
  (with-screen (s 10 5)
    (push "clip-a" (cl-tmux/terminal/types:screen-clipboard-queue s))
    (push "clip-b" (cl-tmux/terminal/types:screen-clipboard-queue s))
    (let ((items (nreverse (cl-tmux/terminal/types:screen-clipboard-queue s))))
      (setf (cl-tmux/terminal/types:screen-clipboard-queue s) nil)
      (is (equal '("clip-a" "clip-b") items)
          "clipboard-queue must drain in push order"))))

(test screen-drain-queue-reads-and-clears-atomically
  "screen-drain-queue returns queued items in push order and clears the slot,
   without the caller ever calling SETF on the queue slot directly."
  (with-screen (s 10 5)
    (push "pt-a" (cl-tmux/terminal/types:screen-passthrough-queue s))
    (push "pt-b" (cl-tmux/terminal/types:screen-passthrough-queue s))
    (let ((items (cl-tmux/terminal/types:screen-drain-queue
                  s
                  #'cl-tmux/terminal/types:screen-passthrough-queue
                  (lambda (screen value)
                    (setf (cl-tmux/terminal/types:screen-passthrough-queue screen) value)))))
      (is (equal '("pt-a" "pt-b") items)
          "screen-drain-queue must return items in push order")
      (is (null (cl-tmux/terminal/types:screen-passthrough-queue s))
          "screen-drain-queue must clear the queue slot as a side-effect"))))

(test screen-drain-queue-empty-queue-returns-nil
  "screen-drain-queue on an empty queue returns NIL without error."
  (with-screen (s 10 5)
    (is (null (cl-tmux/terminal/types:screen-drain-queue
               s
               #'cl-tmux/terminal/types:screen-clipboard-queue
               (lambda (screen value)
                 (setf (cl-tmux/terminal/types:screen-clipboard-queue screen) value))))
        "draining an empty queue must return NIL")))

;;; ── SUITE: screen-palette-overrides direct slot ──────────────────────────────

(def-suite palette-overrides-slot-suite
  :description "screen-palette-overrides direct slot: NIL default and lazy allocation"
  :in terminal-suite)
(in-suite palette-overrides-slot-suite)

(test screen-palette-overrides-slot-defaults-nil
  "screen-palette-overrides is NIL on a fresh screen."
  (with-screen (s 10 5)
    (is (null (cl-tmux/terminal/types:screen-palette-overrides s))
        "palette-overrides must be NIL initially")))

(test screen-palette-overrides-lazily-allocated-on-first-set
  "After %palette-override-set the slot holds a 256-element simple-vector."
  (with-screen (s 10 5)
    (cl-tmux/terminal/types:%palette-override-set s 42 #xABCDEF)
    (let ((overrides (cl-tmux/terminal/types:screen-palette-overrides s)))
      (is (simple-vector-p overrides)
          "palette-overrides must be a simple-vector after first set")
      (is (= 256 (length overrides))
          "palette-overrides vector must have 256 entries"))))

;;; ── SUITE: %palette-override-get / %palette-override-set / %palette-override-clear ──
;;;
;;; Single-index round-trip and boundary behaviour, distinct from the
;;; %palette-override-clear-all bulk-reset suite below.

(def-suite palette-override-get-set-clear-suite
  :description "%palette-override-get/set/clear: single-index round-trip and out-of-range handling"
  :in terminal-suite)
(in-suite palette-override-get-set-clear-suite)

(test palette-override-get-returns-nil-before-any-set
  "%palette-override-get returns NIL for any index on a fresh screen (no vector allocated)."
  (with-screen (s 10 5)
    (is (null (cl-tmux/terminal/types:%palette-override-get s 0))
        "index 0 must be NIL before any set")
    (is (null (cl-tmux/terminal/types:%palette-override-get s 255))
        "index 255 must be NIL before any set")))

(test palette-override-set-then-get-round-trips
  "%palette-override-get returns the exact RGB value passed to %palette-override-set."
  (with-screen (s 10 5)
    (cl-tmux/terminal/types:%palette-override-set s 42 #xABCDEF)
    (is (= #xABCDEF (cl-tmux/terminal/types:%palette-override-get s 42))
        "index 42 must round-trip the set value")))

(test palette-override-set-does-not-disturb-other-indices
  "%palette-override-set at one index leaves other indices NIL."
  (with-screen (s 10 5)
    (cl-tmux/terminal/types:%palette-override-set s 5 #x123456)
    (is (null (cl-tmux/terminal/types:%palette-override-get s 4))
        "index 4 must remain NIL")
    (is (null (cl-tmux/terminal/types:%palette-override-get s 6))
        "index 6 must remain NIL")))

(test palette-override-get-out-of-range-index-returns-nil
  "%palette-override-get returns NIL for indices outside 0..255, even after other sets."
  (with-screen (s 10 5)
    (cl-tmux/terminal/types:%palette-override-set s 0 #xFFFFFF)
    (is (null (cl-tmux/terminal/types:%palette-override-get s -1))
        "negative index must return NIL")
    (is (null (cl-tmux/terminal/types:%palette-override-get s 256))
        "index 256 (above range) must return NIL")))

(test palette-override-set-out-of-range-index-is-ignored
  "%palette-override-set silently ignores an out-of-range index (no error, no allocation forced)."
  (with-screen (s 10 5)
    (finishes (cl-tmux/terminal/types:%palette-override-set s 256 #xFFFFFF))
    (finishes (cl-tmux/terminal/types:%palette-override-set s -1 #xFFFFFF))))

(test palette-override-clear-resets-single-index-to-nil
  "%palette-override-clear reverts one index to NIL, leaving other indices intact."
  (with-screen (s 10 5)
    (cl-tmux/terminal/types:%palette-override-set s 10 #x111111)
    (cl-tmux/terminal/types:%palette-override-set s 20 #x222222)
    (cl-tmux/terminal/types:%palette-override-clear s 10)
    (is (null (cl-tmux/terminal/types:%palette-override-get s 10))
        "index 10 must be NIL after %palette-override-clear")
    (is (= #x222222 (cl-tmux/terminal/types:%palette-override-get s 20))
        "index 20 must be unaffected by clearing index 10")))

(test palette-override-clear-on-unset-index-is-noop
  "%palette-override-clear on an index that was never set does not signal."
  (with-screen (s 10 5)
    (cl-tmux/terminal/types:%palette-override-set s 0 #xABCDEF)
    (finishes (cl-tmux/terminal/types:%palette-override-clear s 100))
    (is (null (cl-tmux/terminal/types:%palette-override-get s 100))
        "unset index 100 must remain NIL")))

(test palette-override-clear-with-no-overrides-allocated-is-noop
  "%palette-override-clear on a fresh screen (no overrides vector yet) does not signal."
  (with-screen (s 10 5)
    (is (null (cl-tmux/terminal/types:screen-palette-overrides s))
        "pre-condition: no overrides vector allocated")
    (finishes (cl-tmux/terminal/types:%palette-override-clear s 0))
    (is (null (cl-tmux/terminal/types:screen-palette-overrides s))
        "overrides vector must remain NIL (clear must not force allocation)")))

(test palette-override-clear-out-of-range-index-is-ignored
  "%palette-override-clear silently ignores an out-of-range index."
  (with-screen (s 10 5)
    (cl-tmux/terminal/types:%palette-override-set s 0 #xFFFFFF)
    (finishes (cl-tmux/terminal/types:%palette-override-clear s 256))
    (finishes (cl-tmux/terminal/types:%palette-override-clear s -1))
    (is (= #xFFFFFF (cl-tmux/terminal/types:%palette-override-get s 0))
        "index 0 must be untouched by out-of-range clears")))

;;; ── SUITE: %palette-override-clear-all ──────────────────────────────────────

(def-suite palette-clear-all-suite
  :description "%palette-override-clear-all: drops all overrides atomically"
  :in terminal-suite)
(in-suite palette-clear-all-suite)

(test palette-override-clear-all-drops-vector
  "%palette-override-clear-all sets palette-overrides back to NIL."
  (with-screen (s 10 5)
    (cl-tmux/terminal/types:%palette-override-set s 0 #xFF0000)
    (cl-tmux/terminal/types:%palette-override-set s 255 #x00FF00)
    (is-true (cl-tmux/terminal/types:screen-palette-overrides s)
             "pre-condition: palette-overrides must be non-NIL after set")
    (cl-tmux/terminal/types:%palette-override-clear-all s)
    (is (null (cl-tmux/terminal/types:screen-palette-overrides s))
        "palette-overrides must be NIL after %palette-override-clear-all")))

(test palette-override-clear-all-on-empty-screen-is-noop
  "%palette-override-clear-all on a fresh screen (no vector) is a no-op."
  (with-screen (s 10 5)
    (finishes (cl-tmux/terminal/types:%palette-override-clear-all s))
    (is (null (cl-tmux/terminal/types:screen-palette-overrides s))
        "palette-overrides must still be NIL after clear-all on empty screen")))

(test palette-override-clear-all-makes-all-indices-return-nil
  "After %palette-override-clear-all, %palette-override-get returns NIL for every index."
  (with-screen (s 10 5)
    (cl-tmux/terminal/types:%palette-override-set s 0   #x111111)
    (cl-tmux/terminal/types:%palette-override-set s 128 #x888888)
    (cl-tmux/terminal/types:%palette-override-set s 255 #xFFFFFF)
    (cl-tmux/terminal/types:%palette-override-clear-all s)
    (is (null (cl-tmux/terminal/types:%palette-override-get s 0))
        "index 0 must return NIL after clear-all")
    (is (null (cl-tmux/terminal/types:%palette-override-get s 128))
        "index 128 must return NIL after clear-all")
    (is (null (cl-tmux/terminal/types:%palette-override-get s 255))
        "index 255 must return NIL after clear-all")))
