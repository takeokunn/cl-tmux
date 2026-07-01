(in-package #:cl-tmux/test)

;;;; Screen tests — part IV: boolean-slot triples, screen-title-stack, screen-cwd,
;;;;   screen-pending-wrap, screen-focus-events, G0/G1/active-g, passthrough-queue,
;;;;   clipboard-queue, screen-palette-overrides (direct slot), wrap helpers,
;;;;   ANSI modes (IRM/LNM/DECSCNM), copy-search-direction, copy-rect-select-p,
;;;;   and the default-screen-width/height geometry constants.
;;;;
;;;; The define-boolean-slot-tests macro below eliminates the boilerplate
;;;; (1) defaults-NIL, (2) enable-sequence, (3) disable-sequence triple that
;;;; is otherwise repeated ~5 times verbatim for insert-mode, newline-mode,
;;;; reverse-screen, focus-events, and similar boolean screen slots.

;;; ── Boolean-slot test macro ───────────────────────────────────────────────
;;;
;;; Each generated triple tests:
;;;   1. The slot defaults to NIL on a fresh screen.
;;;   2. A specific escape sequence sets it to T.
;;;   3. A complementary sequence clears it to NIL.

(defmacro define-boolean-slot-tests
    (slot-accessor suite-name enable-sequence disable-sequence
     &key (suite-description (symbol-name suite-name))
          (parent-suite 'terminal-suite))
  "Generate a def-suite + three fiveam tests for a boolean screen slot.

   SLOT-ACCESSOR    — accessor symbol (e.g. cl-tmux/terminal/types:screen-insert-mode)
   SUITE-NAME       — unquoted symbol naming the def-suite
   ENABLE-SEQUENCE  — form that feeds the enabling sequence to screen variable S
   DISABLE-SEQUENCE — form that feeds the disabling sequence to screen variable S"
  (let* ((name (symbol-name slot-accessor))
         (default-test  (intern (format nil "~A-DEFAULTS-FALSE" name)))
         (enabled-test  (intern (format nil "~A-ENABLED-BY-SEQUENCE" name)))
         (disabled-test (intern (format nil "~A-DISABLED-BY-SEQUENCE" name))))
    `(progn
       (def-suite ,suite-name
         :description ,suite-description
         :in ,parent-suite)
       (in-suite ,suite-name)
       (test ,default-test
         ,(format nil "~A is NIL on a fresh screen." name)
         (with-screen (s 10 5)
           (is-false (,slot-accessor s)
                     ,(format nil "~A must be NIL initially" name))))
       (test ,enabled-test
         ,(format nil "~A is T after the enable sequence." name)
         (with-screen (s 10 5)
           ,enable-sequence
           (is-true (,slot-accessor s)
                    ,(format nil "~A must be T after enable sequence" name))))
       (test ,disabled-test
         ,(format nil "~A is NIL after the disable sequence." name)
         (with-screen (s 10 5)
           ,enable-sequence
           ,disable-sequence
           (is-false (,slot-accessor s)
                     ,(format nil "~A must be NIL after disable sequence" name)))))))

;;; ── SUITE: screen-title-stack ────────────────────────────────────────────────
;;;
;;; XTPUSHTITLE / XTPOPTITLE: a stack of saved title strings, bounded to
;;; +title-stack-max-depth+ = 8 entries.

(def-suite title-stack-suite
  :description "screen-title-stack slot: defaults, push, pop, depth limit"
  :in terminal-suite)
(in-suite title-stack-suite)

(test screen-title-stack-defaults-nil
  "screen-title-stack is NIL on a fresh screen."
  (with-screen (s 10 5)
    (is (null (cl-tmux/terminal/types:screen-title-stack s))
        "title-stack must be NIL initially")))

(test screen-title-stack-push-pop-via-sequences
  "ESC[>0t pushes the current title; ESC[<0t pops and restores it."
  (with-screen (s 10 5)
    (feed s (format nil "~C]2;MyTitle~C\\" #\Escape #\Escape))
    (is (string= "MyTitle" (cl-tmux/terminal/types:screen-title s))
        "pre-condition: title must be MyTitle")
    (feed s (esc "[>0t"))
    (is (not (null (cl-tmux/terminal/types:screen-title-stack s)))
        "title-stack must be non-NIL after push")
    (feed s (format nil "~C]2;NewTitle~C\\" #\Escape #\Escape))
    (is (string= "NewTitle" (cl-tmux/terminal/types:screen-title s))
        "title must change to NewTitle after OSC 2")
    (feed s (esc "[<0t"))
    (is (string= "MyTitle" (cl-tmux/terminal/types:screen-title s))
        "title must be restored to MyTitle after pop")))

(test screen-title-stack-depth-limit
  "Pushing beyond +title-stack-max-depth+ does not grow the stack beyond the limit."
  (with-screen (s 10 5)
    (dotimes (_ (+ cl-tmux/terminal/types:+title-stack-max-depth+ 2))
      (feed s (esc "[>0t")))
    (is (<= (length (cl-tmux/terminal/types:screen-title-stack s))
            cl-tmux/terminal/types:+title-stack-max-depth+)
        "title-stack must never exceed +title-stack-max-depth+ entries")))

;;; ── SUITE: screen-cwd ────────────────────────────────────────────────────────

(def-suite screen-cwd-suite
  :description "screen-cwd slot: default empty string and OSC 7 update"
  :in terminal-suite)
(in-suite screen-cwd-suite)

(test screen-cwd-defaults-empty-string
  "screen-cwd is the empty string on a fresh screen."
  (with-screen (s 10 5)
    (is (string= "" (cl-tmux/terminal/types:screen-cwd s))
        "cwd must be empty string initially")))

(test screen-cwd-can-be-set-directly
  "screen-cwd can be set to an arbitrary string via setf."
  (with-screen (s 10 5)
    (setf (cl-tmux/terminal/types:screen-cwd s) "/home/user/project")
    (is (string= "/home/user/project" (cl-tmux/terminal/types:screen-cwd s))
        "cwd must hold the value after setf")))

(test screen-cwd-updated-by-osc7
  "OSC 7 ; file://host/path sets screen-cwd to a non-empty value."
  (with-screen (s 20 5)
    (feed s (format nil "~C]7;file://localhost/tmp/foo~C\\" #\Escape #\Escape))
    (is (string/= "" (cl-tmux/terminal/types:screen-cwd s))
        "cwd must be non-empty after OSC 7 ; file://host/path")))

;;; ── SUITE: screen-pending-wrap ───────────────────────────────────────────────

(def-suite pending-wrap-suite
  :description "screen-pending-wrap slot: default, set, clear"
  :in terminal-suite)
(in-suite pending-wrap-suite)

(test screen-pending-wrap-defaults-false
  "screen-pending-wrap is NIL on a fresh screen."
  (with-screen (s 10 5)
    (is-false (cl-tmux/terminal/types:screen-pending-wrap s)
              "pending-wrap must be NIL initially")))

(test screen-pending-wrap-set-when-cursor-at-last-column
  "Writing a character into the last column with autowrap sets pending-wrap."
  (with-screen (s 3 2)
    (feed s "abc")
    (is-true (cl-tmux/terminal/types:screen-pending-wrap s)
             "pending-wrap must be T after filling the last column with autowrap on")))

(test screen-pending-wrap-cleared-on-wrap
  "pending-wrap is cleared when the next character triggers an actual wrap."
  (with-screen (s 3 2)
    (feed s "abc")
    (is-true (cl-tmux/terminal/types:screen-pending-wrap s) "pre-condition")
    (feed s "d")
    (is-false (cl-tmux/terminal/types:screen-pending-wrap s)
              "pending-wrap must be cleared after a character is written following a wrap")))

(test screen-pending-wrap-cleared-by-cursor-move
  "Any explicit cursor movement (CR) clears pending-wrap."
  (with-screen (s 3 2)
    (feed s "abc")
    (feed s (string #\Return))
    (is-false (cl-tmux/terminal/types:screen-pending-wrap s)
              "pending-wrap must be cleared by CR")))

;;; ── SUITE: screen-focus-events (using define-boolean-slot-tests) ─────────────

(define-boolean-slot-tests
  cl-tmux/terminal/types:screen-focus-events
  focus-events-suite
  (feed s (esc "[?1004h"))   ; ?1004h enables focus event reporting
  (feed s (esc "[?1004l"))   ; ?1004l disables focus event reporting
  :suite-description "screen-focus-events: defaults NIL, ?1004h enables, ?1004l disables")

;;; ── SUITE: G0/G1 charset designation and SO/SI ───────────────────────────────

(def-suite g0-g1-charset-suite
  :description "screen-g0-charset, screen-g1-charset, screen-active-g: defaults and sequences"
  :in terminal-suite)
(in-suite g0-g1-charset-suite)

(test screen-g0-charset-defaults-ascii
  "screen-g0-charset defaults to :ascii on a fresh screen."
  (with-screen (s 10 5)
    (is (eq :ascii (cl-tmux/terminal/types:screen-g0-charset s))
        "g0-charset must default to :ascii")))

(test screen-g1-charset-defaults-ascii
  "screen-g1-charset defaults to :ascii on a fresh screen."
  (with-screen (s 10 5)
    (is (eq :ascii (cl-tmux/terminal/types:screen-g1-charset s))
        "g1-charset must default to :ascii")))

(test screen-active-g-defaults-g0
  "screen-active-g defaults to :g0 on a fresh screen."
  (with-screen (s 10 5)
    (is (eq :g0 (cl-tmux/terminal/types:screen-active-g s))
        "active-g must default to :g0")))

(test screen-g0-charset-designated-by-esc-paren-0
  "ESC ( 0 designates G0 as DEC special graphics."
  (with-screen (s 10 5)
    (feed s (esc "(0"))
    (is (eq :dec-graphics (cl-tmux/terminal/types:screen-g0-charset s))
        "g0-charset must be :dec-graphics after ESC(0")))

(test screen-g1-charset-designated-by-esc-paren-0
  "ESC ) 0 designates G1 as DEC special graphics."
  (with-screen (s 10 5)
    (feed s (esc ")0"))
    (is (eq :dec-graphics (cl-tmux/terminal/types:screen-g1-charset s))
        "g1-charset must be :dec-graphics after ESC)0")))

(test screen-active-g-toggled-by-so-si
  "SO (0x0E) selects G1; SI (0x0F) selects G0."
  (with-screen (s 10 5)
    (screen-process-bytes s (make-array 1 :element-type '(unsigned-byte 8)
                                          :initial-contents '(#x0E)))
    (is (eq :g1 (cl-tmux/terminal/types:screen-active-g s))
        "active-g must be :g1 after SO (0x0E)")
    (screen-process-bytes s (make-array 1 :element-type '(unsigned-byte 8)
                                          :initial-contents '(#x0F)))
    (is (eq :g0 (cl-tmux/terminal/types:screen-active-g s))
        "active-g must be :g0 after SI (0x0F)")))

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
