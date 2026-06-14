(in-package #:cl-tmux/test)

;;;; Emulator tests (src/terminal/emulator.lisp and scroll helpers).
;;;; Tests: copy-mode scrollback projection, screen-display-cell OOB,
;;;;        screen-process-bytes keyword arguments, and trim-scroll-history.

;;; ── SUITE: copy-mode scrollback projection ──────────────────────────────────

(def-suite copy-mode
  :description "Scrollback capture and copy-mode viewport projection"
  :in terminal-suite)
(in-suite copy-mode)

(test scrollback-accumulates
  "Auto-scrolling a full screen pushes displaced top rows into the scrollback."
  (with-screen (s 5 3)
    (feed-lines s "L0" "L1" "L2" "L3" "L4")   ; 5 lines into a 3-row screen
    ;; Two scrolls happened, so the two oldest rows are in scrollback,
    ;; newest-first: L1 then L0.
    (is (= 2 (length (screen-scrollback s))))
    ;; Live grid now shows the most recent three lines.
    (is (string= "L2" (row-string s 0 :end 2)))
    (is (string= "L4" (row-string s 2 :end 2)))))

(test copy-offset-projects-history
  "screen-display-cell shifts the viewport into scrollback by copy-offset rows."
  (with-screen (s 5 3)
    (feed-lines s "L0" "L1" "L2" "L3" "L4")
    (setf (screen-copy-mode-p s) t)
    ;; Offset 0: viewport is the live grid unchanged.
    (setf (screen-copy-offset s) 0)
    (is (string= "L2" (display-row-string s 0 :end 2)))
    (is (string= "L4" (display-row-string s 2 :end 2)))
    ;; Offset 1: top row is newest scrollback line (L1); live grid pushed down.
    (setf (screen-copy-offset s) 1)
    (is (string= "L1" (display-row-string s 0 :end 2)))
    (is (string= "L2" (display-row-string s 1 :end 2)))
    (is (string= "L3" (display-row-string s 2 :end 2)))
    ;; Offset 2: the two scrollback lines (L0, L1) sit above the live top (L2).
    (setf (screen-copy-offset s) 2)
    (is (string= "L0" (display-row-string s 0 :end 2)))
    (is (string= "L1" (display-row-string s 1 :end 2)))
    (is (string= "L2" (display-row-string s 2 :end 2)))))

(test copy-mode-off-ignores-offset
  "A stale copy-offset is ignored entirely when copy mode is off."
  (with-screen (s 5 3)
    (feed-lines s "L0" "L1" "L2" "L3" "L4")
    (setf (screen-copy-mode-p s) nil
          (screen-copy-offset s) 2)  ; should have no effect
    (is (string= "L2" (display-row-string s 0 :end 2)))
    (is (string= "L4" (display-row-string s 2 :end 2)))))

;;; ── Coverage: screen-display-cell out-of-range reads ────────────────────────
;;;
;;; modes.lisp returns *display-blank-cell* for two out-of-range conditions:
;;;   1. col exceeds the length of a scrollback row-vector
;;;   2. live-row exceeds screen-height (i.e. offset > scrollback depth)
;;; These paths were previously uncovered by any test.

(def-suite display-cell-oob
  :description "screen-display-cell fallback to *display-blank-cell* for out-of-range reads"
  :in copy-mode)
(in-suite display-cell-oob)

(test display-cell-scrollback-col-oob-returns-blank
  "screen-display-cell returns the blank-cell fallback when COL exceeds the
   length of the requested scrollback row-vector.
   This happens when an old row was narrower than the current screen width."
  (with-screen (s 10 3)
    ;; Build a scrollback row that is only 3 wide (narrower than screen width 10).
    (let ((narrow-row (make-array 3 :initial-element
                                    (cl-tmux/terminal/types:blank-cell))))
      (setf (cl-tmux/terminal/types:screen-scrollback s) (list narrow-row))
      (setf (cl-tmux/terminal/types:screen-copy-mode-p s) t
            (cl-tmux/terminal/types:screen-copy-offset  s) 1))
    ;; col 5 is outside the 3-wide row — should return the blank-cell fallback.
    (let ((cell (cl-tmux/terminal/actions:screen-display-cell s 5 0)))
      (is (char= #\Space (cl-tmux/terminal/types:cell-char cell))
          "out-of-range col in scrollback must return a blank cell"))))

(test display-cell-live-row-oob-returns-blank
  "screen-display-cell returns the blank-cell fallback when live-row exceeds
   screen-height.  This happens when the caller passes a row argument that is
   >= height with offset=0, which can occur in renderers that probe beyond the
   live grid boundary."
  (with-screen (s 5 3)
    (feed-lines s "L0" "L1" "L2")
    ;; With copy-offset=0 (live grid mode) and height=3, valid live rows are 0-2.
    ;; Querying row=3 gives live-row=3 which equals height → OOB → blank cell.
    (let ((cell (cl-tmux/terminal/actions:screen-display-cell s 0 3)))
      (is (char= #\Space (cl-tmux/terminal/types:cell-char cell))
          "live-row beyond screen-height must return a blank cell"))))

(test display-cell-scrollback-nil-row-returns-blank
  :description "screen-display-cell returns blank when the scrollback row vector is NIL."
  (with-screen (s 5 3)
    ;; Push a NIL (corrupted) scrollback entry
    (setf (cl-tmux/terminal/types:screen-scrollback s) (list nil))
    (setf (cl-tmux/terminal/types:screen-copy-mode-p s) t
          (cl-tmux/terminal/types:screen-copy-offset  s) 1)
    (let ((cell (cl-tmux/terminal/actions:screen-display-cell s 0 0)))
      (is (char= #\Space (cl-tmux/terminal/types:cell-char cell))
          "NIL scrollback row must return a blank cell"))))

;;; ── SUITE: screen-process-bytes keyword arguments ────────────────────────────

(def-suite screen-process-bytes-suite
  :description "screen-process-bytes start/end keyword arguments"
  :in terminal-suite)
(in-suite screen-process-bytes-suite)

(test screen-process-bytes-start-end-slice
  :description "screen-process-bytes :start/:end process only the specified byte slice."
  (with-screen (s 10 5)
    ;; Buffer: A B C D E  — process only bytes 1..2 (B C)
    (let ((buf (map '(simple-array (unsigned-byte 8) (*)) #'char-code "ABCDE")))
      (screen-process-bytes s buf :start 1 :end 3))
    ;; Only B and C should appear on the screen
    (is (char= #\B (char-at s 0 0)) "first written char must be B")
    (is (char= #\C (char-at s 1 0)) "second written char must be C")
    ;; A, D, E must not have been written
    (is (char= #\Space (char-at s 2 0)) "third cell must remain blank")))

(test screen-process-bytes-empty-slice-is-noop
  :description "screen-process-bytes with start=end processes no bytes (no-op)."
  (with-screen (s 10 5)
    (screen-clear-dirty s)
    (let ((buf (map '(simple-array (unsigned-byte 8) (*)) #'char-code "XYZ")))
      (screen-process-bytes s buf :start 0 :end 0))
    ;; No cell should change — screen stays at initial state
    (is (char= #\Space (char-at s 0 0))
        "no bytes processed: cell (0,0) must remain blank")))

(test screen-process-bytes-processes-full-buffer-by-default
  :description "Without :start/:end, screen-process-bytes processes all bytes."
  (with-screen (s 10 5)
    (let ((buf (map '(simple-array (unsigned-byte 8) (*)) #'char-code "HELLO")))
      (screen-process-bytes s buf))
    (is (string= "HELLO" (row-string s 0 :end 5))
        "entire buffer must be processed by default")))

;;; ── SUITE: trim-scroll-history ───────────────────────────────────────────────

(def-suite trim-scroll-history-suite
  :description "trim-scroll-history caps the scrollback buffer"
  :in terminal-suite)
(in-suite trim-scroll-history-suite)

(test trim-scroll-history-caps-at-effective-limit
  :description "trim-scroll-history truncates the scrollback list when it exceeds the cap."
  (with-screen (s 5 3)
    ;; Install 5 dummy rows
    (setf (cl-tmux/terminal/types:screen-scrollback s)
          (loop repeat 5 collect (make-array 5 :initial-element
                                               (cl-tmux/terminal/types:blank-cell))))
    ;; Override the limit function to return 3
    (let ((cl-tmux/terminal/actions:*history-limit-function* (lambda () 3)))
      (cl-tmux/terminal/actions:trim-scroll-history s))
    (is (<= (length (cl-tmux/terminal/types:screen-scrollback s)) 3)
        "scrollback must be capped at 3 entries after trim")))

(test trim-scroll-history-noop-when-within-limit
  :description "trim-scroll-history does nothing when scrollback is within the limit."
  (with-screen (s 5 3)
    (setf (cl-tmux/terminal/types:screen-scrollback s)
          (loop repeat 2 collect (make-array 5 :initial-element
                                               (cl-tmux/terminal/types:blank-cell))))
    (let ((cl-tmux/terminal/actions:*history-limit-function* (lambda () 100)))
      (cl-tmux/terminal/actions:trim-scroll-history s))
    (is (= 2 (length (cl-tmux/terminal/types:screen-scrollback s)))
        "scrollback with 2 entries under limit of 100 must remain at 2")))

(test scroll-enforces-history-cap-during-feed
  :description "Scrolling a full screen through many lines caps scrollback at the default limit."
  (with-screen (s 5 3)
    ;; Feed enough lines to cause many scrolls (default cap is +max-scrollback-lines+)
    ;; We use a small cap via *history-limit-function* to keep the test fast
    (let ((cl-tmux/terminal/actions:*history-limit-function* (lambda () 5)))
      (loop for i below 20
            do (feed s (format nil "L~D" i))
            do (feed s (format nil "~C~C" #\Return #\Linefeed))))
    (is (<= (length (cl-tmux/terminal/types:screen-scrollback s)) 5)
        "scrollback must not exceed the cap of 5 even after many scrolls")))

;;; ── SUITE: decstbm (set scroll region) ──────────────────────────────────────

(def-suite decstbm-suite
  :description "decstbm sets and homes the scroll region"
  :in terminal-suite)
(in-suite decstbm-suite)

(test decstbm-sets-scroll-region
  :description "decstbm installs the specified scroll region (0-based)."
  (with-screen (s 10 10)
    (cl-tmux/terminal/actions:decstbm s 2 7)
    (is (= 2 (cl-tmux/terminal/types:screen-scroll-top    s)) "scroll-top must be 2")
    (is (= 7 (cl-tmux/terminal/types:screen-scroll-bottom s)) "scroll-bottom must be 7")))

(test decstbm-homes-cursor-to-origin
  :description "After decstbm, the cursor is at (0, 0)."
  (with-screen (s 10 10)
    (setf (cl-tmux/terminal/types:screen-cursor-x s) 5
          (cl-tmux/terminal/types:screen-cursor-y s) 5)
    (cl-tmux/terminal/actions:decstbm s 2 7)
    (check-cursor s 0 0)))

(test decstbm-rejects-invalid-region
  :description "decstbm ignores a region where top >= bottom."
  (with-screen (s 10 10)
    ;; Pre-condition: default scroll region (0..9)
    (let ((old-top    (cl-tmux/terminal/types:screen-scroll-top    s))
          (old-bottom (cl-tmux/terminal/types:screen-scroll-bottom s)))
      ;; top=5, bottom=5 is invalid (not strictly less than)
      (cl-tmux/terminal/actions:decstbm s 5 5)
      ;; Region should be unchanged
      (is (= old-top    (cl-tmux/terminal/types:screen-scroll-top    s))
          "scroll-top must be unchanged for invalid region top=bottom")
      (is (= old-bottom (cl-tmux/terminal/types:screen-scroll-bottom s))
          "scroll-bottom must be unchanged for invalid region top=bottom"))))

(test decstbm-clamps-bottom-to-height-minus-one
  :description "decstbm clamps bottom to height-1 when given a value >= height."
  (with-screen (s 10 10)
    (cl-tmux/terminal/actions:decstbm s 0 99)
    (is (= 9 (cl-tmux/terminal/types:screen-scroll-bottom s))
        "scroll-bottom must be clamped to height-1 (9)")))

;;; ── SUITE: screen-consume-bell ───────────────────────────────────────────────

(def-suite screen-consume-bell-suite
  :description "screen-consume-bell atomically reads and clears the bell-pending flag"
  :in terminal-suite)
(in-suite screen-consume-bell-suite)

(test screen-consume-bell-pending-and-idempotent
  "screen-consume-bell: returns T and clears bell-pending; a second call returns NIL."
  (with-screen (s 10 5)
    (screen-process-bytes s (make-array 1 :element-type '(unsigned-byte 8)
                                          :initial-contents '(#x07)))
    (is (cl-tmux/terminal/types:screen-bell-pending s)
        "bell-pending must be T before consume-bell")
    (is (cl-tmux/terminal/types:screen-consume-bell s)
        "screen-consume-bell must return T when bell was pending")
    (is-false (cl-tmux/terminal/types:screen-bell-pending s)
              "bell-pending must be NIL after screen-consume-bell")
    (is-false (cl-tmux/terminal/types:screen-consume-bell s)
              "second consume-bell must return NIL (bell already consumed)")))

;;; ── SUITE: screen-resize emulator integration ────────────────────────────────

(def-suite screen-resize-suite
  :description "screen-resize adjusts width/height and preserves on-screen content"
  :in terminal-suite)
(in-suite screen-resize-suite)

(test screen-resize-dimensions-table
  "screen-resize updates width and height for both growing and shrinking."
  (dolist (row '((10 5  20 10 "grow:   10x5 → 20x10")
                 (20 10 10 5  "shrink: 20x10 → 10x5")))
    (destructuring-bind (init-w init-h new-w new-h desc) row
      (with-screen (s init-w init-h)
        (screen-resize s new-w new-h)
        (is (= new-w (screen-width  s)) "~A: width"  desc)
        (is (= new-h (screen-height s)) "~A: height" desc)))))

(test screen-resize-preserves-content-within-new-bounds
  :description "Content written before a grow-resize is accessible afterwards."
  (with-screen (s 5 3)
    (feed s "hello")
    (screen-resize s 10 5)
    ;; Original content at (0..4, 0) must still be readable.
    (check-row s 0 "hello")))

(test screen-resize-clamps-cursor-inside-new-bounds
  :description "After shrink-resize, the cursor is clamped inside the new grid."
  (with-screen (s 20 10)
    ;; Move cursor to a position that would be out of bounds after shrink.
    (feed s (esc "[10;20H"))   ; row 9, col 19
    (screen-resize s 5 3)
    ;; Cursor must be clamped to fit the new 5x3 grid.
    (is (<= (screen-cursor-x s) 4)
        "cursor-x must be clamped to width-1 after shrink")
    (is (<= (screen-cursor-y s) 2)
        "cursor-y must be clamped to height-1 after shrink")))
