(in-package #:cl-tmux/test)

;;;; Emulator tests (src/terminal/emulator.lisp and scroll helpers).
;;;; Tests: copy-mode scrollback projection, screen-display-cell OOB,
;;;;        screen-process-bytes keyword arguments, and trim-scroll-history.

;;; ── SUITE: copy-mode scrollback projection ──────────────────────────────────

(describe "terminal-suite/copy-mode"

  ;; Auto-scrolling a full screen pushes displaced top rows into the scrollback.
  (it "scrollback-accumulates"
    (with-screen (s 5 3)
      (feed-lines s "L0" "L1" "L2" "L3" "L4")   ; 5 lines into a 3-row screen
      ;; Two scrolls happened, so the two oldest rows are in scrollback,
      ;; newest-first: L1 then L0.
      (expect (= 2 (length (screen-scrollback s))))
      ;; Live grid now shows the most recent three lines.
      (expect (string= "L2" (row-string s 0 :end 2)))
      (expect (string= "L4" (row-string s 2 :end 2)))))

  ;; screen-display-cell shifts the viewport into scrollback by copy-offset rows.
  (it "copy-offset-projects-history"
    (with-screen (s 5 3)
      (feed-lines s "L0" "L1" "L2" "L3" "L4")
      (setf (screen-copy-mode-p s) t)
      (dolist (group '((0 ((0 "L2") (2 "L4")))
                       (1 ((0 "L1") (1 "L2") (2 "L3")))
                       (2 ((0 "L0") (1 "L1") (2 "L2")))))
        (destructuring-bind (offset checks) group
          (setf (screen-copy-offset s) offset)
          (check-table (loop for (row expected) in checks
                             collect (list (display-row-string s row :end 2)
                                           expected
                                           (format nil "offset ~D row ~D"
                                                   offset row)))
                       :test #'string=)))))

  ;; A copy-offset is ignored entirely when copy mode is off.
  (it "copy-mode-off-ignores-offset"
    (with-screen (s 5 3)
      (feed-lines s "L0" "L1" "L2" "L3" "L4")
      (setf (screen-copy-mode-p s) nil
            (screen-copy-offset s) 2)  ; should have no effect
      (expect (string= "L2" (display-row-string s 0 :end 2)))
      (expect (string= "L4" (display-row-string s 2 :end 2))))))

;;; ── Coverage: screen-display-cell out-of-range reads ────────────────────────
;;;
;;; modes.lisp returns *display-blank-cell* for two out-of-range conditions:
;;;   1. col exceeds the length of a scrollback row-vector
;;;   2. live-row exceeds screen-height (i.e. offset > scrollback depth)
;;; These paths were previously uncovered by any test.

(describe "copy-mode/display-cell-oob"

  ;; screen-display-cell returns the blank-cell fallback when COL exceeds the
  ;; length of the requested scrollback row-vector.
  ;; This happens when an old row was narrower than the current screen width.
  (it "display-cell-scrollback-col-oob-returns-blank"
    (with-screen (s 10 3)
      ;; Build a scrollback row that is only 3 wide (narrower than screen width 10).
      (let ((narrow-row (make-array 3 :initial-element
                                      (cl-tmux/terminal/types:blank-cell))))
        (setf (cl-tmux/terminal/types:screen-scrollback s) (list narrow-row))
        (setf (cl-tmux/terminal/types:screen-copy-mode-p s) t
              (cl-tmux/terminal/types:screen-copy-offset  s) 1))
      ;; col 5 is outside the 3-wide row — should return the blank-cell fallback.
      (let ((cell (cl-tmux/terminal/actions:screen-display-cell s 5 0)))
        (expect (char= #\Space (cl-tmux/terminal/types:cell-char cell))))))

  ;; screen-display-cell returns the blank-cell fallback when live-row exceeds
  ;; screen-height.  This happens when the caller passes a row argument that is
  ;; >= height with offset=0, which can occur in renderers that probe beyond the
  ;; live grid boundary.
  (it "display-cell-live-row-oob-returns-blank"
    (with-screen (s 5 3)
      (feed-lines s "L0" "L1" "L2")
      ;; With copy-offset=0 (live grid mode) and height=3, valid live rows are 0-2.
      ;; Querying row=3 gives live-row=3 which equals height → OOB → blank cell.
      (let ((cell (cl-tmux/terminal/actions:screen-display-cell s 0 3)))
        (expect (char= #\Space (cl-tmux/terminal/types:cell-char cell))))))

  ;; screen-display-cell returns blank when the scrollback row vector is NIL.
  (it "display-cell-scrollback-nil-row-returns-blank"
    (with-screen (s 5 3)
      ;; Push a NIL (corrupted) scrollback entry
      (setf (cl-tmux/terminal/types:screen-scrollback s) (list nil))
      (setf (cl-tmux/terminal/types:screen-copy-mode-p s) t
            (cl-tmux/terminal/types:screen-copy-offset  s) 1)
      (let ((cell (cl-tmux/terminal/actions:screen-display-cell s 0 0)))
        (expect (char= #\Space (cl-tmux/terminal/types:cell-char cell)))))))

;;; ── SUITE: screen-process-bytes keyword arguments ────────────────────────────

(describe "terminal-suite/screen-process-bytes-suite"

  ;; screen-process-bytes :start/:end process only the specified byte slice.
  (it "screen-process-bytes-start-end-slice"
    (with-screen (s 10 5)
      ;; Buffer: A B C D E  — process only bytes 1..2 (B C)
      (let ((buf (map '(simple-array (unsigned-byte 8) (*)) #'char-code "ABCDE")))
        (screen-process-bytes s buf :start 1 :end 3))
      ;; Only B and C should appear on the screen
      (expect (char= #\B (char-at s 0 0)))
      (expect (char= #\C (char-at s 1 0)))
      ;; A, D, E must not have been written
      (expect (char= #\Space (char-at s 2 0)))))

  ;; screen-process-bytes with start=end processes no bytes (no-op).
  (it "screen-process-bytes-empty-slice-is-noop"
    (with-screen (s 10 5)
      (screen-clear-dirty s)
      (let ((buf (map '(simple-array (unsigned-byte 8) (*)) #'char-code "XYZ")))
        (screen-process-bytes s buf :start 0 :end 0))
      ;; No cell should change — screen stays at initial state
      (expect (char= #\Space (char-at s 0 0)))))

  ;; Without :start/:end, screen-process-bytes processes all bytes.
  (it "screen-process-bytes-processes-full-buffer-by-default"
    (with-screen (s 10 5)
      (let ((buf (map '(simple-array (unsigned-byte 8) (*)) #'char-code "HELLO")))
        (screen-process-bytes s buf))
      (expect (string= "HELLO" (row-string s 0 :end 5))))))

;;; ── SUITE: trim-scroll-history ───────────────────────────────────────────────

(describe "terminal-suite/trim-scroll-history-suite"

  ;; trim-scroll-history truncates the scrollback list when it exceeds the cap.
  (it "trim-scroll-history-caps-at-effective-limit"
    (with-screen (s 5 3)
      ;; Install 5 dummy rows
      (setf (cl-tmux/terminal/types:screen-scrollback s)
            (loop repeat 5 collect (make-array 5 :initial-element
                                                 (cl-tmux/terminal/types:blank-cell))))
      ;; Override the limit function to return 3
      (let ((cl-tmux/terminal/actions:*history-limit-function* (lambda () 3)))
        (cl-tmux/terminal/actions:trim-scroll-history s))
      (expect (<= (length (cl-tmux/terminal/types:screen-scrollback s)) 3))))

  ;; trim-scroll-history does nothing when scrollback is within the limit.
  (it "trim-scroll-history-noop-when-within-limit"
    (with-screen (s 5 3)
      (setf (cl-tmux/terminal/types:screen-scrollback s)
            (loop repeat 2 collect (make-array 5 :initial-element
                                                 (cl-tmux/terminal/types:blank-cell))))
      (let ((cl-tmux/terminal/actions:*history-limit-function* (lambda () 100)))
        (cl-tmux/terminal/actions:trim-scroll-history s))
      (expect (= 2 (length (cl-tmux/terminal/types:screen-scrollback s))))))

  ;; Scrolling a full screen through many lines caps scrollback at the default limit.
  (it "scroll-enforces-history-cap-during-feed"
    (with-screen (s 5 3)
      ;; Feed enough lines to cause many scrolls (default cap is +max-scrollback-lines+)
      ;; We use a small cap via *history-limit-function* to keep the test fast
      (let ((cl-tmux/terminal/actions:*history-limit-function* (lambda () 5)))
        (loop for i below 20
              do (feed s (format nil "L~D" i))
              do (feed s (format nil "~C~C" #\Return #\Linefeed))))
      (expect (<= (length (cl-tmux/terminal/types:screen-scrollback s)) 5)))))

;;; ── SUITE: decstbm (set scroll region) ──────────────────────────────────────

(describe "terminal-suite/decstbm-suite"

  ;; decstbm installs the specified scroll region (0-based).
  (it "decstbm-sets-scroll-region"
    (with-screen (s 10 10)
      (cl-tmux/terminal/actions:decstbm s 2 7)
      (expect (= 2 (cl-tmux/terminal/types:screen-scroll-top    s)))
      (expect (= 7 (cl-tmux/terminal/types:screen-scroll-bottom s)))))

  ;; After decstbm, the cursor is at (0, 0).
  (it "decstbm-homes-cursor-to-origin"
    (with-screen (s 10 10)
      (setf (cl-tmux/terminal/types:screen-cursor-x s) 5
            (cl-tmux/terminal/types:screen-cursor-y s) 5)
      (cl-tmux/terminal/actions:decstbm s 2 7)
      (check-cursor s 0 0)))

  ;; decstbm ignores a region where top >= bottom.
  (it "decstbm-rejects-invalid-region"
    (with-screen (s 10 10)
      ;; Pre-condition: default scroll region (0..9)
      (let ((old-top    (cl-tmux/terminal/types:screen-scroll-top    s))
            (old-bottom (cl-tmux/terminal/types:screen-scroll-bottom s)))
        ;; top=5, bottom=5 is invalid (not strictly less than)
        (cl-tmux/terminal/actions:decstbm s 5 5)
        ;; Region should be unchanged
        (expect (= old-top    (cl-tmux/terminal/types:screen-scroll-top    s)))
        (expect (= old-bottom (cl-tmux/terminal/types:screen-scroll-bottom s))))))

  ;; decstbm clamps bottom to height-1 when given a value >= height.
  (it "decstbm-clamps-bottom-to-height-minus-one"
    (with-screen (s 10 10)
      (cl-tmux/terminal/actions:decstbm s 0 99)
      (expect (= 9 (cl-tmux/terminal/types:screen-scroll-bottom s))))))

;;; ── SUITE: screen-consume-bell ───────────────────────────────────────────────

(describe "terminal-suite/screen-consume-bell-suite"

  ;; screen-consume-bell: returns T and clears bell-pending; a second call returns NIL.
  (it "screen-consume-bell-pending-and-idempotent"
    (with-screen (s 10 5)
      (screen-process-bytes s (make-array 1 :element-type '(unsigned-byte 8)
                                            :initial-contents '(#x07)))
      (expect (cl-tmux/terminal/types:screen-bell-pending s))
      (expect (cl-tmux/terminal/types:screen-consume-bell s))
      (expect (cl-tmux/terminal/types:screen-bell-pending s) :to-be-falsy)
      (expect (cl-tmux/terminal/types:screen-consume-bell s) :to-be-falsy))))

;;; ── SUITE: screen-resize emulator integration ────────────────────────────────

(describe "terminal-suite/screen-resize-suite"

  ;; screen-resize updates width and height for both growing and shrinking.
  (it "screen-resize-dimensions-table"
    (dolist (row '((10 5  20 10 "grow:   10x5 → 20x10")
                   (20 10 10 5  "shrink: 20x10 → 10x5")))
      (destructuring-bind (init-w init-h new-w new-h desc) row
        (declare (ignore desc))
        (with-screen (s init-w init-h)
          (screen-resize s new-w new-h)
          (expect (= new-w (screen-width  s)))
          (expect (= new-h (screen-height s)))))))

  ;; Content written before a grow-resize is accessible afterwards.
  (it "screen-resize-preserves-content-within-new-bounds"
    (with-screen (s 5 3)
      (feed s "hello")
      (screen-resize s 10 5)
      ;; Original content at (0..4, 0) must still be readable.
      (check-row s 0 "hello")))

  ;; After shrink-resize, the cursor is clamped inside the new grid.
  (it "screen-resize-clamps-cursor-inside-new-bounds"
    (with-screen (s 20 10)
      ;; Move cursor to a position that would be out of bounds after shrink.
      (feed s (esc "[10;20H"))   ; row 9, col 19
      (screen-resize s 5 3)
      ;; Cursor must be clamped to fit the new 5x3 grid.
      (expect (<= (screen-cursor-x s) 4))
      (expect (<= (screen-cursor-y s) 2)))))
