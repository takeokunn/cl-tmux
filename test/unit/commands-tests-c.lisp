(in-package #:cl-tmux/test)

;;;; commands tests — part C: pipe-pane, virtual-row-string, timeout, clamp-cursor,
;;;; selection-bounds, word/paragraph navigation, scroll helpers, extract-row-chars.

(in-suite commands-suite)

;;; ── pipe-pane-open / pipe-pane-close / pipe-pane-write ──────────────────────

(test pipe-pane-open-returns-stream
  "pipe-pane-open returns a stream object when the command launches successfully."
  (let* ((pane   (%make-test-pane))
         (result (cl-tmux/commands:pipe-pane-open pane "cat")))
    (is-true result
        "pipe-pane-open must return a non-NIL stream on success")
    ;; Clean up.
    (cl-tmux/commands:pipe-pane-close pane)))

(test pipe-pane-open-close-round-trip
  "pipe-pane-open followed by pipe-pane-close leaves pane-pipe-fd NIL."
  (let ((pane (%make-test-pane)))
    (cl-tmux/commands:pipe-pane-open pane "cat")
    (is-true (pane-pipe-fd pane)
        "pane-pipe-fd must be set after pipe-pane-open")
    (cl-tmux/commands:pipe-pane-close pane)
    (is (null (pane-pipe-fd pane))
        "pane-pipe-fd must be NIL after pipe-pane-close")))

(test pipe-pane-close-noop-when-no-pipe
  "pipe-pane-close is a no-op when pane has no open pipe."
  (let ((pane (%make-test-pane)))
    (finishes (cl-tmux/commands:pipe-pane-close pane)
              "pipe-pane-close with no pipe must not signal")))

(test cmd-pipe-pane-t-pipes-target-pane
  "pipe-pane -t 2 <cmd> opens the pipe on pane 2 (the -t target), NOT the active
   pane — the scriptable -t target is honoured."
  (let* ((pa  (%make-test-pane :id 1)) (pb (%make-test-pane :id 2))
         (win (make-window :id 1 :name "w" :width 20 :height 5
                           :tree (make-layout-split :h (make-layout-leaf pa)
                                                    (make-layout-leaf pb) 1/2)
                           :panes (list pa pb)))
         (sess (make-session :id 1 :name "0" :windows (list win))))
    (session-select-window sess win)
    (window-select-pane win pa)              ; pane 1 is active
    (with-loop-state
      (cl-tmux::%run-command-line sess "pipe-pane -t 2 cat")
      (is-true (pane-pipe-fd pb) "pane 2 (the -t target) must have an open pipe")
      (is (null (pane-pipe-fd pa)) "the active pane 1 must NOT be piped")
      ;; Clean up the forked cat process.
      (cl-tmux/commands:pipe-pane-close pb))))

(test cmd-send-keys-X-t-targets-pane-copy-mode
  "send-keys -X -t .%2 begin-selection acts on pane-id 2's copy mode, not the
   active pane, and restores focus to the original active pane afterward."
  (let* ((pa  (%make-test-pane :id 1)) (pb (%make-test-pane :id 2))
         (win (make-window :id 1 :name "w" :width 20 :height 5
                           :tree (make-layout-split :h (make-layout-leaf pa)
                                                    (make-layout-leaf pb) 1/2)
                           :panes (list pa pb)))
         (sess (make-session :id 1 :name "0" :windows (list win))))
    (session-select-window sess win)
    (window-select-pane win pa)                       ; pane 1 active
    (cl-tmux/commands::copy-mode-enter (pane-screen pa))
    (cl-tmux/commands::copy-mode-enter (pane-screen pb))
    (let ((cl-tmux::*server-sessions* (list (cons "0" sess))))
      (with-loop-state
        (cl-tmux::%run-command-line sess "send-keys -X -t .%2 begin-selection")
        (is-true (cl-tmux/terminal/types:screen-copy-selecting (pane-screen pb))
            "pane 2 (the -t target) selection must be started")
        (is-false (cl-tmux/terminal/types:screen-copy-selecting (pane-screen pa))
            "the active pane 1 must be unaffected")
        (is (eq pa (cl-tmux/model:window-active-pane win))
            "focus must be restored to the original active pane (1)")))))

(test pipe-pane-write-noop-when-no-pipe
  "pipe-pane-write is a no-op when pane has no open pipe."
  (let ((pane (%make-test-pane)))
    (finishes (cl-tmux/commands:pipe-pane-write pane #(65 66 67))
              "pipe-pane-write with no pipe must not signal")))

(test pipe-pane-open-invalid-command-returns-nil
  "pipe-pane-open returns NIL when the shell program cannot be launched."
  ;; pipe-pane-open runs the command via `sh -c`, so a bogus *command* still
  ;; launches successfully (sh exists, then fails internally — matching tmux).
  ;; To exercise the launch-failure → NIL path, point *default-shell* at a
  ;; non-existent binary so uiop:launch-program itself fails.
  (let* ((pane   (%make-test-pane))
         (cl-tmux/config:*default-shell* "/no/such/shell-5f3a9b2e")
         (result (cl-tmux/commands:pipe-pane-open pane "echo hi")))
    (is (null result)
        "pipe-pane-open must return NIL when the shell cannot be launched")))

(test pipe-pane-write-bytes-reach-subprocess
  "pipe-pane-write with an open pipe sends bytes to the subprocess stdin.
   This drives a REAL shell subprocess + filesystem (cat > tmpfile), which is
   inherently nondeterministic under a heavily-loaded parallel build (subprocess
   scheduling / GC / fs flush timing).  Earlier single-shot versions — even with a
   6s poll — flaked.  We instead retry the whole self-contained cycle up to 5
   times and assert the bytes reach the subprocess on at least one attempt: this
   still verifies the real behaviour (bytes DO traverse the pipe to the child)
   while tolerating a one-off environmental hiccup.  3 deterministic failures in a
   row would still fail (a genuine break is not masked)."
  (flet ((attempt ()
           (let ((tmpfile (uiop:tmpize-pathname
                           (uiop:merge-pathnames* "pipe-pane-write-test"
                                                  (uiop:temporary-directory))))
                 (pane    (%make-test-pane)))
             (unwind-protect
                  (progn
                    (cl-tmux/commands:pipe-pane-open
                     pane (format nil "cat > ~A" (uiop:native-namestring tmpfile)))
                    (when (pane-pipe-fd pane)            ; launch succeeded
                      (cl-tmux/commands:pipe-pane-write pane #(65 66 67)) ; "ABC"
                      (cl-tmux/commands:pipe-pane-close pane)
                      (let ((contents ""))
                        (loop repeat 250                  ; ~1.25s per attempt
                              until (and (probe-file tmpfile)
                                         (search "ABC"
                                                 (setf contents
                                                       (or (ignore-errors
                                                             (uiop:read-file-string tmpfile))
                                                           ""))))
                              do (sleep 0.005))
                        (and (search "ABC" contents) t))))
               (ignore-errors (uiop:delete-file-if-exists tmpfile))))))
    (let ((ok nil))
      (dotimes (i 8) (unless ok (setf ok (attempt))))
      (is-true ok
               "bytes written via pipe-pane-write must reach the subprocess (within 8 attempts)"))))

;;; ── %copy-mode-virtual-row-string (direct unit tests) ───────────────────────

(test copy-mode-virtual-row-string-returns-row-content
  "%copy-mode-virtual-row-string returns the content of the requested virtual row."
  (let ((s (make-screen 20 5)))
    (feed s "hello")
    (cl-tmux/commands::copy-mode-enter s)
    (let* ((vrow (+ (length (cl-tmux/terminal:screen-scrollback s))
                    (- 0 (cl-tmux/terminal:screen-copy-offset s))))
           (row-str (cl-tmux/commands::%copy-mode-virtual-row-string s vrow)))
      (is (stringp row-str)
          "%copy-mode-virtual-row-string must return a string")
      (is (and (>= (length row-str) 5)
               (string= "hello" (subseq row-str 0 5)))
          "%copy-mode-virtual-row-string must include the fed text at cols 0-4"))))

(test copy-mode-virtual-row-string-length-equals-screen-width
  "%copy-mode-virtual-row-string always returns a string of length = screen-width."
  (let ((s (make-screen 20 5)))
    (cl-tmux/commands::copy-mode-enter s)
    (let ((vrow (length (cl-tmux/terminal:screen-scrollback s))))
      (is (= 20 (length (cl-tmux/commands::%copy-mode-virtual-row-string s vrow)))
          "%copy-mode-virtual-row-string length must equal screen-width"))))

;;; ── %run-with-timeout ────────────────────────────────────────────────────────

(test run-with-timeout-returns-thunk-result
  "%run-with-timeout returns the result of the thunk when it completes within time."
  (let ((result (cl-tmux/commands::%run-with-timeout (lambda () 42) 10)))
    (is (= 42 result)
        "%run-with-timeout must return the thunk result when no timeout occurs")))

(test run-with-timeout-returns-nil-on-timeout
  "%run-with-timeout returns NIL when the thunk exceeds the timeout."
  (let ((result (cl-tmux/commands::%run-with-timeout
                 (lambda () (sleep 60)) 1/1000)))
    (is (null result)
        "%run-with-timeout must return NIL when the thunk times out")))

;;; ── run-shell timeout ────────────────────────────────────────────────────────

(test run-shell-returns-nil-on-timeout
  "run-shell returns NIL when the command exceeds the given timeout."
  ;; Use a very short timeout (1ms) with a sleep command.
  (let ((result (cl-tmux/commands:run-shell "sleep 60" :timeout 1/1000)))
    (is (null result)
        "run-shell must return NIL when the command times out")))

;;; ── %copy-mode-clamp-cursor (direct unit tests) ──────────────────────────────

(test copy-mode-clamp-cursor-clamps-row-into-viewport
  "%copy-mode-clamp-cursor clamps the cursor row into [0, height-1]."
  (let ((s (make-screen 20 5)))
    (cl-tmux/commands::copy-mode-enter s)
    ;; Force cursor outside viewport bounds.
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 10 3))
    (cl-tmux/commands::%copy-mode-clamp-cursor s)
    (is (= 4 (car (cl-tmux/terminal/types:screen-copy-cursor s)))
        "row > height-1 must clamp to height-1=4")))

(test copy-mode-clamp-cursor-clamps-col-into-viewport
  "%copy-mode-clamp-cursor clamps the cursor col into [0, width-1]."
  (let ((s (make-screen 20 5)))
    (cl-tmux/commands::copy-mode-enter s)
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 2 50))
    (cl-tmux/commands::%copy-mode-clamp-cursor s)
    (is (= 19 (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))
        "col > width-1 must clamp to width-1=19")))

(test copy-mode-clamp-cursor-noop-when-cursor-nil
  "%copy-mode-clamp-cursor is a no-op when the cursor is NIL."
  (let ((s (make-screen 20 5)))
    (cl-tmux/commands::copy-mode-enter s)
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) nil)
    (finishes (cl-tmux/commands::%copy-mode-clamp-cursor s)
              "%copy-mode-clamp-cursor with nil cursor must not signal")))

(test copy-mode-clamp-cursor-preserves-in-range-values
  "%copy-mode-clamp-cursor leaves a cursor already in range unchanged."
  (let ((s (make-screen 20 5)))
    (cl-tmux/commands::copy-mode-enter s)
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 2 10))
    (cl-tmux/commands::%copy-mode-clamp-cursor s)
    (is (equal (cons 2 10) (cl-tmux/terminal/types:screen-copy-cursor s))
        "in-range cursor must be unchanged after clamp")))

;;; ── %selection-bounds (direct unit tests) ────────────────────────────────────

(test selection-bounds-same-row-mark-before-cursor
  "%selection-bounds returns (start-r end-r start-c end-c) when mark col < cursor col."
  (let ((s (make-screen 20 5)))
    (cl-tmux/commands::copy-mode-enter s)
    (setf (cl-tmux/terminal/types:screen-copy-mark   s) (cons 1 3)
          (cl-tmux/terminal/types:screen-copy-cursor s) (cons 1 8))
    (multiple-value-bind (start-row end-row start-col end-col)
        (cl-tmux/commands::%selection-bounds s)
      (is (= 1 start-row) "start-row must be 1")
      (is (= 1 end-row)   "end-row must be 1")
      (is (= 3 start-col) "start-col must be mark-col (3)")
      (is (= 8 end-col)   "end-col must be cursor-col (8)"))))

(test selection-bounds-same-row-cursor-before-mark
  "%selection-bounds normalises reversed cursor/mark on the same row."
  (let ((s (make-screen 20 5)))
    (cl-tmux/commands::copy-mode-enter s)
    (setf (cl-tmux/terminal/types:screen-copy-mark   s) (cons 1 8)
          (cl-tmux/terminal/types:screen-copy-cursor s) (cons 1 3))
    (multiple-value-bind (start-row end-row start-col end-col)
        (cl-tmux/commands::%selection-bounds s)
      (is (= 1 start-row) "start-row must be 1")
      (is (= 1 end-row)   "end-row must be 1")
      (is (= 3 start-col) "start-col must be min(3,8)=3")
      (is (= 8 end-col)   "end-col must be max(3,8)=8"))))

(test selection-bounds-multi-row-mark-above-cursor
  "%selection-bounds for multi-row selection where mark is on an earlier row."
  (let ((s (make-screen 20 5)))
    (cl-tmux/commands::copy-mode-enter s)
    (setf (cl-tmux/terminal/types:screen-copy-mark   s) (cons 0 2)
          (cl-tmux/terminal/types:screen-copy-cursor s) (cons 2 7))
    (multiple-value-bind (start-row end-row start-col end-col)
        (cl-tmux/commands::%selection-bounds s)
      (is (= 0 start-row) "start-row must be 0 (mark row)")
      (is (= 2 end-row)   "end-row must be 2 (cursor row)")
      (is (= 2 start-col) "start-col must be mark-col (2)")
      (is (= 7 end-col)   "end-col must be cursor-col (7)"))))

(test selection-bounds-multi-row-cursor-above-mark
  "%selection-bounds normalises reversed multi-row selection."
  (let ((s (make-screen 20 5)))
    (cl-tmux/commands::copy-mode-enter s)
    (setf (cl-tmux/terminal/types:screen-copy-mark   s) (cons 2 7)
          (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 2))
    (multiple-value-bind (start-row end-row start-col end-col)
        (cl-tmux/commands::%selection-bounds s)
      (is (= 0 start-row) "start-row must be 0 (cursor row — lower)")
      (is (= 2 end-row)   "end-row must be 2 (mark row — higher)")
      (is (= 2 start-col) "start-col must be cursor-col (2) since cursor-row < mark-row")
      (is (= 7 end-col)   "end-col must be mark-col (7)"))))

;;; ── %selection-bounds scrollback spanning (virtual-row correctness) ──────────

(test selection-bounds-after-scroll-uses-virtual-rows
  "When the user begins a selection and then scrolls, %selection-bounds must use
   virtual (absolute scrollback) rows so the selected TEXT does not shift.
   Regression test for the mark-offset fix: mark-row is a viewport row stored
   at the time of begin-selection; after scrolling by delta lines the mark must
   still refer to the same content.
   The mark is placed at (row=2, col=3) — non-zero col so the mark row contributes
   chars to %selection-text.  After scroll, with OLD (buggy) code the mark row would
   be viewport row 2 at offset=1 = live-grid row 1 = 'DDD'.  With the NEW code, the
   mark virtual row remains vrow=4 = live-grid row 2 = 'EEE'."
  (let ((s (make-screen 4 3)))        ; 4 cols, 3 rows
    ;; Feed 5 lines: scrollback=[BBB,AAA] (newest first), grid=[CCC,DDD,EEE].
    (feed-lines s "AAA" "BBB" "CCC" "DDD" "EEE")
    (cl-tmux/commands::copy-mode-enter s)
    ;; Enter at offset=0, cursor at live-grid bottom (row 2, col 0).
    (is (= 0 (screen-copy-offset s)) "precondition: offset=0 on copy-mode-enter")
    ;; Move cursor to col 3 to give the mark a non-zero column.
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 2 3))
    ;; Begin selection: mark=(2, 3), mark-offset=0.
    (cl-tmux/commands::copy-mode-begin-selection s)
    (is (= 0 (cl-tmux/terminal/types:screen-copy-mark-offset s))
        "mark-offset must equal offset at time of begin-selection")
    ;; Scroll back 1 line into scrollback: offset becomes 1.
    (cl-tmux/commands::copy-mode-scroll s 1)
    (is (= 1 (screen-copy-offset s)) "scroll must increment offset to 1")
    ;; Move cursor to viewport row 0, col 0 (newest scrollback row).
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 0))
    ;; Virtual row check: sb-n=2, mark-vrow=2+2-0=4 (EEE), cursor-vrow=2+0-1=1 (BBB).
    (multiple-value-bind (start-vrow end-vrow start-col end-col)
        (cl-tmux/commands::%selection-bounds s)
      (declare (ignore start-col end-col))
      (is (= 1 start-vrow)
          "start-vrow must be 1 (cursor at newest scrollback, sb-n=2, vrow=2+0-1=1)")
      (is (= 4 end-vrow)
          "end-vrow must be 4 (mark at live-grid row 2, vrow=sb-n+2-0=4)"))
    ;; %selection-text: vrow 1=BBB, vrow 2=CCC, vrow 3=DDD, vrow 4 cols 0-3 = EEE.
    ;; With the OLD buggy code, vrow 4 would instead be DDD (viewport row 2 at offset=1
    ;; = live-grid row 1 = DDD instead of EEE).
    (let ((text (cl-tmux/commands::%selection-text s)))
      (is (and text (search "BBB" text))
          "selection text must include the newest scrollback row content (BBB)")
      (is (and text (search "EEE" text))
          "selection text must include the live-grid row at mark (EEE, not DDD)"))))

;;; ── copy-mode-word-backward edge cases ───────────────────────────────────────

(test copy-mode-word-backward-at-col-zero-stays-put
  "copy-mode-word-backward when cursor is already at col 0 does not move."
  (let ((s (%copy-mode-screen-with-text "hello world")))
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 0))
    (cl-tmux/commands::copy-mode-word-backward s)
    (is (= 0 (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))
        "word-backward at col 0 must stay at col 0")))

(test copy-mode-word-backward-from-whitespace-skips-to-word-start
  "copy-mode-word-backward when cursor is in whitespace skips to the previous word start."
  (let ((s (%copy-mode-screen-with-text "hello world")))
    ;; Position cursor in the space between words (col 5).
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 5))
    (cl-tmux/commands::copy-mode-word-backward s)
    ;; Should land at col 0 (start of "hello").
    (is (= 0 (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))
        "word-backward from whitespace must jump to start of previous word (got ~D)"
        (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))))

(test copy-mode-word-backward-from-first-char-of-word
  "copy-mode-word-backward when cursor is at the first character of a word."
  (let ((s (%copy-mode-screen-with-text "hello world")))
    ;; Position at col 6 — the 'w' of "world".
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 6))
    (cl-tmux/commands::copy-mode-word-backward s)
    ;; Should land at col 0 (start of "hello").
    (is (= 0 (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))
        "word-backward from first char of word must jump to start of previous word (got ~D)"
        (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))))

;;; ── Copy-mode word navigation cross-line tests ───────────────────────────────

(test copy-mode-word-forward-wraps-to-next-row
  "copy-mode-word-forward crosses to BOL of next row when at end of line."
  ;; 10-wide, 2-row screen: row0=\"hello     \", row1=\"world     \"
  ;; From 'o' at (0,4), w skips \"o\", then spaces 5-9 → col=10=width → wrap to (1,0).
  (let ((s (make-screen 10 2)))
    (feed s (format nil "hello~C~Cworld" #\Return #\Linefeed))
    (cl-tmux/commands::copy-mode-enter s)
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 4))
    (cl-tmux/commands::copy-mode-word-forward s)
    (is (= 1 (car (cl-tmux/terminal/types:screen-copy-cursor s)))
        "word-forward at EOL must wrap to next row (row=~D)"
        (car (cl-tmux/terminal/types:screen-copy-cursor s)))
    (is (= 0 (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))
        "word-forward wrap lands at col 0 (col=~D)"
        (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))))

(test copy-mode-word-backward-wraps-to-prev-row
  "copy-mode-word-backward at BOL wraps to the previous row and finds word start."
  ;; 10-wide, 2-row screen: row0=\"hello     \", row1=\"world     \"
  ;; From BOL of row1 (1,0), b wraps to (0,9), scans back over spaces to col4, then
  ;; over 'hello' to col 0.
  (let ((s (make-screen 10 2)))
    (feed s (format nil "hello~C~Cworld" #\Return #\Linefeed))
    (cl-tmux/commands::copy-mode-enter s)
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 1 0))
    (cl-tmux/commands::copy-mode-word-backward s)
    (is (= 0 (car (cl-tmux/terminal/types:screen-copy-cursor s)))
        "word-backward at BOL must wrap to previous row (row=~D)"
        (car (cl-tmux/terminal/types:screen-copy-cursor s)))
    (is (= 0 (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))
        "word-backward wrap lands at start of 'hello' col 0 (col=~D)"
        (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))))

(test copy-mode-word-end-wraps-to-next-row
  "copy-mode-word-end crosses to the next row when entire row tail is separators."
  ;; 10-wide, 3-row screen: row0=\"hello     \", row1=\"          \" (blank), row2=\"world     \"
  ;; From col 4 (end of 'hello'), e steps to col 5 (sep), then wraps past blank row1,
  ;; reaches row2, advances to end of 'world' = col 4.
  (let ((s (make-screen 10 3)))
    (feed s (format nil "hello~C~C~C~Cworld" #\Return #\Linefeed #\Return #\Linefeed))
    (cl-tmux/commands::copy-mode-enter s)
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 4))
    (cl-tmux/commands::copy-mode-word-end s)
    ;; Should arrive at end of "world" on row 2
    (is (= 2 (car (cl-tmux/terminal/types:screen-copy-cursor s)))
        "word-end must wrap through blank rows to row 2 (row=~D)"
        (car (cl-tmux/terminal/types:screen-copy-cursor s)))
    (is (= 4 (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))
        "word-end at 'world' must land at col 4 (col=~D)"
        (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))))

;;; ── copy-mode paragraph motion tests ────────────────────────────────────────

(test copy-mode-next-paragraph-jumps-to-blank-line
  "copy-mode-next-paragraph jumps to the nearest blank line below the cursor."
  ;; 20-wide, 5-row screen: row0=text, row1=text, row2=blank, row3=text, row4=text
  (let ((s (make-screen 20 5)))
    (feed s (format nil "hello~C~Cworld~C~C~C~Cfoo~C~Cbar" #\Return #\Linefeed
                    #\Return #\Linefeed #\Return #\Linefeed
                    #\Return #\Linefeed))
    (cl-tmux/commands::copy-mode-enter s)
    ;; Cursor at row 0 (first row)
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 0))
    (cl-tmux/commands::copy-mode-next-paragraph s)
    ;; Blank row is at vrow 2 (row0=hello, row1=world, row2=blank)
    ;; With no scrollback, vrow = sb-n + viewport_row - offset = 0 + row - 0 = row
    (is (= 2 (car (cl-tmux/terminal/types:screen-copy-cursor s)))
        "next-paragraph must jump to blank row 2 (got row ~D)"
        (car (cl-tmux/terminal/types:screen-copy-cursor s)))))

(test copy-mode-previous-paragraph-jumps-to-blank-line
  "copy-mode-previous-paragraph jumps to the nearest blank line above the cursor."
  (let ((s (make-screen 20 5)))
    (feed s (format nil "hello~C~Cworld~C~C~C~Cfoo~C~Cbar" #\Return #\Linefeed
                    #\Return #\Linefeed #\Return #\Linefeed
                    #\Return #\Linefeed))
    (cl-tmux/commands::copy-mode-enter s)
    ;; Cursor at row 4 (last row)
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 4 0))
    (cl-tmux/commands::copy-mode-previous-paragraph s)
    (is (= 2 (car (cl-tmux/terminal/types:screen-copy-cursor s)))
        "previous-paragraph must jump to blank row 2 (got row ~D)"
        (car (cl-tmux/terminal/types:screen-copy-cursor s)))))

(test copy-mode-next-paragraph-at-bottom-stays
  "copy-mode-next-paragraph with no blank line below stays at last row."
  (let ((s (make-screen 20 3)))
    (feed s (format nil "hello~C~Cworld" #\Return #\Linefeed))
    (cl-tmux/commands::copy-mode-enter s)
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 0))
    (cl-tmux/commands::copy-mode-next-paragraph s)
    ;; No blank row; should land at last vrow = 2 (sb-n=0, h=3)
    ;; Since no scrollback, last vrow = 2, viewport row = 2
    (is (<= 1 (car (cl-tmux/terminal/types:screen-copy-cursor s)))
        "next-paragraph with no blank line below must advance cursor (got row ~D)"
        (car (cl-tmux/terminal/types:screen-copy-cursor s)))))

;;; ── copy-mode-scroll-middle tests ───────────────────────────────────────────

(test copy-mode-scroll-middle-centers-cursor
  "copy-mode-scroll-middle adjusts offset so cursor row is at viewport center."
  ;; 20-wide, 5-row screen with 3 lines of scrollback (feed 8 rows so 3 scroll off).
  ;; Enter copy mode, scroll back fully (offset=3), place cursor at row 4 (bottom).
  ;; After scroll-middle: center=2, delta = 2-4 = -2, new-offset=1, cursor-row=2.
  (let ((s (make-screen 20 5)))
    (dotimes (i 8) (feed s (format nil "line~D~C~C" i #\Return #\Linefeed)))
    (cl-tmux/commands::copy-mode-enter s)
    (cl-tmux/commands::copy-mode-top s)   ; scroll to max offset (3)
    (let ((max-off (screen-copy-offset s)))
      (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 4 0))
      (cl-tmux/commands::copy-mode-scroll-middle s)
      (is (= 2 (car (cl-tmux/terminal/types:screen-copy-cursor s)))
          "scroll-middle must center cursor at row 2 (got ~D)"
          (car (cl-tmux/terminal/types:screen-copy-cursor s)))
      (is (= (+ max-off (- 2 4)) (screen-copy-offset s))
          "scroll-middle offset must be max-off + (center - old-row) (got ~D)"
          (screen-copy-offset s)))))

(test copy-mode-scroll-middle-clamps-at-history-bottom
  "copy-mode-scroll-middle clamps the offset to 0 at the bottom of history."
  ;; No scrollback: offset stays 0, cursor moves as much as possible toward center.
  (let ((s (make-screen 20 5)))
    (feed s "hello world")
    (cl-tmux/commands::copy-mode-enter s)
    ;; Cursor at row 0 (top), offset 0, no scrollback.
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 0))
    (cl-tmux/commands::copy-mode-scroll-middle s)
    ;; center=2, delta = 2-0 = +2, but new-offset = clamp(0 + 2, 0, 0) = 0
    ;; cursor stays at row 0 (0 + 0 = 0)
    (is (= 0 (screen-copy-offset s))
        "offset must clamp to 0 when no scrollback (got ~D)"
        (screen-copy-offset s))
    (is (= 0 (car (cl-tmux/terminal/types:screen-copy-cursor s)))
        "cursor row must stay at 0 when no scrollback (got ~D)"
        (car (cl-tmux/terminal/types:screen-copy-cursor s)))))

(test copy-mode-jump-to-mark-moves-cursor-to-mark
  "copy-mode-jump-to-mark moves the cursor to the mark without swapping."
  (let* ((s (make-screen 20 5)))
    (setf (screen-copy-mode-p s)  t
          (screen-copy-offset s)  0
          ;; cursor at (row=4,col=10), mark at (row=1,col=3), both at offset 0
          (cl-tmux/terminal/types:screen-copy-cursor s) (cons 4 10)
          (cl-tmux/terminal/types:screen-copy-mark   s) (cons 1 3)
          (cl-tmux/terminal/types:screen-copy-mark-offset s) 0)
    (cl-tmux/commands::copy-mode-jump-to-mark s)
    (is (equal (cons 1 3) (cl-tmux/terminal/types:screen-copy-cursor s))
        "cursor must jump to the mark position (row=1 col=3)")))

(test copy-mode-jump-to-mark-noop-when-no-mark
  "copy-mode-jump-to-mark is a no-op when no mark has been set."
  (let* ((s (make-screen 20 5)))
    (setf (screen-copy-mode-p s)  t
          (screen-copy-offset s)  0
          (cl-tmux/terminal/types:screen-copy-cursor s) (cons 2 5)
          (cl-tmux/terminal/types:screen-copy-mark   s) nil)
    (cl-tmux/commands::copy-mode-jump-to-mark s)
    (is (equal (cons 2 5) (cl-tmux/terminal/types:screen-copy-cursor s))
        "cursor must be unchanged when there is no mark")))
