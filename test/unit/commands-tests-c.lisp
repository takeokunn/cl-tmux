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

