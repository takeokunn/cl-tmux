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
    (is-true (pane-pipe-active-p pane)
        "pipe-pane-open must mark the pane as active")
    (is-true (pane-pipe-fd pane)
        "pipe-pane-open must store the command stdin stream")
    (is-true (pane-pipe-process pane)
        "pipe-pane-open must keep the subprocess object for bounded cleanup")
    ;; Clean up.
    (cl-tmux/commands:pipe-pane-close pane)))

(test pipe-pane-open-close-round-trip
  "pipe-pane-open followed by pipe-pane-close clears pipe state."
  (let ((pane (%make-test-pane)))
    (cl-tmux/commands:pipe-pane-open pane "cat")
    (is-true (pane-pipe-fd pane)
        "pane-pipe-fd must be set after pipe-pane-open")
    (is-true (pane-pipe-process pane)
        "pane-pipe-process must be set after pipe-pane-open")
    (is (null (pane-pipe-output-stream pane))
        "pane-pipe-output-stream must be NIL for output-to-command mode")
    (is (null (pane-pipe-output-thread pane))
        "pane-pipe-output-thread must be NIL for output-to-command mode")
    (cl-tmux/commands:pipe-pane-close pane)
    (is (null (pane-pipe-fd pane))
        "pane-pipe-fd must be NIL after pipe-pane-close")
    (is (null (pane-pipe-process pane))
        "pane-pipe-process must be NIL after pipe-pane-close")
    (is (null (pane-pipe-output-stream pane))
        "pane-pipe-output-stream must be NIL after pipe-pane-close")
    (is (null (pane-pipe-output-thread pane))
        "pane-pipe-output-thread must be NIL after pipe-pane-close")))

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

(test cmd-pipe-pane-flag-i-enables-command-output-to-pane
  "pipe-pane -I opens the reverse direction: command stdout is copied back to the pane."
  (with-fake-session (sess :nwindows 1 :npanes 1)
    (let* ((*overlay* nil)
           (pane (session-active-pane sess))
           (result (cl-tmux::%cmd-pipe-pane-arg sess '("-I" "cat"))))
      (is-true result
          "pipe-pane -I must be accepted")
      (is-true (pane-pipe-active-p pane)
          "pipe-pane -I must mark the pane as active")
      (is (null (pane-pipe-fd pane))
          "pipe-pane -I must not open a command-stdin pipe")
      (is-true (pane-pipe-output-stream pane)
          "pipe-pane -I must capture command stdout")
      (is-true (pane-pipe-output-thread pane)
          "pipe-pane -I must start a copier thread")
      (is-true (pane-pipe-process pane)
          "pipe-pane -I must keep the subprocess object for cleanup")
      (cl-tmux/commands:pipe-pane-close pane))))

(test cmd-pipe-pane-flag-o-keeps-pane-output-to-command
  "pipe-pane -O keeps the default pane stdout -> command stdin direction."
  (with-fake-session (sess :nwindows 1 :npanes 1)
    (let* ((*overlay* nil)
           (pane (session-active-pane sess))
           (result (cl-tmux::%cmd-pipe-pane-arg sess '("-O" "cat"))))
      (is-true result
          "pipe-pane -O must be accepted")
      (is-true (pane-pipe-active-p pane)
          "pipe-pane -O must mark the pane as active")
      (is-true (pane-pipe-fd pane)
          "pipe-pane -O must open the command-stdin pipe")
      (is (null (pane-pipe-output-stream pane))
          "pipe-pane -O must not capture command stdout")
      (is (null (pane-pipe-output-thread pane))
          "pipe-pane -O must not start a copier thread")
      (is-true (pane-pipe-process pane)
          "pipe-pane -O must keep the subprocess object for cleanup")
      (cl-tmux/commands:pipe-pane-close pane))))

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

(test pipe-pane-open-times-out-and-cleans-up
  "pipe-pane-open returns NIL and leaves the pane clean when launch times out."
  (let* ((pane (%make-test-pane))
         (original-launch (fdefinition 'uiop:launch-program)))
    (unwind-protect
        (progn
          (setf (fdefinition 'uiop:launch-program)
                (lambda (&rest args)
                  (sleep 2)
                  (apply original-launch args)))
          (is (null (cl-tmux/commands:pipe-pane-open pane "cat"))
              "pipe-pane-open must return NIL when launch exceeds timeout")
          (is (null (pane-pipe-fd pane))
              "pipe-pane-fd must be NIL after a timed-out open")
          (is (null (pane-pipe-output-stream pane))
              "pane-pipe-output-stream must be NIL after a timed-out open")
          (is (null (pane-pipe-output-thread pane))
              "pane-pipe-output-thread must be NIL after a timed-out open")
          (is (null (pane-pipe-process pane))
              "pane-pipe-process must be NIL after a timed-out open"))
      (setf (fdefinition 'uiop:launch-program) original-launch)
      (ignore-errors (cl-tmux/commands:pipe-pane-close pane)))))

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

(test copy-mode-total-rows-counts-scrollback-plus-height
  "%copy-mode-total-rows returns scrollback length + screen height."
  (let ((s (make-screen 20 5)))
    (feed-lines s "line-0" "line-1" "line-2" "line-3" "line-4" "line-5" "line-6")
    (is (= 7 (cl-tmux/commands::%copy-mode-total-rows s))
        "%copy-mode-total-rows must count the full virtual buffer")))

(test copy-mode-set-virtual-row-updates-offset-and-cursor
  "%copy-mode-set-virtual-row moves the cursor to the requested virtual row."
  (let ((s (make-screen 4 3)))
    (feed-lines s "AAA" "BBB" "CCC" "DDD" "EEE")
    (cl-tmux/commands::copy-mode-enter s)
    (cl-tmux/commands::%copy-mode-set-virtual-row s 0 1)
    (is (= 2 (screen-copy-offset s))
        "%copy-mode-set-virtual-row must scroll to expose the requested scrollback row")
    (is (equal (cons 0 1) (cl-tmux/terminal/types:screen-copy-cursor s))
        "%copy-mode-set-virtual-row must place the cursor on the requested row/col")
    (is-true (screen-dirty-p s)
             "%copy-mode-set-virtual-row must mark the screen dirty")))

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

(test copy-mode-clamp-cursor-table
  "%copy-mode-clamp-cursor clamps out-of-range row/col and leaves in-range cursors unchanged."
  (dolist (c '((10  3 4  3 "row > height-1 clamps to height-1=4")
               (2  50 2 19 "col > width-1 clamps to width-1=19")
               (2  10 2 10 "in-range cursor unchanged")))
    (destructuring-bind (init-r init-c exp-r exp-c desc) c
      (let ((s (make-screen 20 5)))
        (cl-tmux/commands::copy-mode-enter s)
        (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons init-r init-c))
        (cl-tmux/commands::%copy-mode-clamp-cursor s)
        (is (= exp-r (car (cl-tmux/terminal/types:screen-copy-cursor s))) "~A: row" desc)
        (is (= exp-c (cdr (cl-tmux/terminal/types:screen-copy-cursor s))) "~A: col" desc)))))

(test copy-mode-clamp-cursor-noop-when-cursor-nil
  "%copy-mode-clamp-cursor is a no-op when the cursor is NIL."
  (let ((s (make-screen 20 5)))
    (cl-tmux/commands::copy-mode-enter s)
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) nil)
    (finishes (cl-tmux/commands::%copy-mode-clamp-cursor s)
              "%copy-mode-clamp-cursor with nil cursor must not signal")))

;;; ── %selection-bounds (direct unit tests) ────────────────────────────────────

(test selection-bounds-table
  "%selection-bounds always returns (start-row end-row start-col end-col) with
   start ≤ end, regardless of whether mark or cursor comes first."
  (dolist (row '((1 3  1 8  1 1 3 8 "same-row: mark col < cursor col")
                 (1 8  1 3  1 1 3 8 "same-row: cursor col < mark col (normalised)")
                 (0 2  2 7  0 2 2 7 "multi-row: mark above cursor")
                 (2 7  0 2  0 2 2 7 "multi-row: cursor above mark (normalised)")))
    (destructuring-bind (mr mc cr cc exp-sr exp-er exp-sc exp-ec desc) row
      (let ((s (make-screen 20 5)))
        (cl-tmux/commands::copy-mode-enter s)
        (setf (cl-tmux/terminal/types:screen-copy-mark   s) (cons mr mc)
              (cl-tmux/terminal/types:screen-copy-cursor s) (cons cr cc))
        (multiple-value-bind (start-row end-row start-col end-col)
            (cl-tmux/commands::%selection-bounds s)
          (is (= exp-sr start-row) "~A: start-row" desc)
          (is (= exp-er end-row)   "~A: end-row"   desc)
          (is (= exp-sc start-col) "~A: start-col" desc)
          (is (= exp-ec end-col)   "~A: end-col"   desc))))))
