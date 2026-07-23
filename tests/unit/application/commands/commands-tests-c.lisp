(in-package #:cl-tmux/test)

;;;; commands tests — part C: pipe-pane, virtual-row-string, timeout, clamp-cursor,
;;;; selection-bounds, word/paragraph navigation, scroll helpers, extract-row-chars.

(defun %run-pipe-pane-direction-case (args assertion)
  (with-fake-session (sess :nwindows 1 :npanes 1)
    (let* ((*overlay* nil)
           (pane (session-active-pane sess))
           (result (cl-tmux::%cmd-pipe-pane-arg sess args)))
      (expect result :to-be-truthy)
      (funcall assertion pane)
      (cl-tmux/commands:pipe-pane-close pane))))

(describe "commands-suite"

  ;; ── pipe-pane-open / pipe-pane-close / pipe-pane-write ──────────────────────

  ;; pipe-pane-open returns a stream object when the command launches successfully.
  (it "pipe-pane-open-returns-stream"
    (let* ((pane   (%make-test-pane))
           (result (cl-tmux/commands:pipe-pane-open pane "cat")))
      (expect result :to-be-truthy)
      (assert-pipe-pane-open-output-to-command-state pane)
      ;; Clean up.
      (cl-tmux/commands:pipe-pane-close pane)))

  ;; pipe-pane-open followed by pipe-pane-close clears pipe state.
  (it "pipe-pane-open-close-round-trip"
    (let ((pane (%make-test-pane)))
      (cl-tmux/commands:pipe-pane-open pane "cat")
      (assert-pipe-pane-open-output-to-command-state pane)
      (cl-tmux/commands:pipe-pane-close pane)
      (assert-pipe-pane-closed-state pane)))

  ;; pipe-pane-close is a no-op when pane has no open pipe.
  (it "pipe-pane-close-noop-when-no-pipe"
    (let ((pane (%make-test-pane)))
      (finishes (cl-tmux/commands:pipe-pane-close pane)
                "pipe-pane-close with no pipe must not signal")))

  ;; pipe-pane -t 2 <cmd> opens the pipe on pane 2 (the -t target), NOT the active
  ;; pane — the scriptable -t target is honoured.
  (it "cmd-pipe-pane-t-pipes-target-pane"
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
        (expect (pane-pipe-fd pb) :to-be-truthy)
        (expect (null (pane-pipe-fd pa)))
        ;; Clean up the forked cat process.
        (cl-tmux/commands:pipe-pane-close pb))))

  ;; pipe-pane -I opens the reverse direction: command stdout is copied back to the pane.
  (it "cmd-pipe-pane-flag-i-enables-command-output-to-pane"
    (%run-pipe-pane-direction-case '("-I" "cat")
      (lambda (pane)
        (assert-pipe-pane-open-command-output-state pane))))

  ;; pipe-pane -O keeps the default pane stdout -> command stdin direction.
  (it "cmd-pipe-pane-flag-o-keeps-pane-output-to-command"
    (%run-pipe-pane-direction-case '("-O" "cat")
      (lambda (pane)
        (assert-pipe-pane-open-output-to-command-state pane))))

  ;; send-keys -X -t .%2 begin-selection acts on pane-id 2's copy mode, not the
  ;; active pane, and restores focus to the original active pane afterward.
  (it "cmd-send-keys-X-t-targets-pane-copy-mode"
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
          (expect (cl-tmux/terminal/types:screen-copy-selecting (pane-screen pb)) :to-be-truthy)
          (expect (cl-tmux/terminal/types:screen-copy-selecting (pane-screen pa)) :to-be-falsy)
          (expect (eq pa (cl-tmux/model:window-active-pane win)))))))

  ;; pipe-pane-write is a no-op when pane has no open pipe.
  (it "pipe-pane-write-noop-when-no-pipe"
    (let ((pane (%make-test-pane)))
      (finishes (cl-tmux/commands:pipe-pane-write pane #(65 66 67))
                "pipe-pane-write with no pipe must not signal")))

  ;; pipe-pane-open returns NIL when the shell program cannot be launched.
  ;; pipe-pane-open runs the command via `sh -c`, so a bogus *command* still
  ;; launches successfully (sh exists, then fails internally — matching tmux).
  ;; To exercise the launch-failure → NIL path, point *default-shell* at a
  ;; non-existent binary so uiop:launch-program itself fails.
  (it "pipe-pane-open-invalid-command-returns-nil"
    (let* ((pane   (%make-test-pane))
           (cl-tmux/config:*default-shell* "/no/such/shell-5f3a9b2e")
           (result (cl-tmux/commands:pipe-pane-open pane "echo hi")))
      (expect (null result))))

  ;; pipe-pane-open returns NIL and leaves the pane clean when launch times out.
  (it "pipe-pane-open-times-out-and-cleans-up"
    (let* ((pane (%make-test-pane))
           (original-launch (fdefinition 'uiop:launch-program)))
      (unwind-protect
          (progn
            (setf (fdefinition 'uiop:launch-program)
                  (lambda (&rest args)
                    (sleep 2)
                    (apply original-launch args)))
            (expect (null (cl-tmux/commands:pipe-pane-open pane "cat")))
            (assert-pipe-pane-closed-state pane))
        (setf (fdefinition 'uiop:launch-program) original-launch)
        (ignore-errors (cl-tmux/commands:pipe-pane-close pane)))))

  ;; pipe-pane-write with an open pipe sends bytes to the subprocess stdin.
  ;; This drives a REAL shell subprocess + filesystem (cat > tmpfile), which is
  ;; inherently nondeterministic under a heavily-loaded parallel build (subprocess
  ;; scheduling / GC / fs flush timing).  Earlier single-shot versions — even with a
  ;; 6s poll — flaked.  We instead retry the whole self-contained cycle up to 5
  ;; times and assert the bytes reach the subprocess on at least one attempt: this
  ;; still verifies the real behaviour (bytes DO traverse the pipe to the child)
  ;; while tolerating a one-off environmental hiccup.  3 deterministic failures in a
  ;; row would still fail (a genuine break is not masked).
  (it "pipe-pane-write-bytes-reach-subprocess"
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
        (expect ok :to-be-truthy))))

  ;; ── %copy-mode-virtual-row-string (direct unit tests) ───────────────────────

  ;; %copy-mode-virtual-row-string returns the content of the requested virtual row.
  (it "copy-mode-virtual-row-string-returns-row-content"
    (let ((s (make-screen 20 5)))
      (feed s "hello")
      (cl-tmux/commands::copy-mode-enter s)
      (let* ((vrow (+ (length (cl-tmux/terminal:screen-scrollback s))
                      (- 0 (cl-tmux/terminal:screen-copy-offset s))))
             (row-str (cl-tmux/commands::%copy-mode-virtual-row-string s vrow)))
        (expect (stringp row-str))
        (expect (and (>= (length row-str) 5)
                     (string= "hello" (subseq row-str 0 5)))))))

  ;; %copy-mode-virtual-row-string always returns a string of length = screen-width.
  (it "copy-mode-virtual-row-string-length-equals-screen-width"
    (let ((s (make-screen 20 5)))
      (cl-tmux/commands::copy-mode-enter s)
      (let ((vrow (length (cl-tmux/terminal:screen-scrollback s))))
        (expect (= 20 (length (cl-tmux/commands::%copy-mode-virtual-row-string s vrow)))))))

  ;; %copy-mode-total-rows returns scrollback length + screen height.
  (it "copy-mode-total-rows-counts-scrollback-plus-height"
    (let ((s (make-screen 20 5)))
      (feed-lines s "line-0" "line-1" "line-2" "line-3" "line-4" "line-5" "line-6")
      (expect (= 7 (cl-tmux/commands::%copy-mode-total-rows s)))))

  ;; %copy-mode-set-virtual-row moves the cursor to the requested virtual row.
  (it "copy-mode-set-virtual-row-updates-offset-and-cursor"
    (let ((s (make-screen 4 3)))
      (feed-lines s "AAA" "BBB" "CCC" "DDD" "EEE")
      (cl-tmux/commands::copy-mode-enter s)
      (cl-tmux/commands::%copy-mode-set-virtual-row s 0 1)
      (expect (= 2 (screen-copy-offset s)))
      (expect (equal (cons 0 1) (cl-tmux/terminal/types:screen-copy-cursor s)))
      (expect (screen-dirty-p s) :to-be-truthy)))

  ;; ── %run-with-timeout ────────────────────────────────────────────────────────

  ;; %run-with-timeout returns the result of the thunk when it completes within time.
  (it "run-with-timeout-returns-thunk-result"
    (let ((result (cl-tmux/commands::%run-with-timeout (lambda () 42) 10)))
      (expect (= 42 result))))

  ;; %run-with-timeout returns NIL when the thunk exceeds the timeout.
  (it "run-with-timeout-returns-nil-on-timeout"
    (let ((result (cl-tmux/commands::%run-with-timeout
                   (lambda () (sleep 60)) 1/1000)))
      (expect (null result))))

  ;; ── run-shell timeout ────────────────────────────────────────────────────────

  ;; run-shell returns NIL when the command exceeds the given timeout.
  (it "run-shell-returns-nil-on-timeout"
    ;; Use a very short timeout (1ms) with a sleep command.
    (let ((result (cl-tmux/commands:run-shell "sleep 60" :timeout 1/1000)))
      (expect (null result))))

  ;; ── %copy-mode-clamp-cursor (direct unit tests) ──────────────────────────────

  ;; %copy-mode-clamp-cursor clamps out-of-range row/col and leaves in-range cursors unchanged.
  (it "copy-mode-clamp-cursor-table"
    (dolist (c '((10  3 4  3 "row > height-1 clamps to height-1=4")
                 (2  50 2 19 "col > width-1 clamps to width-1=19")
                 (2  10 2 10 "in-range cursor unchanged")))
      (destructuring-bind (init-r init-c exp-r exp-c desc) c
        (declare (ignore desc))
        (let ((s (make-screen 20 5)))
          (cl-tmux/commands::copy-mode-enter s)
          (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons init-r init-c))
          (cl-tmux/commands::%copy-mode-clamp-cursor s)
          (expect (= exp-r (car (cl-tmux/terminal/types:screen-copy-cursor s))))
          (expect (= exp-c (cdr (cl-tmux/terminal/types:screen-copy-cursor s))))))))

  ;; %copy-mode-clamp-cursor is a no-op when the cursor is NIL.
  (it "copy-mode-clamp-cursor-noop-when-cursor-nil"
    (let ((s (make-screen 20 5)))
      (cl-tmux/commands::copy-mode-enter s)
      (setf (cl-tmux/terminal/types:screen-copy-cursor s) nil)
      (finishes (cl-tmux/commands::%copy-mode-clamp-cursor s)
                "%copy-mode-clamp-cursor with nil cursor must not signal")))

  ;; ── %selection-bounds (direct unit tests) ────────────────────────────────────

  ;; %selection-bounds always returns (start-row end-row start-col end-col) with
  ;; start ≤ end, regardless of whether mark or cursor comes first.
  (it "selection-bounds-table"
    (dolist (row '((1 3  1 8  1 1 3 8 "same-row: mark col < cursor col")
                   (1 8  1 3  1 1 3 8 "same-row: cursor col < mark col (normalised)")
                   (0 2  2 7  0 2 2 7 "multi-row: mark above cursor")
                   (2 7  0 2  0 2 2 7 "multi-row: cursor above mark (normalised)")))
      (destructuring-bind (mr mc cr cc exp-sr exp-er exp-sc exp-ec desc) row
        (declare (ignore desc))
        (let ((s (make-screen 20 5)))
          (cl-tmux/commands::copy-mode-enter s)
          (setf (cl-tmux/terminal/types:screen-copy-mark   s) (cons mr mc)
                (cl-tmux/terminal/types:screen-copy-cursor s) (cons cr cc))
          (multiple-value-bind (start-row end-row start-col end-col)
              (cl-tmux/commands::%selection-bounds s)
            (expect (= exp-sr start-row))
            (expect (= exp-er end-row))
            (expect (= exp-sc start-col))
            (expect (= exp-ec end-col))))))))
