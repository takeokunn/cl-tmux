(in-package #:cl-tmux/test)

;;;; rename-window, kill-window, run-shell, if-shell, selection-text, swap-pane, capture-pane — part III

(in-suite commands-suite)

;;; ── rename-window ────────────────────────────────────────────────────────────

(test rename-window-sets-name
  "rename-window sets the window name to the supplied string."
  (let ((win (make-window :id 1 :name "old" :width 20 :height 5 :panes nil)))
    (cl-tmux/commands:rename-window win "new")
    (is (string= "new" (window-name win))
        "window name must be updated to \"new\"")))

(test rename-window-nil-window-is-noop
  "rename-window with NIL window does not signal an error."
  (finishes (cl-tmux/commands:rename-window nil "irrelevant")))

(test rename-window-invalid-name-is-noop-table
  "rename-window with an empty or NIL name leaves the window name unchanged."
  (dolist (c '(("" "original" "empty string → no-op")
               (nil "keep"    "nil → no-op")))
    (destructuring-bind (new-name original desc) c
      (let ((win (make-window :id 1 :name original :width 20 :height 5 :panes nil)))
        (cl-tmux/commands:rename-window win new-name)
        (is (string= original (window-name win)) "~A" desc)))))

;;; ── kill-window (direct path) ────────────────────────────────────────────────

(test kill-window-explicit-window-arg-removes-that-window
  "kill-window with an explicit WINDOW removes that specific window even when it
   is not the active one."
  (let* ((p0  (%make-test-pane :id 1))
         (p1  (%make-test-pane :id 2))
         (w1  (make-window :id 1 :name "a" :width 20 :height 5
                           :tree (make-layout-leaf p0) :panes (list p0)))
         (w2  (make-window :id 2 :name "b" :width 20 :height 5
                           :tree (make-layout-leaf p1) :panes (list p1)))
         (sess (make-session :id 1 :name "0" :windows (list w1 w2))))
    (session-select-window sess w1)          ; active = w1
    ;; Kill the non-active window w2 explicitly.
    (is (null (kill-window sess w2))
        "killing a non-active window must return NIL (session survives)")
    (is (equal (list w1) (session-windows sess))
        "only w2 must be removed from the session")
    (is (eq w1 (session-active-window sess))
        "active window must remain w1 when the killed window was not active")))

(test kill-window-last-window-returns-quit
  "Destroying the sole window of a session returns :quit."
  (let* ((p0  (%make-test-pane))
         (w1  (make-window :id 1 :name "w" :width 20 :height 5
                           :tree (make-layout-leaf p0) :panes (list p0)))
         (sess (make-session :id 1 :name "0" :windows (list w1))))
    (session-select-window sess w1)
    (is (eq :quit (kill-window sess))
        "killing the sole window must return :quit")
    (is (null (session-windows sess)) "session must have no windows")))

(test kill-window-active-switches-to-remaining
  "Killing the active window of two switches the active pointer to the survivor."
  (let* ((p0  (%make-test-pane :id 1))
         (p1  (%make-test-pane :id 2))
         (w1  (make-window :id 1 :name "a" :width 20 :height 5
                           :tree (make-layout-leaf p0) :panes (list p0)))
         (w2  (make-window :id 2 :name "b" :width 20 :height 5
                           :tree (make-layout-leaf p1) :panes (list p1)))
         (sess (make-session :id 1 :name "0" :windows (list w1 w2))))
    (session-select-window sess w1)
    (is (null (kill-window sess))
        "session with a remaining window must not quit")
    (is (eq w2 (session-active-window sess))
        "active window must switch to the survivor after killing the active one")))

(test kill-window-active-reselects-mru-not-nearest
  "End-to-end: killing the active window selects the last-used (MRU) survivor, not
   the numerically-nearest one (tmux session_detach / session_last).  Timestamps
   are preset (session-select-window has 1-second universal-time resolution, so
   live switches would tie); killed=1 with remaining {0,2} is an id-distance tie
   the OLD %nearest-window rule broke toward the higher id (w2)."
  (let* ((p0 (%make-test-pane :id 1))
         (p1 (%make-test-pane :id 2))
         (p2 (%make-test-pane :id 3))
         (w0 (make-window :id 0 :name "a" :width 20 :height 5
                          :tree (make-layout-leaf p0) :panes (list p0)
                          :last-active-time 200))   ; MRU survivor
         (w1 (make-window :id 1 :name "b" :width 20 :height 5
                          :tree (make-layout-leaf p1) :panes (list p1)))
         (w2 (make-window :id 2 :name "c" :width 20 :height 5
                          :tree (make-layout-leaf p2) :panes (list p2)
                          :last-active-time 100))
         (sess (make-session :id 1 :name "0" :windows (list w0 w1 w2))))
    ;; Make w1 active (its timestamp becomes 'now', irrelevant — it is killed).
    (session-select-window sess w1)
    (kill-window sess)
    (is (eq w0 (session-active-window sess))
        "MRU survivor w0 (time 200 > w2's 100) is selected, NOT nearest-tie w2")))

;;; ── run-shell ────────────────────────────────────────────────────────────────
;;;
;;; Tests use /bin/true (always exits 0) and /bin/echo (prints output) which are
;;; universally available on POSIX systems.  Background mode is verified via the
;;; T return value without inspecting the process object.

(test run-shell-foreground-captures-stdout
  "run-shell (background nil) returns a string containing the command's output."
  (let ((out (cl-tmux/commands:run-shell "echo hello")))
    (is (stringp out) "return value must be a string")
    (is (search "hello" out) "output must contain the echoed word")))

(test run-shell-background-returns-t
  "run-shell :background T returns T immediately without waiting."
  (let ((result (cl-tmux/commands:run-shell "true" :background t)))
    (is (eq t result) "background run must return T")))

(test run-shell-foreground-empty-command-returns-string
  "run-shell with a no-op command returns an empty or whitespace-only string."
  (let ((out (cl-tmux/commands:run-shell "true")))
    (is (stringp out) "return value must be a string even for a no-op command")))

(test run-shell-combine-stderr-captures-stderr
  "run-shell :combine-stderr T returns stdout and stderr in one output string."
  (let ((out (cl-tmux/commands:run-shell "printf out; printf err >&2"
                                         :combine-stderr t)))
    (is (stringp out) "return value must be a string")
    (is (search "out" out) "output must contain stdout")
    (is (search "err" out) "output must contain stderr")))

(test run-shell-start-directory-controls-working-directory
  "run-shell :start-directory runs the shell subprocess from that directory."
  (let ((dir (merge-pathnames
              (format nil "cl-tmux-run-shell-cwd-~D/" (random 1000000))
              (uiop:temporary-directory))))
    (unwind-protect
         (let* ((created (ensure-directories-exist dir))
                (expected (string-right-trim '(#\/)
                                             (namestring (truename created))))
                (out (cl-tmux/commands:run-shell
                      "pwd"
                      :start-directory (namestring created)))
                (actual (string-right-trim '(#\/ #\Newline #\Return #\Space #\Tab)
                                           out)))
           (is (stringp out) "return value must be a string")
           (is (string= expected actual)
               "pwd must report the requested start directory"))
      (ignore-errors (uiop:delete-directory-tree dir :validate t)))))

(test run-shell-delay-waits-before-running-command
  "run-shell :delay waits before launching the shell subprocess."
  (let* ((start (get-internal-real-time))
         (out (cl-tmux/commands:run-shell "printf delayed" :delay 1/20))
         (elapsed (/ (- (get-internal-real-time) start)
                     internal-time-units-per-second)))
    (is (stringp out) "return value must be a string")
    (is (search "delayed" out) "output must contain the command output")
    (is (>= elapsed 1/20) "run-shell must wait before launching the command")))

(test run-command-line-run-shell-accepts-tmux-parity-flags
  "%run-command-line run-shell accepts the tmux parity flags -c/-d/-t (their
   arguments are consumed) and runs the remaining command (tmux args bCEc:d:t:)."
  (with-fake-session (s)
    (let ((*overlay* nil))
      (cl-tmux::%run-command-line s "run-shell -d 0 -t %1 echo ok")
      (is (null (search "unsupported argument" *overlay*))
          "run-shell -d/-t must be accepted, not rejected")
      (is (search "ok" *overlay*)
          "run-shell must run the command after consuming the flag arguments")
      (is (null (search "%1" *overlay*))
          "the -t value must be consumed, not run as part of the command"))))

(test run-command-line-run-shell-t-targets-output-pane-context
  "%run-command-line run-shell -t shows shell output from the target pane context
   and restores the previously active pane."
  (with-two-pane-h-session (s win p0 p1)
    (with-command-test-state (s :overlay t)
      (let ((seen-pane nil)
            (real-show-overlay (symbol-function 'show-overlay)))
        (unwind-protect
             (progn
               (setf (symbol-function 'show-overlay)
                     (lambda (text)
                       (setf seen-pane (window-active-pane win))
                       (funcall real-show-overlay text)))
               (cl-tmux::%run-command-line
                s "run-shell -t %2 printf targeted"))
          (setf (symbol-function 'show-overlay) real-show-overlay))
        (is (search "targeted" *overlay*)
            "overlay must contain the shell command output")
        (is (eq p1 seen-pane)
            "run-shell -t must show output while the target pane is active")
        (is (eq p0 (window-active-pane win))
            "run-shell -t must restore the previously active pane")))))

(test run-command-line-run-shell-d-delays-before-running-command
  "%run-command-line run-shell -d waits before launching the shell command."
  (with-fake-session (s)
    (let ((*overlay* nil)
          (start (get-internal-real-time)))
      (cl-tmux::%run-command-line s "run-shell -d 1 echo delayed")
      (let ((elapsed (/ (- (get-internal-real-time) start)
                        internal-time-units-per-second)))
        (is (search "delayed" *overlay*)
            "overlay must contain the delayed command output")
        (is (>= elapsed 1)
            "dispatch run-shell -d must wait before launching the command")))))

(test run-command-line-run-shell-c-controls-working-directory
  "%run-command-line run-shell -c runs the shell command from the supplied directory."
  (let ((dir (merge-pathnames
              (format nil "cl-tmux-run-shell-dispatch-cwd-~D/" (random 1000000))
              (uiop:temporary-directory))))
    (unwind-protect
         (let* ((created (ensure-directories-exist dir))
                (expected (string-right-trim '(#\/)
                                             (namestring (truename created)))))
           (with-fake-session (s)
             (let ((*overlay* nil))
               (cl-tmux::%run-command-line
                s
                (format nil "run-shell -c ~A pwd" (namestring created)))
               (is (null (search "unsupported argument" *overlay*))
                   "run-shell -c must be accepted, not rejected")
               (is (search expected *overlay*)
                   "overlay must contain the command working directory"))))
      (ignore-errors (uiop:delete-directory-tree dir :validate t)))))

(test run-command-line-run-shell-E-captures-stderr
  "%run-command-line run-shell -E includes stderr in the displayed output."
  (with-fake-session (s)
    (let ((*overlay* nil))
      (cl-tmux::%run-command-line s
                                  "run-shell -E 'printf out; printf err >&2'")
      (is (null (search "unsupported argument" *overlay*))
          "run-shell -E must be accepted, not rejected")
      (is (search "out" *overlay*) "overlay must contain stdout")
      (is (search "err" *overlay*) "overlay must contain stderr"))))

(test run-command-line-run-shell-C-runs-tmux-command
  "%run-command-line run-shell -C executes the argument as a tmux command."
  (with-fake-session (s)
    (let ((*overlay* nil))
      (cl-tmux::%run-command-line s "run-shell -C 'display-message from-run-shell-C'")
      (is (search "from-run-shell-C" *overlay*)
          "run-shell -C must dispatch the supplied tmux command"))))

;;; ── if-shell ─────────────────────────────────────────────────────────────────

(test if-shell-dispatch-table
  "if-shell calls then-fn on zero exit; else-fn on non-zero exit."
  (dolist (row (list (list (lambda (f) (cl-tmux/commands:if-shell "true" f))
                           "zero exit → then-fn called")
                     (list (lambda (f) (cl-tmux/commands:if-shell "false"
                                                                   (lambda () nil)
                                                                   :else-fn f))
                           "non-zero exit → else-fn called")))
    (destructuring-bind (runner desc) row
      (let ((flag nil))
        (funcall runner (lambda () (setf flag t)))
        (is-true flag "~A" desc)))))

(test if-shell-no-applicable-callback-table
  "if-shell with no applicable callback is a no-op (no error signalled)."
  (dolist (row (list (list "false" (lambda () nil) "non-zero exit, no else-fn")
                     (list "true"  nil             "zero exit, nil then-fn")))
    (destructuring-bind (cmd then-fn desc) row
      (finishes (cl-tmux/commands:if-shell cmd then-fn) "~A must not error" desc))))

(test if-shell-timeout-returns-calls-else-fn
  "if-shell with a very short timeout calls ELSE-FN (timeout treated as non-zero exit)."
  (let ((else-called nil))
    (cl-tmux/commands:if-shell "sleep 60"
                               (lambda () nil)
                               :else-fn (lambda () (setf else-called t))
                               :timeout 1/1000)
    (is-true else-called "else-fn must be invoked when if-shell times out")))

;;; ── %selection-text ──────────────────────────────────────────────────────────
;;;
;;; %selection-text is a private helper in cl-tmux/commands that extracts the
;;; selected text from a copy-mode screen.  It returns NIL when no selection is
;;; active, a string for a single-row selection, and a newline-joined string for
;;; a multi-row selection.

(test selection-text-returns-nil-when-no-selection
  "%selection-text returns NIL when copy-selecting is NIL (no active selection)."
  (let ((s (copy-mode-screen :w 20 :h 5)))
    (is (null (cl-tmux/commands::%selection-text s))
        "%selection-text must return NIL when no selection is active")))

(test selection-text-returns-nil-when-mark-nil
  "%selection-text returns NIL when copy-selecting is T but mark is NIL."
  (let ((s (copy-mode-screen :w 20 :h 5
                             :selecting t
                             :cursor (cons 0 5))))
    (is (null (cl-tmux/commands::%selection-text s))
        "%selection-text must return NIL when mark is NIL")))

(test selection-text-single-row-returns-correct-text
  "%selection-text returns the correct string for a single-row selection."
  (let ((s (copy-mode-screen :w 20 :h 5
                             :content "hello world"
                             :mark (cons 0 0)
                             :cursor (cons 0 5)
                             :selecting t)))
    (let ((text (cl-tmux/commands::%selection-text s)))
      (is (stringp text) "%selection-text must return a string for a valid selection")
      (is (string= "hello" text)
          "%selection-text must return \"hello\" for cols 0-4 of row 0 (got ~S)" text))))

(test selection-text-multi-row-returns-newline-joined-text
  "%selection-text returns newline-joined text for a multi-row selection."
  (let ((s (copy-mode-screen :w 20 :h 5
                             :content (format nil "abc~C~Cdef" #\Return #\Linefeed)
                             :mark (cons 0 0)
                             :cursor (cons 1 3)
                             :selecting t)))
    (let ((text (cl-tmux/commands::%selection-text s)))
      (is (stringp text) "result must be a string")
      (is (find #\Newline text) "multi-row result must contain a newline")
      ;; Row 0 contributes cols 0..2 = "abc"; row 1 contributes cols 0..2 = "def".
      (is (string= (format nil "abc~%def") text)
          "%selection-text must be \"abc\\ndef\" for rows 0-1 (got ~S)" text))))

(test selection-text-reversed-mark-cursor-order
  "%selection-text normalises selection when cursor is before mark."
  (let ((s (copy-mode-screen :w 20 :h 5
                             :content "hello world"
                             :mark (cons 0 5)
                             :cursor (cons 0 0)
                             :selecting t)))
    (let ((text (cl-tmux/commands::%selection-text s)))
      (is (stringp text) "%selection-text must return a string even when mark > cursor")
      (is (string= "hello" text)
          "%selection-text must normalise reversed mark/cursor (got ~S)" text))))
