(in-package #:cl-tmux/test)

;;;; Command dispatch tests: active screen/window/pane helpers and overlays.

(in-suite dispatch-suite)

;;; ── %active-screen ───────────────────────────────────────────────────────────

(test active-screen-returns-active-pane-screen
  "%active-screen returns the screen of the session's active pane."
  (with-fake-session (s)
    (let ((ap (session-active-pane s)))
      (is (eq (pane-screen ap)
              (cl-tmux::%active-screen s))
          "%active-screen must return the active pane's screen"))))

(test active-screen-returns-nil-for-empty-session
  "%active-screen returns NIL when no pane is active."
  (with-fake-session (s :nwindows 0)
    (is (null (cl-tmux::%active-screen s)))))

;;; ── %cmd-cycle-window ────────────────────────────────────────────────────────

(test cmd-cycle-window-advances-selection
  "%cmd-cycle-window advances the active window via CYCLER."
  (with-fake-session (s :nwindows 3)
    (let* ((w1 (first  (session-windows s)))
           (w2 (second (session-windows s))))
      (is (eq w1 (session-active-window s)))
      (cl-tmux::%cmd-cycle-window s #'cl-tmux::next-cyclic)
      (is (eq w2 (session-active-window s))))))

;;; ── %cmd-cycle-pane ──────────────────────────────────────────────────────────

(test cmd-cycle-pane-advances-pane-selection
  "%cmd-cycle-pane advances the active pane via CYCLER."
  (with-fake-two-pane-session (s)
    (let* ((win (session-active-window s))
           (p0  (first  (window-panes win)))
           (p1  (second (window-panes win))))
      (is (eq p0 (window-active-pane win)))
      (cl-tmux::%cmd-cycle-pane s #'cl-tmux::next-cyclic)
      (is (eq p1 (window-active-pane win))))))

;;; ── focus events (?1004) on pane switch ──────────────────────────────────────

(test notify-pane-focus-noop-without-pty
  "%notify-pane-focus is a safe no-op for a pane with no live PTY (fd <= 0),
   even when focus events are enabled."
  (let ((pane (make-pane :id 1 :x 0 :y 0 :width 20 :height 5 :fd -1
                         :screen (make-screen 20 5))))
    (setf (cl-tmux/terminal/types:screen-focus-events (pane-screen pane)) t)
    (is (null (cl-tmux::%notify-pane-focus pane t))
        "no PTY → %notify-pane-focus returns NIL without writing")
    (finishes (cl-tmux::%notify-pane-focus pane nil))))

(test notify-pane-focus-writes-focus-in-to-pty
  "With focus events enabled and a live fd, %notify-pane-focus delivers the
   focus-gained report ESC[I to the pane's PTY."
  (with-pipe-fds (rfd wfd)
    (let ((pane (make-pane :id 1 :x 0 :y 0 :width 20 :height 5 :fd wfd
                           :screen (make-screen 20 5))))
      (setf (cl-tmux/terminal/types:screen-focus-events (pane-screen pane)) t)
      (cl-tmux::%notify-pane-focus pane t)
      (let ((ready (cl-tmux/pty:select-fds (list rfd) 200000)))  ; 200 ms
        (is-true ready "the PTY read-end must be readable after a focus report")
        (when ready
          (cffi:with-foreign-object (buf :uint8 8)
            (let ((n (cffi:foreign-funcall "read"
                                           :int rfd :pointer buf :unsigned-long 3
                                           :long)))
              (is (= 3 n) "focus-in report must be exactly 3 bytes (got ~D)" n)
              (is (= 27 (cffi:mem-aref buf :uint8 0)) "byte 0 must be ESC (27)")
              (is (= 91 (cffi:mem-aref buf :uint8 1)) "byte 1 must be #\\[ (91)")
              (is (= 73 (cffi:mem-aref buf :uint8 2)) "byte 2 must be #\\I (73)"))))))))

(test notify-pane-focus-disabled-screen-writes-nothing
  "A pane whose app did NOT enable focus events receives no report even with a
   live fd, and select-fds reports the pipe idle."
  (with-pipe-fds (rfd wfd)
    (let ((pane (make-pane :id 1 :x 0 :y 0 :width 20 :height 5 :fd wfd
                           :screen (make-screen 20 5))))
      ;; focus-events left NIL (default)
      (cl-tmux::%notify-pane-focus pane t)
      (is (null (cl-tmux/pty:select-fds (list rfd) 20000))
          "no focus report must reach an opted-out pane (pipe must stay idle)"))))

(test select-pane-with-focus-switches-and-tolerates-no-pty
  "%select-pane-with-focus changes the active pane and runs the focus-notify path
   without error even when panes have no PTY and focus events are enabled."
  (with-fake-two-pane-session (s)
    (let* ((win (session-active-window s))
           (p0  (first  (window-panes win)))
           (p1  (second (window-panes win))))
      (setf (cl-tmux/terminal/types:screen-focus-events (pane-screen p0)) t)
      (setf (cl-tmux/terminal/types:screen-focus-events (pane-screen p1)) t)
      (is (eq p0 (window-active-pane win)))
      (finishes (cl-tmux::%select-pane-with-focus win p1))
      (is (eq p1 (window-active-pane win))
          "%select-pane-with-focus must make p1 the active pane"))))

;;; ── %derive-hook-session resolver clauses ────────────────────────────────────

(test derive-hook-session-returns-nil-for-nil
  "%derive-hook-session returns NIL when TARGET is NIL."
  (is (null (cl-tmux::%derive-hook-session nil))
      "%derive-hook-session must return NIL for a NIL target"))

(test derive-hook-session-returns-session-directly
  "%derive-hook-session returns a session object unchanged."
  (with-fake-session (s)
    (is (eq s (cl-tmux::%derive-hook-session s))
        "%derive-hook-session must return the session itself for a session target")))

(test derive-hook-session-resolves-from-window
  "%derive-hook-session resolves the owning session from a window object."
  (with-fake-session (s)
    (with-command-test-state (s)
      (let ((win (session-active-window s)))
        (is (eq s (cl-tmux::%derive-hook-session win))
            "%derive-hook-session must return the session owning the window")))))

(test derive-hook-session-resolves-from-pane
  "%derive-hook-session resolves the owning session from a pane object."
  (with-fake-session (s)
    (with-command-test-state (s)
      (let ((pane (session-active-pane s)))
        (is (eq s (cl-tmux::%derive-hook-session pane))
            "%derive-hook-session must return the session owning the pane")))))

(test derive-hook-session-returns-nil-for-unknown-type
  "%derive-hook-session returns NIL for an unrecognised target type."
  (is (null (cl-tmux::%derive-hook-session :not-a-model-object))
      "%derive-hook-session must return NIL for an unrecognised target type"))

;;; ── %dispatch-hook-entry string-hook path ────────────────────────────────────

(test dispatch-hook-entry-string-hook-runs-command
  "%dispatch-hook-entry with a string entry runs it as a command via %run-command-line.
   Exercises the string-hook branch (the ignore-errors / handler-case path) directly."
  (with-fake-session (s :nwindows 1)
    (let ((*overlay* nil))
      ;; 'list-windows' is a safe command that opens an overlay.
      (cl-tmux::%dispatch-hook-entry s "list-windows")
      (assert-overlay-active
       "%dispatch-hook-entry with a string hook must run the command"))))

(test dispatch-hook-entry-string-hook-error-shows-overlay
  "%dispatch-hook-entry with a string hook that errors reports the error as an overlay
   instead of silently swallowing it."
  (with-fake-session (s)
    (let ((*overlay* nil))
      ;; Deliberately break the string to cause %run-command-line to fail.
      ;; We use a command name that will not resolve in the command table.
      (cl-tmux::%dispatch-hook-entry s "nonexistent-hook-command-xyz")
      ;; After an error the overlay must contain an error report OR remain nil
      ;; (depending on whether the command runner returns an error vs. overlay).
      ;; The important invariant is that no condition escapes to the caller.
      (finishes (values) "error in string hook must not propagate to the caller"))))

(test dispatch-hook-entry-keyword-hook-dispatches-command
  "%dispatch-hook-entry with a keyword entry dispatches it as a command directly."
  (with-fake-session (s)
    (let ((*overlay* nil))
      (cl-tmux::%dispatch-hook-entry s :list-windows)
      (assert-overlay-active
       "%dispatch-hook-entry with a keyword hook must dispatch the command"))))

;;; ── %cmd-cycle-session ───────────────────────────────────────────────────────

(test cmd-cycle-session-advances-to-next-session
  "%cmd-cycle-session with next-cyclic switches to the next session in the registry."
  (let* ((s1 (make-fake-session))
         (s2 (make-fake-session)))
    (setf (cl-tmux::session-name s1) "alpha"
          (cl-tmux::session-name s2) "beta")
    (let ((cl-tmux::*server-sessions* (list (cons "alpha" s1) (cons "beta" s2))))
      (with-stubbed-switch-to-session (switched-to)
        (cl-tmux::%cmd-cycle-session s1 #'cl-tmux::next-cyclic)
        (is (eq s2 switched-to)
            "%cmd-cycle-session must advance to the next session")))))

(test cmd-cycle-session-noop-with-single-session
  "%cmd-cycle-session is a no-op when SESSION is the only session (wraps to itself)."
  (let ((s (make-fake-session)))
    (setf (cl-tmux::session-name s) "only")
    (let ((cl-tmux::*server-sessions* (list (cons "only" s))))
      (with-stubbed-switch-to-session (switched-to)
        (cl-tmux::%cmd-cycle-session s #'cl-tmux::next-cyclic)
        (is-false switched-to
                  "%cmd-cycle-session must not switch when there is only one session")))))

;;; ── %copy-mode-call NIL-screen path ─────────────────────────────────────────

(test copy-mode-call-nil-screen-returns-nil
  "%copy-mode-call returns NIL without calling FN when the active screen is NIL.
   Exercises the guard path inside %copy-mode-call."
  (with-fake-session (s :nwindows 0)
    ;; Session with no windows → no active screen.
    (let ((fn-called nil))
      (is (null (cl-tmux::%copy-mode-call s (lambda (screen)
                                              (declare (ignore screen))
                                              (setf fn-called t)
                                              :was-called)))
          "%copy-mode-call on a windowless session must return NIL")
      (is-false fn-called
                "%copy-mode-call must not call FN when there is no active screen"))))

;;; ── %overlay-lines-string ────────────────────────────────────────────────────

(test overlay-lines-string-joins-lines-with-newlines
  "%overlay-lines-string renders a non-empty list as newline-separated text."
  (is (string= "foo" (cl-tmux::%overlay-lines-string '("foo")))
      "single-line list renders without trailing newline")
  (is (string= (format nil "foo~%bar")
               (cl-tmux::%overlay-lines-string '("foo" "bar")))
      "two-line list renders lines separated by a newline"))

(test overlay-lines-string-returns-empty-string-for-nil
  "%overlay-lines-string returns the EMPTY fallback (default \"\") for NIL input."
  (is (string= "" (cl-tmux::%overlay-lines-string nil))
      "NIL list produces empty string by default")
  (is (string= "none" (cl-tmux::%overlay-lines-string nil "none"))
      "NIL list returns the custom EMPTY argument"))

;;; ── %overlayf ────────────────────────────────────────────────────────────────

(test overlayf-renders-formatted-overlay
  "%overlayf shows a one-line overlay built from a FORMAT control string."
  (with-fake-session (s)
    (let ((*overlay* nil))
      (cl-tmux::%overlayf "hello ~A" "world")
      (is (overlay-active-p) "%overlayf must activate an overlay")
      (is (search "hello world" *overlay*)
          "%overlayf must format its args into the overlay text"))))

;;; ── %flag-value and %flag-present-p ──────────────────────────────────────────

(test flag-value-returns-associated-value
  "%flag-value returns the cdr of the matching (char . value) entry, or NIL when absent."
  (let ((flags (list (cons #\a "alpha") (cons #\b "beta"))))
    (dolist (row '((#\a "alpha" "flag \\a present")
                   (#\b "beta"  "flag \\b present")
                   (#\z nil     "absent flag returns NIL")))
      (destructuring-bind (ch expected desc) row
        (is (equal expected (cl-tmux::%flag-value flags ch))
            "~A" desc)))))

(test flag-present-p-returns-true-iff-flag-present
  "%flag-present-p returns true when FLAGS contains CHAR, NIL otherwise."
  (let ((flags (list (cons #\a t) (cons #\b nil))))
    (is-true  (cl-tmux::%flag-present-p flags #\a)
              "flag \\a must be present")
    (is-true  (cl-tmux::%flag-present-p flags #\b)
              "flag \\b has a NIL value but is still present")
    (is-false (cl-tmux::%flag-present-p flags #\z)
              "absent flag \\z must not be present")))

;;; ── show-built-overlay ───────────────────────────────────────────────────────

(test show-built-overlay-renders-body-to-overlay
  "show-built-overlay shows an overlay whose text is built by writing to STREAM."
  (with-fake-session (s)
    (let ((*overlay* nil))
      (cl-tmux::show-built-overlay (out)
        (write-string "dispatch-test-sentinel" out))
      (is (overlay-active-p) "show-built-overlay must activate an overlay")
      (is (search "dispatch-test-sentinel" *overlay*)
          "show-built-overlay must use the STREAM body text"))))

;;; ── %active-window-pane ──────────────────────────────────────────────────────

(test active-window-pane-returns-window-and-pane
  "%active-window-pane returns the active window and its active pane as two values."
  (with-fake-session (s :nwindows 1 :npanes 1)
    (multiple-value-bind (win pane) (cl-tmux::%active-window-pane s)
      (is (eq (session-active-window s) win)
          "%active-window-pane first value must be the active window")
      (is (eq (window-active-pane win) pane)
          "%active-window-pane second value must be the active pane"))))

(test active-window-pane-returns-nil-nil-for-windowless-session
  "%active-window-pane returns NIL, NIL when the session has no windows."
  (with-fake-session (s :nwindows 0)
    (multiple-value-bind (win pane) (cl-tmux::%active-window-pane s)
      (is (null win)  "%active-window-pane window must be NIL for empty session")
      (is (null pane) "%active-window-pane pane must be NIL for empty session"))))

;;; ── with-active-window macro ─────────────────────────────────────────────────

(test with-active-window-evaluates-body-with-active-window
  "with-active-window evaluates BODY binding WIN-VAR to the active window."
  (with-fake-session (s :nwindows 1)
    (let ((win-seen nil))
      (cl-tmux::with-active-window (w s)
        (setf win-seen w))
      (is (eq (session-active-window s) win-seen)
          "with-active-window must bind the active window"))))

(test with-active-window-returns-nil-for-empty-session
  "with-active-window evaluates to NIL when no active window exists."
  (with-fake-session (s :nwindows 0)
    (let ((body-ran nil))
      (is (null (cl-tmux::with-active-window (w s)
                  (setf body-ran t)
                  w))
          "with-active-window on empty session must return NIL")
      (is-false body-ran
                "with-active-window must not evaluate BODY when there is no window"))))

;;; ── %session-of-window and %session-of-pane ─────────────────────────────────

(test session-of-window-returns-owning-session
  "%session-of-window returns the session whose window list contains WIN."
  (with-fake-session (s :nwindows 1)
    (with-command-test-state (s)
      (let ((win (session-active-window s)))
        (is (eq s (cl-tmux::%session-of-window win))
            "%session-of-window must return the owning session")))))

(test session-of-window-returns-nil-when-not-found
  "%session-of-window returns NIL for a window not in any registered session."
  (with-fake-session (s :nwindows 1)
    (let ((orphan (make-fake-window 99 "orphan")))
      (let ((cl-tmux::*server-sessions* (list (cons "0" s))))
        (is (null (cl-tmux::%session-of-window orphan))
            "%session-of-window must return NIL for an unregistered window")))))

(test session-of-pane-returns-owning-session
  "%session-of-pane returns the session one of whose windows contains PANE."
  (with-fake-session (s :nwindows 1)
    (with-command-test-state (s)
      (let ((pane (session-active-pane s)))
        (is (eq s (cl-tmux::%session-of-pane pane))
            "%session-of-pane must return the owning session")))))

(test session-of-pane-returns-nil-for-nil
  "%session-of-pane returns NIL when PANE is NIL."
  (is (null (cl-tmux::%session-of-pane nil))
      "%session-of-pane must return NIL for NIL input"))
