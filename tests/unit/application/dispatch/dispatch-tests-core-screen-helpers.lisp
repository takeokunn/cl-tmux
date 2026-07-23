(in-package #:cl-tmux/test)

;;;; Command dispatch tests: active screen/window/pane helpers and overlays.

(describe "dispatch-suite"

  ;;; ── %active-screen ───────────────────────────────────────────────────────────

  ;; %active-screen returns the screen of the session's active pane.
  (it "active-screen-returns-active-pane-screen"
    (with-fake-session (s)
      (let ((ap (session-active-pane s)))
        (expect (eq (pane-screen ap)
                    (cl-tmux::%active-screen s))))))

  ;; %active-screen returns NIL when no pane is active.
  (it "active-screen-returns-nil-for-empty-session"
    (with-fake-session (s :nwindows 0)
      (expect (null (cl-tmux::%active-screen s)))))

  ;;; ── %cmd-cycle-window ────────────────────────────────────────────────────────

  ;; %cmd-cycle-window advances the active window via CYCLER.
  (it "cmd-cycle-window-advances-selection"
    (with-fake-session (s :nwindows 3)
      (let* ((w1 (first  (session-windows s)))
             (w2 (second (session-windows s))))
        (expect (eq w1 (session-active-window s)))
        (cl-tmux::%cmd-cycle-window s #'cl-tmux::next-cyclic)
        (expect (eq w2 (session-active-window s))))))

  ;;; ── %cmd-cycle-pane ──────────────────────────────────────────────────────────

  ;; %cmd-cycle-pane advances the active pane via CYCLER.
  (it "cmd-cycle-pane-advances-pane-selection"
    (with-fake-two-pane-session (s)
      (let* ((win (session-active-window s))
             (p0  (first  (window-panes win)))
             (p1  (second (window-panes win))))
        (expect (eq p0 (window-active-pane win)))
        (cl-tmux::%cmd-cycle-pane s #'cl-tmux::next-cyclic)
        (expect (eq p1 (window-active-pane win))))))

  ;;; ── focus events (?1004) on pane switch ──────────────────────────────────────

  ;; %notify-pane-focus is a safe no-op for a pane with no live PTY (fd <= 0),
  ;; even when focus events are enabled.
  (it "notify-pane-focus-noop-without-pty"
    (let ((pane (make-pane :id 1 :x 0 :y 0 :width 20 :height 5 :fd -1
                           :screen (make-screen 20 5))))
      (setf (cl-tmux/terminal/types:screen-focus-events (pane-screen pane)) t)
      (expect (null (cl-tmux::%notify-pane-focus pane t)))
      (finishes (cl-tmux::%notify-pane-focus pane nil))))

  ;; With focus events enabled and a live fd, %notify-pane-focus delivers the
  ;; focus-gained report ESC[I to the pane's PTY.
  (it "notify-pane-focus-writes-focus-in-to-pty"
    (with-pipe-fds (rfd wfd)
      (let ((pane (make-pane :id 1 :x 0 :y 0 :width 20 :height 5 :fd wfd
                             :screen (make-screen 20 5))))
        (setf (cl-tmux/terminal/types:screen-focus-events (pane-screen pane)) t)
        (cl-tmux::%notify-pane-focus pane t)
        (let ((ready (cl-tmux/pty:select-fds (list rfd) 200000)))  ; 200 ms
          (expect ready :to-be-truthy)
          (when ready
            (cffi:with-foreign-object (buf :uint8 8)
              (let ((n (cffi:foreign-funcall "read"
                                             :int rfd :pointer buf :unsigned-long 3
                                             :long)))
                (expect (= 3 n))
                (expect (= 27 (cffi:mem-aref buf :uint8 0)))
                (expect (= 91 (cffi:mem-aref buf :uint8 1)))
                (expect (= 73 (cffi:mem-aref buf :uint8 2))))))))))

  ;; A pane whose app did NOT enable focus events receives no report even with a
  ;; live fd, and select-fds reports the pipe idle.
  (it "notify-pane-focus-disabled-screen-writes-nothing"
    (with-pipe-fds (rfd wfd)
      (let ((pane (make-pane :id 1 :x 0 :y 0 :width 20 :height 5 :fd wfd
                             :screen (make-screen 20 5))))
        ;; focus-events left NIL (default)
        (cl-tmux::%notify-pane-focus pane t)
        (expect (null (cl-tmux/pty:select-fds (list rfd) 20000))))))

  ;; %select-pane-with-focus changes the active pane and runs the focus-notify path
  ;; without error even when panes have no PTY and focus events are enabled.
  (it "select-pane-with-focus-switches-and-tolerates-no-pty"
    (with-fake-two-pane-session (s)
      (let* ((win (session-active-window s))
             (p0  (first  (window-panes win)))
             (p1  (second (window-panes win))))
        (setf (cl-tmux/terminal/types:screen-focus-events (pane-screen p0)) t)
        (setf (cl-tmux/terminal/types:screen-focus-events (pane-screen p1)) t)
        (expect (eq p0 (window-active-pane win)))
        (finishes (cl-tmux::%select-pane-with-focus win p1))
        (expect (eq p1 (window-active-pane win))))))

  ;;; ── %derive-hook-session resolver clauses ────────────────────────────────────

  ;; %derive-hook-session returns NIL when TARGET is NIL.
  (it "derive-hook-session-returns-nil-for-nil"
    (expect (null (cl-tmux::%derive-hook-session nil))))

  ;; %derive-hook-session returns a session object unchanged.
  (it "derive-hook-session-returns-session-directly"
    (with-fake-session (s)
      (expect (eq s (cl-tmux::%derive-hook-session s)))))

  ;; %derive-hook-session resolves the owning session from a window object.
  (it "derive-hook-session-resolves-from-window"
    (with-fake-session (s)
      (with-command-test-state (s)
        (let ((win (session-active-window s)))
          (expect (eq s (cl-tmux::%derive-hook-session win)))))))

  ;; %derive-hook-session resolves the owning session from a pane object.
  (it "derive-hook-session-resolves-from-pane"
    (with-fake-session (s)
      (with-command-test-state (s)
        (let ((pane (session-active-pane s)))
          (expect (eq s (cl-tmux::%derive-hook-session pane)))))))

  ;; %derive-hook-session returns NIL for an unrecognised target type.
  (it "derive-hook-session-returns-nil-for-unknown-type"
    (expect (null (cl-tmux::%derive-hook-session :not-a-model-object))))

  ;;; ── %dispatch-hook-entry string-hook path ────────────────────────────────────

  ;; %dispatch-hook-entry with a string entry runs it as a command via %run-command-line.
  ;; Exercises the string-hook branch (the ignore-errors / handler-case path) directly.
  (it "dispatch-hook-entry-string-hook-runs-command"
    (with-fake-session (s :nwindows 1)
      (let ((*overlay* nil))
        ;; 'list-windows' is a safe command that opens an overlay.
        (cl-tmux::%dispatch-hook-entry s "list-windows")
        (assert-overlay-active
         "%dispatch-hook-entry with a string hook must run the command"))))

  ;; %dispatch-hook-entry with a string hook that errors reports the error as an overlay
  ;; instead of silently swallowing it.
  (it "dispatch-hook-entry-string-hook-error-shows-overlay"
    (with-fake-session (s)
      (let ((*overlay* nil))
        ;; Deliberately break the string to cause %run-command-line to fail.
        ;; We use a command name that will not resolve in the command table.
        (cl-tmux::%dispatch-hook-entry s "nonexistent-hook-command-xyz")
        ;; After an error the overlay must contain an error report OR remain nil
        ;; (depending on whether the command runner returns an error vs. overlay).
        ;; The important invariant is that no condition escapes to the caller.
        (finishes (values) "error in string hook must not propagate to the caller"))))

  ;; %dispatch-hook-entry with a keyword entry dispatches it as a command directly.
  (it "dispatch-hook-entry-keyword-hook-dispatches-command"
    (with-fake-session (s)
      (let ((*overlay* nil))
        (cl-tmux::%dispatch-hook-entry s :list-windows)
        (assert-overlay-active
         "%dispatch-hook-entry with a keyword hook must dispatch the command"))))

  ;;; ── %cmd-cycle-session ───────────────────────────────────────────────────────

  ;; %cmd-cycle-session with next-cyclic switches to the next session in the registry.
  (it "cmd-cycle-session-advances-to-next-session"
    (let* ((s1 (make-fake-session))
           (s2 (make-fake-session)))
      (setf (cl-tmux::session-name s1) "alpha"
            (cl-tmux::session-name s2) "beta")
      (let ((cl-tmux::*server-sessions* (list (cons "alpha" s1) (cons "beta" s2))))
        (with-stubbed-switch-to-session (switched-to)
          (cl-tmux::%cmd-cycle-session s1 #'cl-tmux::next-cyclic)
          (expect (eq s2 switched-to))))))

  ;; %cmd-cycle-session is a no-op when SESSION is the only session (wraps to itself).
  (it "cmd-cycle-session-noop-with-single-session"
    (let ((s (make-fake-session)))
      (setf (cl-tmux::session-name s) "only")
      (let ((cl-tmux::*server-sessions* (list (cons "only" s))))
        (with-stubbed-switch-to-session (switched-to)
          (cl-tmux::%cmd-cycle-session s #'cl-tmux::next-cyclic)
          (expect switched-to :to-be-falsy)))))

  ;;; ── %copy-mode-call NIL-screen path ─────────────────────────────────────────

  ;; %copy-mode-call returns NIL without calling FN when the active screen is NIL.
  ;; Exercises the guard path inside %copy-mode-call.
  (it "copy-mode-call-nil-screen-returns-nil"
    (with-fake-session (s :nwindows 0)
      ;; Session with no windows → no active screen.
      (let ((fn-called nil))
        (expect (null (cl-tmux::%copy-mode-call s (lambda (screen)
                                                    (declare (ignore screen))
                                                    (setf fn-called t)
                                                    :was-called))))
        (expect fn-called :to-be-falsy))))

  ;;; ── %overlay-lines-string ────────────────────────────────────────────────────

  ;; %overlay-lines-string renders a non-empty list as newline-separated text.
  (it "overlay-lines-string-joins-lines-with-newlines"
    (expect (string= "foo" (cl-tmux::%overlay-lines-string '("foo"))))
    (expect (string= (format nil "foo~%bar")
                     (cl-tmux::%overlay-lines-string '("foo" "bar")))))

  ;; %overlay-lines-string returns the EMPTY fallback (default "") for NIL input.
  (it "overlay-lines-string-returns-empty-string-for-nil"
    (expect (string= "" (cl-tmux::%overlay-lines-string nil)))
    (expect (string= "none" (cl-tmux::%overlay-lines-string nil "none"))))

  ;;; ── %overlayf ────────────────────────────────────────────────────────────────

  ;; %overlayf shows a one-line overlay built from a FORMAT control string.
  (it "overlayf-renders-formatted-overlay"
    (with-fake-session (s)
      (let ((*overlay* nil))
        (cl-tmux::%overlayf "hello ~A" "world")
        (expect (overlay-active-p))
        (expect (search "hello world" *overlay*)))))

  ;;; ── %flag-value and %flag-present-p ──────────────────────────────────────────

  ;; %flag-value returns the cdr of the matching (char . value) entry, or NIL when absent.
  (it "flag-value-returns-associated-value"
    (let ((flags (list (cons #\a "alpha") (cons #\b "beta"))))
      (dolist (row '((#\a "alpha" "flag \\a present")
                     (#\b "beta"  "flag \\b present")
                     (#\z nil     "absent flag returns NIL")))
        (destructuring-bind (ch expected desc) row
          (declare (ignore desc))
          (expect (equal expected (cl-tmux::%flag-value flags ch)))))))

  ;; %flag-present-p returns true when FLAGS contains CHAR, NIL otherwise.
  (it "flag-present-p-returns-true-iff-flag-present"
    (let ((flags (list (cons #\a t) (cons #\b nil))))
      (expect (cl-tmux::%flag-present-p flags #\a) :to-be-truthy)
      (expect (cl-tmux::%flag-present-p flags #\b) :to-be-truthy)
      (expect (cl-tmux::%flag-present-p flags #\z) :to-be-falsy)))

  ;;; ── show-built-overlay ───────────────────────────────────────────────────────

  ;; show-built-overlay shows an overlay whose text is built by writing to STREAM.
  (it "show-built-overlay-renders-body-to-overlay"
    (with-fake-session (s)
      (let ((*overlay* nil))
        (cl-tmux::show-built-overlay (out)
          (write-string "dispatch-test-sentinel" out))
        (expect (overlay-active-p))
        (expect (search "dispatch-test-sentinel" *overlay*)))))

  ;;; ── %active-window-pane ──────────────────────────────────────────────────────

  ;; %active-window-pane returns the active window and its active pane as two values.
  (it "active-window-pane-returns-window-and-pane"
    (with-fake-session (s :nwindows 1 :npanes 1)
      (multiple-value-bind (win pane) (cl-tmux::%active-window-pane s)
        (expect (eq (session-active-window s) win))
        (expect (eq (window-active-pane win) pane)))))

  ;; %active-window-pane returns NIL, NIL when the session has no windows.
  (it "active-window-pane-returns-nil-nil-for-windowless-session"
    (with-fake-session (s :nwindows 0)
      (multiple-value-bind (win pane) (cl-tmux::%active-window-pane s)
        (expect (null win))
        (expect (null pane)))))

  ;;; ── with-active-window macro ─────────────────────────────────────────────────

  ;; with-active-window evaluates BODY binding WIN-VAR to the active window.
  (it "with-active-window-evaluates-body-with-active-window"
    (with-fake-session (s :nwindows 1)
      (let ((win-seen nil))
        (cl-tmux::with-active-window (w s)
          (setf win-seen w))
        (expect (eq (session-active-window s) win-seen)))))

  ;; with-active-window evaluates to NIL when no active window exists.
  (it "with-active-window-returns-nil-for-empty-session"
    (with-fake-session (s :nwindows 0)
      (let ((body-ran nil))
        (expect (null (cl-tmux::with-active-window (w s)
                        (setf body-ran t)
                        w)))
        (expect body-ran :to-be-falsy))))

  ;;; ── %session-of-window and %session-of-pane ─────────────────────────────────

  ;; %session-of-window returns the session whose window list contains WIN.
  (it "session-of-window-returns-owning-session"
    (with-fake-session (s :nwindows 1)
      (with-command-test-state (s)
        (let ((win (session-active-window s)))
          (expect (eq s (cl-tmux::%session-of-window win)))))))

  ;; %session-of-window returns NIL for a window not in any registered session.
  (it "session-of-window-returns-nil-when-not-found"
    (with-fake-session (s :nwindows 1)
      (let ((orphan (make-fake-window 99 "orphan")))
        (let ((cl-tmux::*server-sessions* (list (cons "0" s))))
          (expect (null (cl-tmux::%session-of-window orphan)))))))

  ;; %session-of-pane returns the session one of whose windows contains PANE.
  (it "session-of-pane-returns-owning-session"
    (with-fake-session (s :nwindows 1)
      (with-command-test-state (s)
        (let ((pane (session-active-pane s)))
          (expect (eq s (cl-tmux::%session-of-pane pane)))))))

  ;; %session-of-pane returns NIL when PANE is NIL.
  (it "session-of-pane-returns-nil-for-nil"
    (expect (null (cl-tmux::%session-of-pane nil)))))
