(in-package #:cl-tmux/test)

;;;; dispatch tests — part C: focus events on window switch (deliver-in/out),
;;;; list-keys overlay, select-pane, zoom-toggle, rename-session,
;;;; select-pane-in-direction, apply-named-layout, list-windows/sessions, display-panes.

(describe "dispatch-suite"

  ;;; ── focus events (?1004) on window switch ────────────────────────────────────

  ;; Switching windows sends ESC[I (focus gained) to the newly active window's pane
  ;; when that pane's app enabled focus events.
  (it "cycle-window-delivers-focus-in-to-new-window-pane"
    (with-pipe-fds (rfd wfd)
      (with-fake-session (s :nwindows 2 :npanes 1)
        (let* ((w1 (second (session-windows s)))
               (p1 (window-active-pane w1)))
          ;; Make the SECOND window's pane a live, focus-aware PTY.
          (setf (pane-fd p1) wfd)
          (setf (cl-tmux/terminal/types:screen-focus-events (pane-screen p1)) t)
          (cl-tmux::%cmd-cycle-window s #'cl-tmux::next-cyclic)
          (expect (eq w1 (session-active-window s)))
          (let ((ready (cl-tmux/pty:select-fds (list rfd) 200000)))  ; 200 ms
            (expect ready :to-be-truthy)
            (when ready
              (cffi:with-foreign-object (buf :uint8 8)
                (let ((n (cffi:foreign-funcall "read"
                                               :int rfd :pointer buf :unsigned-long 3
                                               :long)))
                  (expect (= 3 n))
                  (expect (= 27 (cffi:mem-aref buf :uint8 0)))
                  (expect (= 73 (cffi:mem-aref buf :uint8 2)))))))))))

  ;; Switching away from a window sends ESC[O (focus lost) to the window being left.
  (it "cycle-window-delivers-focus-out-to-old-window-pane"
    (with-pipe-fds (rfd wfd)
      (with-fake-session (s :nwindows 2 :npanes 1)
        (let* ((w0 (first (session-windows s)))
               (p0 (window-active-pane w0)))
          ;; The FIRST (currently active) window's pane is the live, focus-aware PTY.
          (setf (pane-fd p0) wfd)
          (setf (cl-tmux/terminal/types:screen-focus-events (pane-screen p0)) t)
          (cl-tmux::%cmd-cycle-window s #'cl-tmux::next-cyclic)
          (let ((ready (cl-tmux/pty:select-fds (list rfd) 200000)))
            (expect ready :to-be-truthy)
            (when ready
              (cffi:with-foreign-object (buf :uint8 8)
                (let ((n (cffi:foreign-funcall "read"
                                               :int rfd :pointer buf :unsigned-long 3
                                               :long)))
                  (expect (= 3 n))
                  (expect (= 27 (cffi:mem-aref buf :uint8 0)))
                  (expect (= 79 (cffi:mem-aref buf :uint8 2)))))))))))

  ;; Cycling windows runs the focus-transition path without error when panes have no
  ;; PTY, and still changes the active window.
  (it "cycle-window-with-focus-events-no-pty-no-error"
    (with-fake-session (s :nwindows 2 :npanes 1)
      (let ((w0 (first  (session-windows s)))
            (w1 (second (session-windows s))))
        (setf (cl-tmux/terminal/types:screen-focus-events
               (pane-screen (window-active-pane w0))) t)
        (setf (cl-tmux/terminal/types:screen-focus-events
               (pane-screen (window-active-pane w1))) t)
        (expect (eq w0 (session-active-window s)))
        (finishes (cl-tmux::%cmd-cycle-window s #'cl-tmux::next-cyclic))
        (expect (eq w1 (session-active-window s))))))

  ;;; ── list-keys overlay ───────────────────────────────────────────────────────

  ;; C-b ? opens the key-binding help overlay.
  (it "dispatch-list-keys-shows-overlay"
    (with-fake-session (s)
      (let ((*overlay* nil))
        (cl-tmux::dispatch-command s :list-keys nil)
        (assert-overlay-active "list-keys should open the help overlay"))))

  ;; %run-command-line list-keys [key] shows only matching bindings.
  (it "run-command-line-list-keys-filters-by-key"
    (with-fake-session (s)
      (let ((*overlay* nil))
        (cl-tmux::%run-command-line s "list-keys -T prefix C-Right")
        (expect (search "bind-key -T prefix -r C-Right resize-pane -R 1" *overlay*))
        (expect (null (search "bind-key -T prefix Up select-pane-up" *overlay*))))))

  ;; %run-command-line list-keys -1 keeps only the first line of output.
  (it "run-command-line-list-keys-dash-1-keeps-only-first-line"
    (with-fake-session (s)
      (let ((*overlay* nil))
        (cl-tmux::%run-command-line s "list-keys -T prefix")
        (let ((full *overlay*))
          (setf *overlay* nil)
          (cl-tmux::%run-command-line s "list-keys -T prefix -1")
          (let* ((newline (position #\Newline full))
                 (first-line (if newline (subseq full 0 newline) full)))
            (expect (string= *overlay* first-line))
            (expect (null (position #\Newline *overlay*))))))))

  ;; define-command-handlers is a defined macro.
  (it "define-command-handlers-macro-is-defined"
    (expect (macro-function 'cl-tmux::define-command-handlers)))

  ;; define-copy-mode-key-overrides is a defined macro.
  (it "define-copy-mode-key-overrides-macro-is-defined"
    (expect (macro-function 'cl-tmux::define-copy-mode-key-overrides)))

  ;;; ── select-pane-left/right/up/down dispatch ─────────────────────────────────
  ;;;
  ;;; These tests use the shared fixture macros from helpers-layout-fixtures.lisp instead of
  ;;; duplicating the setup inline.  with-two-pane-h-session and
  ;;; with-two-pane-v-session already encode the exact same geometry.

  ;; :select-pane-right/:select-pane-left move the active pane or stay put at edge.
  (it "dispatch-select-pane-horizontal-table"
    (dolist (c '((:select-pane-right nil  t   "right from p0 → p1")
                 (:select-pane-left   t   nil "left from p1 → p0")
                 (:select-pane-right  t   t   "right at rightmost → no-op")))
      (destructuring-bind (cmd start-p1 expected-p1 desc) c
        (declare (ignore desc))
        (with-two-pane-h-session (sess win p0 p1)
          (when start-p1 (window-select-pane win p1))
          (cl-tmux::dispatch-command sess cmd nil)
          (expect (eq (if expected-p1 p1 p0) (window-active-pane win)))))))

  ;; :select-pane-down/:select-pane-up move the active pane in a vertical split.
  (it "dispatch-select-pane-vertical-table"
    (dolist (c '((:select-pane-down nil  t   "down from p0 → p1")
                 (:select-pane-up    t   nil "up from p1 → p0")))
      (destructuring-bind (cmd start-p1 expected-p1 desc) c
        (declare (ignore desc))
        (with-two-pane-v-session (sess win p0 p1)
          (when start-p1 (window-select-pane win p1))
          (cl-tmux::dispatch-command sess cmd nil)
          (expect (eq (if expected-p1 p1 p0) (window-active-pane win)))))))

  ;;; ── zoom-toggle dispatch ────────────────────────────────────────────────────

  ;; :zoom-toggle zooms the active pane in and marks window-zoom-p as T.
  (it "dispatch-zoom-toggle-sets-zoom-flag"
    (with-two-pane-h-session (sess win p0 p1)
      (expect (and p0 p1))
      (cl-tmux::dispatch-command sess :zoom-toggle nil)
      (expect (cl-tmux/model:window-zoom-p win) :to-be-truthy)
      ;; Toggle back off.
      (cl-tmux::dispatch-command sess :zoom-toggle nil)
      (expect (cl-tmux/model:window-zoom-p win) :to-be-falsy)))

  ;;; ── rename-session dispatch ─────────────────────────────────────────────────

  ;; :rename-session opens a prompt seeded with the current session name, and
  ;; its on-submit closure renames the session.
  (it "dispatch-rename-session-opens-prompt"
    (with-fake-session (s :nwindows 1)
      (let ((*prompt* nil))
        (cl-tmux::dispatch-command s :rename-session nil)
        (expect (prompt-active-p))
        (expect (string= "0" (prompt-buffer *prompt*)))
        (funcall (prompt-on-submit *prompt*) "newsess")
        (expect (string= "newsess" (session-name s))))))

  ;;; ── %select-pane-in-direction ───────────────────────────────────────────────
  ;;;
  ;;; Use the shared fixture macros to avoid repeating pane/window/session setup.

  ;; %select-pane-in-direction :right/:left moves or stays in a horizontal split.
  (it "select-pane-in-direction-h-table"
    (dolist (c '((:right nil  t   "right from p0 → p1")
                 (:left   t   nil "left from p1 → p0")
                 (:right  t   t   "right at rightmost → no-op")))
      (destructuring-bind (dir start-p1 expected-p1 desc) c
        (declare (ignore desc))
        (with-two-pane-h-session (sess win p0 p1)
          (when start-p1 (window-select-pane win p1))
          (cl-tmux::%select-pane-in-direction sess dir)
          (expect (eq (if expected-p1 p1 p0) (window-active-pane win)))))))

  ;; %select-pane-in-direction :down from the top pane selects the bottom pane.
  (it "select-pane-in-direction-vertical-down-selects-lower-pane"
    (with-two-pane-v-session (sess win p0 p1)
      (expect (eq p0 (window-active-pane win)))
      (cl-tmux::%select-pane-in-direction sess :down)
      (expect (eq p1 (window-active-pane win)))))

  ;;; ── %apply-named-layout-to-session ──────────────────────────────────────────

  ;; %apply-named-layout-to-session :even-horizontal repositions two panes into
  ;; equal-width columns and rebuilds the window tree.
  (it "apply-named-layout-even-horizontal-repositions-panes"
    (with-two-pane-layout-session (sess win p0 p1)
      (cl-tmux::%apply-named-layout-to-session sess :even-horizontal)
      ;; After even-horizontal: avail-w = 81 - 1 = 80, each-w = 40.
      ;; p0 should be at x=0, p1 at x=41, both width=40.
      (expect (= 0  (pane-x p0)))
      (expect (= 40 (pane-width p0)))
      (expect (= 41 (pane-x p1)))
      (expect (= 40 (pane-width p1)))))

  ;; %apply-named-layout-to-session :even-vertical repositions two panes into
  ;; equal-height rows and rebuilds the window tree.
  (it "apply-named-layout-even-vertical-repositions-panes"
    ;; Use a vertical split (80×21) so the height arithmetic matches the assertions.
    (with-two-pane-v-session (sess win p0 p1)
      (cl-tmux::%apply-named-layout-to-session sess :even-vertical)
      ;; After even-vertical on an 80×21 window: avail-h = 21 - 1 = 20, each-h = 10.
      ;; p0 should be at y=0, p1 at y=11, both height=10.
      (expect (= 0  (pane-y p0)))
      (expect (= 10 (pane-height p0)))
      (expect (= 11 (pane-y p1)))
      (expect (= 10 (pane-height p1)))))

  ;; %apply-named-layout-to-session with no active window is a no-op.
  (it "apply-named-layout-noop-for-empty-session"
    (with-fake-session (sess :nwindows 0)
      (finishes (cl-tmux::%apply-named-layout-to-session sess :even-horizontal)
                "calling with no active window must not signal an error")))

  ;;; ── :list-windows dispatch ───────────────────────────────────────────────────

  ;; :list-windows opens an overlay containing the window name and marks *dirty*.
  (it "dispatch-list-windows-shows-overlay"
    (with-fake-session (s :nwindows 1)
      (let ((*overlay* nil))
        (cl-tmux::dispatch-command s :list-windows nil)
        (assert-overlay-active ":list-windows must open the overlay")
        (expect cl-tmux::*dirty* :to-be-truthy))))

  ;; :list-windows overlay text includes the active window name.
  (it "dispatch-list-windows-overlay-contains-window-name"
    (with-fake-session (s :nwindows 2)
      (let ((*overlay* nil))
        (cl-tmux::dispatch-command s :list-windows nil)
        (assert-overlay-contains "1" *overlay*
                                 "list-windows"))))

  ;;; ── :list-sessions dispatch ──────────────────────────────────────────────────

  ;; :list-sessions with empty *server-sessions* falls back to the session-name
  ;; single-line format and still opens an overlay.
  (it "dispatch-list-sessions-empty-registry-shows-overlay"
    (with-fake-session (s :nwindows 1)
      (let ((*overlay* nil)
            (cl-tmux::*server-sessions* nil))
        (cl-tmux::dispatch-command s :list-sessions nil)
        (assert-overlay-contains (session-name s) *overlay*
                                 ":list-sessions fallback"))))

  ;; :list-sessions with *server-sessions* populated lists every registered
  ;; session and marks the current one with an asterisk.
  (it "dispatch-list-sessions-populated-registry-shows-all-sessions"
    (with-fake-session (s :nwindows 1)
      (let ((name (session-name s)))
        (let ((*overlay* nil)
              (cl-tmux::*server-sessions* (list (cons name s))))
          (cl-tmux::dispatch-command s :list-sessions nil)
          (assert-overlay-contains name *overlay*
                                   ":list-sessions")
          (assert-overlay-contains "*" *overlay*
                                   ":list-sessions")))))

  ;;; ── :display-panes dispatch ──────────────────────────────────────────────────

  ;; :display-panes with a 2-pane session arms the per-pane number display (via a
  ;; timing overlay) and renders big-digit pane numbers.
  (it "dispatch-display-panes-shows-pane-numbers"
    (with-two-pane-h-session (sess win p0 p1)
      (expect (and win p0 p1))
      (let ((*overlay* nil) (cl-tmux/prompt:*display-panes-active* nil))
        (cl-tmux::dispatch-command sess :display-panes nil)
        (assert-overlay-active ":display-panes opens the (timing) overlay")
        (expect cl-tmux/prompt:*display-panes-active* :to-be-truthy)
        (let ((frame (cl-tmux/renderer:render-session-to-string sess 24 81)))
          (expect (find #\█ frame))))))

  ;; :display-panes sets *dirty* to T.
  (it "dispatch-display-panes-marks-dirty"
    (with-fake-session (sess :nwindows 1 :npanes 1)
      (let ((*overlay* nil))
        (cl-tmux::dispatch-command sess :display-panes nil)
        (expect cl-tmux::*dirty* :to-be-truthy))))

  ;; %run-command-line display-panes accepts -d and arms pane numbers.
  (it "run-command-line-display-panes-arms-overlay"
    (with-fake-session (sess :nwindows 1 :npanes 1)
      (let ((*overlay* nil)
            (cl-tmux/prompt:*display-panes-active* nil)
            (saved (cl-tmux/options:get-option "display-panes-time" 1000)))
        (unwind-protect
             (progn
               (cl-tmux/options:set-option "display-panes-time" 1000)
               (cl-tmux::%run-command-line sess "display-panes -d 125")
               (assert-overlay-active "display-panes command must open the timing overlay")
               (expect cl-tmux/prompt:*display-panes-active* :to-be-truthy)
               (expect (= 1000 (cl-tmux/options:get-option "display-panes-time"))))
          (cl-tmux/options:set-option "display-panes-time" saved)))))

  ;; display-panes rejects arguments that do not affect the local pane-number overlay.
  (it "run-command-line-display-panes-rejects-non-domain-args"
    (with-fake-session (sess :nwindows 1 :npanes 1)
      (dolist (args '(("-b") ("-N") ("-F" "#{pane_id}") ("-t" "client0") ("template")))
        (let ((*overlay* nil))
          (cl-tmux::%cmd-display-panes-arg sess args)
          (assert-overlay-contains "display-panes: unsupported argument"
                                   *overlay*
                                   (format nil "~S must be rejected" args)))))))
