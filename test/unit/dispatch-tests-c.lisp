(in-package #:cl-tmux/test)

;;;; dispatch tests — part C: focus events on window switch (deliver-in/out),
;;;; list-keys overlay, select-pane, zoom-toggle, rename-session,
;;;; select-pane-in-direction, apply-named-layout, list-windows/sessions, display-panes.

(in-suite dispatch-suite)

;;; ── focus events (?1004) on window switch ────────────────────────────────────

(test cycle-window-delivers-focus-in-to-new-window-pane
  "Switching windows sends ESC[I (focus gained) to the newly active window's pane
   when that pane's app enabled focus events."
  (with-pipe-fds (rfd wfd)
    (with-fake-session (s :nwindows 2 :npanes 1)
      (let* ((w1 (second (session-windows s)))
             (p1 (window-active-pane w1)))
        ;; Make the SECOND window's pane a live, focus-aware PTY.
        (setf (pane-fd p1) wfd)
        (setf (cl-tmux/terminal/types:screen-focus-events (pane-screen p1)) t)
        (cl-tmux::%cmd-cycle-window s #'cl-tmux::next-cyclic)
        (is (eq w1 (session-active-window s)) "next-window must activate w1")
        (let ((ready (cl-tmux/pty:select-fds (list rfd) 200000)))  ; 200 ms
          (is-true ready "the new window's pane must receive a focus report")
          (when ready
            (cffi:with-foreign-object (buf :uint8 8)
              (let ((n (cffi:foreign-funcall "read"
                                             :int rfd :pointer buf :unsigned-long 3
                                             :long)))
                (is (= 3 n) "focus-in must be 3 bytes (got ~D)" n)
                (is (= 27 (cffi:mem-aref buf :uint8 0)) "byte 0 must be ESC (27)")
                (is (= 73 (cffi:mem-aref buf :uint8 2))
                    "byte 2 must be #\\I (73) for focus gained")))))))))

(test cycle-window-delivers-focus-out-to-old-window-pane
  "Switching away from a window sends ESC[O (focus lost) to the window being left."
  (with-pipe-fds (rfd wfd)
    (with-fake-session (s :nwindows 2 :npanes 1)
      (let* ((w0 (first (session-windows s)))
             (p0 (window-active-pane w0)))
        ;; The FIRST (currently active) window's pane is the live, focus-aware PTY.
        (setf (pane-fd p0) wfd)
        (setf (cl-tmux/terminal/types:screen-focus-events (pane-screen p0)) t)
        (cl-tmux::%cmd-cycle-window s #'cl-tmux::next-cyclic)
        (let ((ready (cl-tmux/pty:select-fds (list rfd) 200000)))
          (is-true ready "the old window's pane must receive a focus-out report")
          (when ready
            (cffi:with-foreign-object (buf :uint8 8)
              (let ((n (cffi:foreign-funcall "read"
                                             :int rfd :pointer buf :unsigned-long 3
                                             :long)))
                (is (= 3 n) "focus-out must be 3 bytes (got ~D)" n)
                (is (= 27 (cffi:mem-aref buf :uint8 0)) "byte 0 must be ESC (27)")
                (is (= 79 (cffi:mem-aref buf :uint8 2))
                    "byte 2 must be #\\O (79) for focus lost")))))))))

(test cycle-window-with-focus-events-no-pty-no-error
  "Cycling windows runs the focus-transition path without error when panes have no
   PTY, and still changes the active window."
  (with-fake-session (s :nwindows 2 :npanes 1)
    (let ((w0 (first  (session-windows s)))
          (w1 (second (session-windows s))))
      (setf (cl-tmux/terminal/types:screen-focus-events
             (pane-screen (window-active-pane w0))) t)
      (setf (cl-tmux/terminal/types:screen-focus-events
             (pane-screen (window-active-pane w1))) t)
      (is (eq w0 (session-active-window s)))
      (finishes (cl-tmux::%cmd-cycle-window s #'cl-tmux::next-cyclic))
      (is (eq w1 (session-active-window s))
          "cycle-window must activate w1 even with focus events on and no PTY"))))

;;; ── list-keys overlay ───────────────────────────────────────────────────────

(test dispatch-list-keys-shows-overlay
  "C-b ? opens the key-binding help overlay."
  (with-fake-session (s)
    (let ((*overlay* nil))
      (cl-tmux::dispatch-command s :list-keys nil)
      (is (overlay-active-p) "list-keys should open the help overlay"))))

(test define-command-handlers-macro-is-defined
  "define-command-handlers is a defined macro."
  (is (macro-function 'cl-tmux::define-command-handlers)))

(test define-copy-mode-key-overrides-macro-is-defined
  "define-copy-mode-key-overrides is a defined macro."
  (is (macro-function 'cl-tmux::define-copy-mode-key-overrides)))

;;; ── select-pane-left/right/up/down dispatch ─────────────────────────────────
;;;
;;; These tests use the shared fixture macros from helpers.lisp instead of
;;; duplicating the setup inline.  with-two-pane-h-session and
;;; with-two-pane-v-session already encode the exact same geometry.

(test dispatch-select-pane-right-moves-active-pane
  ":select-pane-right moves the active pane to the right neighbour."
  (with-two-pane-h-session (sess win p0 p1)
    (is (eq p0 (window-active-pane win)) "p0 is active initially")
    (cl-tmux::dispatch-command sess :select-pane-right nil)
    (is (eq p1 (window-active-pane win))
        "active pane must be p1 after :select-pane-right"))))
(test dispatch-select-pane-left-moves-active-pane
  ":select-pane-left moves the active pane to the left neighbour."
  (with-two-pane-h-session (sess win p0 p1)
    ;; Start on p1, then go left.
    (window-select-pane win p1)
    (cl-tmux::dispatch-command sess :select-pane-left nil)
    (is (eq p0 (window-active-pane win))
        "active pane must be p0 after :select-pane-left"))))
(test dispatch-select-pane-right-noop-at-rightmost
  ":select-pane-right is a no-op when the active pane has no right neighbour."
  (with-two-pane-h-session (sess win p0 p1)
    ;; Make p1 (rightmost) active, then try to go further right.
    (is (eq p0 (window-active-pane win)) "p0 is active initially")
    (window-select-pane win p1)
    (cl-tmux::dispatch-command sess :select-pane-right nil)
    (is (eq p1 (window-active-pane win))
        "active pane must remain p1 when no right neighbour exists"))))
(test dispatch-select-pane-down-moves-active-pane
  ":select-pane-down moves the active pane to the pane below."
  (with-two-pane-v-session (sess win p0 p1)
    (is (eq p0 (window-active-pane win)) "p0 is active initially")
    (cl-tmux::dispatch-command sess :select-pane-down nil)
    (is (eq p1 (window-active-pane win))
        "active pane must be p1 after :select-pane-down"))))
(test dispatch-select-pane-up-moves-active-pane
  ":select-pane-up moves the active pane to the pane above."
  (with-two-pane-v-session (sess win p0 p1)
    ;; Start on p1 (bottom), then go up.
    (window-select-pane win p1)
    (cl-tmux::dispatch-command sess :select-pane-up nil)
    (is (eq p0 (window-active-pane win))
        "active pane must be p0 after :select-pane-up"))))
;;; ── zoom-toggle dispatch ────────────────────────────────────────────────────

(test dispatch-zoom-toggle-sets-zoom-flag
  ":zoom-toggle zooms the active pane in and marks window-zoom-p as T."
  (with-two-pane-h-session (sess win p0 p1)
    (is (and p0 p1) "both panes exist")
    (cl-tmux::dispatch-command sess :zoom-toggle nil)
    (is-true (cl-tmux/model:window-zoom-p win)
             "window-zoom-p must be T after :zoom-toggle dispatch")
    ;; Toggle back off.
    (cl-tmux::dispatch-command sess :zoom-toggle nil)
    (is-false (cl-tmux/model:window-zoom-p win)
              "window-zoom-p must be NIL after second :zoom-toggle dispatch"))))
;;; ── rename-session dispatch ─────────────────────────────────────────────────

(test dispatch-rename-session-opens-prompt
  ":rename-session opens a prompt seeded with the current session name, and
   its on-submit closure renames the session."
  (with-fake-session (s :nwindows 1)
    (let ((*prompt* nil))
      (cl-tmux::dispatch-command s :rename-session nil)
      (is (prompt-active-p) "rename-session must open a prompt")
      (is (string= "0" (prompt-buffer *prompt*))
          "prompt must be seeded with the current session name \"0\"")
      (funcall (prompt-on-submit *prompt*) "newsess")
      (is (string= "newsess" (session-name s))
          "on-submit must rename the session to the supplied name"))))

;;; ── %select-pane-in-direction ───────────────────────────────────────────────
;;;
;;; Use the shared fixture macros to avoid repeating pane/window/session setup.

(test select-pane-in-direction-right-selects-right-pane
  "%select-pane-in-direction :right from the left pane selects the right pane."
  (with-two-pane-h-session (sess win p0 p1)
    (is (eq p0 (window-active-pane win)) "p0 is active initially")
    (cl-tmux::%select-pane-in-direction sess :right)
    (is (eq p1 (window-active-pane win))
        "active pane must be p1 after %select-pane-in-direction :right"))))
(test select-pane-in-direction-left-selects-left-pane
  "%select-pane-in-direction :left from the right pane selects the left pane."
  (with-two-pane-h-session (sess win p0 p1)
    (window-select-pane win p1)
    (cl-tmux::%select-pane-in-direction sess :left)
    (is (eq p0 (window-active-pane win))
        "active pane must be p0 after %select-pane-in-direction :left"))))
(test select-pane-in-direction-noop-when-no-neighbor
  "%select-pane-in-direction is a no-op when the active pane has no neighbor
   in the requested direction."
  (with-two-pane-h-session (sess win p0 p1)
    (is-false (null p0) "fixture created")
    (window-select-pane win p1)          ; start at the rightmost pane

(test select-pane-in-direction-vertical-down-selects-lower-pane
  "%select-pane-in-direction :down from the top pane selects the bottom pane."
  (with-two-pane-v-session (sess win p0 p1)
    (is (eq p0 (window-active-pane win)) "p0 is active initially")
    (cl-tmux::%select-pane-in-direction sess :down)
    (is (eq p1 (window-active-pane win))
        "active pane must be p1 after %select-pane-in-direction :down"))))
;;; ── %apply-named-layout-to-session ──────────────────────────────────────────

(test apply-named-layout-even-horizontal-repositions-panes
  "%apply-named-layout-to-session :even-horizontal repositions two panes into
   equal-width columns and rebuilds the window tree."
  (with-two-pane-layout-session (sess win p0 p1)
    (with-loop-state
      (cl-tmux::%apply-named-layout-to-session sess :even-horizontal)
      ;; After even-horizontal: avail-w = 81 - 1 = 80, each-w = 40.
      ;; p0 should be at x=0, p1 at x=41, both width=40.
      (is (= 0  (pane-x p0)) "p0 x must be 0 after even-horizontal layout")
      (is (= 40 (pane-width p0)) "p0 width must be 40 after even-horizontal layout")
      (is (= 41 (pane-x p1)) "p1 x must be 41 after even-horizontal layout")
      (is (= 40 (pane-width p1)) "p1 width must be 40 after even-horizontal layout"))))

(test apply-named-layout-even-vertical-repositions-panes
  "%apply-named-layout-to-session :even-vertical repositions two panes into
   equal-height rows and rebuilds the window tree."
  ;; Use a vertical split (80×21) so the height arithmetic matches the assertions.
  (with-two-pane-v-session (sess win p0 p1)
    (cl-tmux::%apply-named-layout-to-session sess :even-vertical)
    ;; After even-vertical on an 80×21 window: avail-h = 21 - 1 = 20, each-h = 10.
    ;; p0 should be at y=0, p1 at y=11, both height=10.
    (is (= 0  (pane-y p0)) "p0 y must be 0 after even-vertical layout")
    (is (= 10 (pane-height p0)) "p0 height must be 10 after even-vertical layout")
    (is (= 11 (pane-y p1)) "p1 y must be 11 after even-vertical layout")
    (is (= 10 (pane-height p1)) "p1 height must be 10 after even-vertical layout"))))
(test apply-named-layout-noop-for-empty-session
  "%apply-named-layout-to-session with no active window is a no-op."
  (with-empty-session (sess)
    (with-loop-state
      (finishes (cl-tmux::%apply-named-layout-to-session sess :even-horizontal))
      "calling with no active window must not signal an error")))

;;; ── :list-windows dispatch ───────────────────────────────────────────────────

(test dispatch-list-windows-shows-overlay
  ":list-windows opens an overlay containing the window name and marks *dirty*."
  (with-fake-session (s :nwindows 1)
    (let ((*overlay* nil))
      (cl-tmux::dispatch-command s :list-windows nil)
      (is (overlay-active-p)
          ":list-windows must open the overlay")
      (is-true cl-tmux::*dirty*
               ":list-windows must mark *dirty*"))))

(test dispatch-list-windows-overlay-contains-window-name
  ":list-windows overlay text includes the active window name."
  (with-fake-session (s :nwindows 2)
    (let ((*overlay* nil))
      (cl-tmux::dispatch-command s :list-windows nil)
      (is (overlay-active-p) "overlay must be open")
      (let ((text (format nil "~{~A~%~}" (overlay-lines))))
        (is (search "1" text)
            "overlay must contain the first window entry")))))

;;; ── :list-sessions dispatch ──────────────────────────────────────────────────

(test dispatch-list-sessions-empty-registry-shows-overlay
  ":list-sessions with empty *server-sessions* falls back to the session-name
   single-line format and still opens an overlay."
  (with-fake-session (s :nwindows 1)
    (let ((*overlay* nil)
          (cl-tmux::*server-sessions* nil))
      (cl-tmux::dispatch-command s :list-sessions nil)
      (is (overlay-active-p)
          ":list-sessions must open an overlay even when *server-sessions* is nil")
      (let ((text (format nil "~{~A~%~}" (overlay-lines))))
        (is (search (session-name s) text)
            "fallback overlay must contain the session name")))))

(test dispatch-list-sessions-populated-registry-shows-all-sessions
  ":list-sessions with *server-sessions* populated lists every registered
   session and marks the current one with an asterisk."
  (with-fake-session (s :nwindows 1)
    (let ((name (session-name s)))
      (let ((*overlay* nil)
            (cl-tmux::*server-sessions* (list (cons name s))))
        (cl-tmux::dispatch-command s :list-sessions nil)
        (is (overlay-active-p)
            ":list-sessions must open an overlay")
        (let ((text (format nil "~{~A~%~}" (overlay-lines))))
          (is (search name text)
              "overlay must contain the session name")
          (is (search "*" text)
              "current session must be marked with an asterisk"))))))

;;; ── :display-panes dispatch ──────────────────────────────────────────────────

(test dispatch-display-panes-shows-pane-numbers
  ":display-panes with a 2-pane session arms the per-pane number display (via a
   timing overlay) and renders big-digit pane numbers."
  (with-two-pane-h-session (sess win p0 p1)
    (is (and win p0 p1) "session with 2 panes")
    (let ((*overlay* nil) (cl-tmux/prompt:*display-panes-active* nil))
      (cl-tmux::dispatch-command sess :display-panes nil)
      (is (overlay-active-p)
          ":display-panes opens the (timing) overlay")
      (is-true cl-tmux/prompt:*display-panes-active*
               ":display-panes arms the per-pane number display")
      (let ((frame (cl-tmux/renderer:render-session-to-string sess 24 81)))
        (is (find #\█ frame)
            ":display-panes must render big-digit pane numbers (got no █)")))))

(test dispatch-display-panes-marks-dirty
  ":display-panes sets *dirty* to T."
  (with-fake-session (sess :nwindows 1 :npanes 1)
    (let ((*overlay* nil))
      (cl-tmux::dispatch-command sess :display-panes nil)
      (is-true cl-tmux::*dirty*
               ":display-panes must mark *dirty*"))))

